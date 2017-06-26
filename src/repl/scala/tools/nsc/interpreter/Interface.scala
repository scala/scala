// Copyright 2002-2017 LAMP/EPFL and Lightbend, Inc.

package scala.tools.nsc.interpreter
import java.io.PrintWriter
import java.net.URL

import scala.reflect.ClassTag
import scala.reflect.internal.util.{AbstractFileClassLoader, SourceFile}
import scala.reflect.internal.util.Position
import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.Results.Result
import scala.tools.nsc.reporters.Reporter


/** The subset of the Repl used by sbt.
  *
  */
trait ReplCore {
  /**
    * Interpret one line of input. All feedback, including parse errors
    * and evaluation results, are printed via the supplied compiler's
    *  reporter. Values defined are available for future interpreted strings.
    *
    * The return value is whether the line was interpreter successfully,
    *  e.g. that there were no parse errors.
    */
  def interpret(line: String): Result

  /** The reporter will not print results during execution of `body`.
    */
  def beQuietDuring(body: => Unit): Unit

  /** Bind a specified name to a specified value.  The name may
    * later be used by expressions passed to interpret.
    *
    *
    * @param name      the variable name to bind
    * @param boundType the type of the variable, as a string
    * @param value     the object value to bind to it
    * @return an indication of whether the binding succeeded
    */
  def bind(name: String, boundType: String, value: Any, modifiers: List[String] = Nil): Result

  /** Bind a specified `name` to a specified `value`.
    * The type is derived from the run-time class of the value.
    */
  def bindValue(name: String, value: Any): Result = bind(name, value.asInstanceOf[AnyRef].getClass.getName, value)

  /** Set the current Java "context" class loader to this interpreter's class loader
    *
    */
  def setContextClassLoader(): Unit
}

/**
  * Interface to the repl for use by the frontend (shell, the UI).
  *
  * The interface should not depend on symbols and types (the compiler's internal state).
  * At most, expose untyped trees and positions in addition to standard Java types.
  * This decoupling would allow running the shell in a separate thread, or even
  * in a separate process from the compiler. It should also be possible to write
  * a new REPL frontend using this interface, and be compatible across minor compiler releases.
  *
  * (The first iteration of this interface is only uses Positions and
  *  standard JVM types, but we could loosen that.)
  *
  */
trait Repl extends ReplCore {
  val settings: Settings
  type Setting = settings.Setting
  type SettingSet = scala.collection.Set[Setting]

  def reporter: ReplReporter

  type Request <: ReplRequest

  // Apply a temporary label for compilation (for example, script name)
  def withLabel[A](temp: String)(body: => A): A

  def visibleSettings: SettingSet

  def userSetSettings: SettingSet

  def updateSettings(arguments: List[String]): Boolean

  def initializeComplete: Boolean

  // initializes the compiler, returning false if something went wrong
  def initializeCompiler(): Boolean

  def classPathString: String

  def quietRun(code: String): Result

  def setExecutionWrapper(code: String): Unit
  def clearExecutionWrapper(): Unit

  /**
    * Adds all specified jars to the compile and runtime classpaths.
    *
    * @note Currently only supports jars, not directories.
    * @param urls The list of items to add to the compile and runtime classpaths.
    */
  def addUrlsToClassPath(urls: URL*): Unit

  def classLoader: AbstractFileClassLoader

  def originalPath(name: String): String

  def translatePath(path: String): Option[String]

  def translateEnclosingClass(n: String): Option[String]

  def isPackaged(line: String): Boolean

  /** Compile an nsc SourceFile.  Returns true if there are
    * no compilation errors, or false otherwise.
    */
  def compileSources(sources: SourceFile*): Boolean

  /** Compile a string.  Returns true if there are no
    * compilation errors, or false otherwise.
    */
  def compileString(code: String): Boolean

  def interpret(line: String, synthetic: Boolean): Result

  final def beQuietDuring(body: => Unit): Unit = reporter.withoutPrintingResults(body)



  def namedParam[T: reflect.runtime.universe.TypeTag : ClassTag](name: String, value: T): NamedParam

  def quietBind(p: NamedParam): Result

  def bind(p: NamedParam): Result

  def presentationCompile(cursor: Int, buf: String): Either[Result, PresentationCompilationResult]

  /** Reset this interpreter, forgetting all user-specified requests. */
  def reset(): Unit

  /** This instance is no longer needed, so release any resources
    * it is using.  The reporter's output gets flushed.
    */
  def close(): Unit

  val power: Power[StdReplVals]


  def requestDefining(name: String): Option[ReplRequest]

  /** Returns the name of the most recent interpreter result.
    * Mostly this exists so you can conveniently invoke methods on
    * the previous result.
    */
  def mostRecentVar: String

  def definedTypes: List[String]

  // Terms with user-given names (i.e. not res0 and not synthetic)
  def namedDefinedTerms: List[String]

  def lastWarnings: List[(Position, String)]

  def importsCommandInternal(tokens: List[String]): List[String]

  def implicitsCommandInternal(line: String): (List[String], String)

  def kindCommandInternal(expr: String, verbose: Boolean): String

  /** TODO -
    * -n normalize
    * -l label with case class parameter names
    * -c complete - leave nothing out
    */
  def typeCommandInternal(expr: String, verbose: Boolean): (String, String)

  // Used in a test case.
  def showDirectory: String

  // Used in a test case.
  def valueOfTerm(id: String): Option[Any]
}

/**
  * The interface used to expose the repl as a Java Script Engine
  */
trait ScriptedRepl extends Repl {
  def compile(code: String): Boolean
  def compile(line: String, synthetic: Boolean): Either[Result, Request]

  def call(name: String, args: Any*): Either[Throwable, AnyRef]

  // TODO: should we move more of the wrapping from shell/Scripted to interpreter/Scripted,
  // to avoid exposing this low-level stuff to the frontend (shell)?
  def evalName: String
  def evalPath: String

  def recordRequest(req: Request): Unit
  def addBackReferences(req: Request): Either[String, Request]
}

trait ReplReporter extends Reporter {
  def out: PrintWriter

  /**
    * Print message (info/warning/error).
    * By default, messages beyond a certain length are truncated (see `withoutTruncating`),
    * and internal repl wrapping is removed (see `withoutUnwrapping` and `unmangleInterpreterOutput`).
    * To suppress all output, use `suppressOutput`
    */
  def printMessage(msg: String): Unit

  /** Don't print any errors/messages/echos during the execution of `body`.
    */
  def suppressOutput[T](body: => T): T

  /** Suppress truncation during the executing of `body`.
    */
  def withoutTruncating[T](body: => T): T

  /** Do not remove interpreter wrappers ($iw etc) from all output during the execution of `body`.
    */
  def withoutUnwrapping(body: => Unit): Unit


  /** Print result (Right --> success, Left --> error)
    */
  def printResult(result: Either[String, String]): Unit

  /** Don't print result lines.
    */
  def withoutPrintingResults[T](body: => T): T

  /** Whether we're printing results (should only be used from the shell).
    */
  def printResults: Boolean

  /** Toggle whether to print results (should only be used from the shell).
    */
  def togglePrintResults(): Unit


  //// println debugging ftw
  def isDebug: Boolean
  def debug(msg: => String): Unit = if (isDebug) echo(msg)

  def isTrace: Boolean
  def trace(msg: => String): Unit = if (isTrace) echo(msg)

  //// Internal signalling from repl to shell

  /** Currently executing request (used to determine position of error in terms of user-submitted code)
    *
    * TODO: should no longer be needed if we do wrapping after type checking
    */
  def currentRequest: ReplRequest

  /** Set currently executing request.
    */
  def currentRequest_= (req: ReplRequest): Unit
}

trait ReplRequest {
  def line: String

  def eval: Either[Throwable, AnyRef]
}

/**
  *
  * Created by scala.tools.nsc.interpreter.Repl#presentationCompile
  */
trait PresentationCompilationResult {
  /** The start and end of this range position correspond to the start and end of user input inside `buf`
    * Start may not be zero if there's leading whitespace/comments, which are not represented as trees.
    * Similarly for the end position.
    */
  protected def inputRange: Position

  /** The 0-based offset of the cursor into `buf` */
  protected def cursor: Int

  /** The user's input */
  protected def buf: String

  def cleanup(): Unit

  def print: String

  def typeAt(start: Int, end: Int): String

  def candidates(tabCount: Int): (Int, List[String])
}
