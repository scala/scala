/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Scala Center, Lightbend dba Akka, and Mark Harrah
 *
 * Scala (https://www.scala-lang.org)
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package xsbt

import xsbti.{AnalysisCallback, AnalysisCallback2, Logger, Problem, Reporter, VirtualFile}
import xsbti.compile._

import scala.reflect.io.AbstractFile
import scala.tools.nsc.{CompilerCommand, Settings}
import Log.debug

import java.io.File

/**
 * This is the entry point for the compiler bridge (implementation of CompilerInterface)
 */
final class CompilerBridge extends xsbti.compile.CompilerInterface2 {
  override def run(
      sources: Array[VirtualFile],
      changes: DependencyChanges,
      options: Array[String],
      output: Output,
      callback: AnalysisCallback,
      delegate: Reporter,
      progress: CompileProgress,
      log: Logger
  ): Unit = {
    val cached = new CachedCompiler0(options, output, new WeakLog(log, delegate))
    try {
      cached.run(sources.toList, changes, callback, log, delegate, progress)
    } finally {
      cached.close()
    }
  }
}

class InterfaceCompileFailed(
    val arguments: Array[String],
    val problems: Array[Problem],
    override val toString: String
) extends xsbti.CompileFailed

class InterfaceCompileCancelled(val arguments: Array[String], override val toString: String)
    extends xsbti.CompileCancelled

private final class WeakLog(private[this] var log: Logger, private[this] var delegate: Reporter) {
  def apply(message: String): Unit = {
    assert(log ne null, "Stale reference to logger")
    log.error(Message(message))
  }
  def logger: Logger = log
  def reporter: Reporter = delegate
  def clear(): Unit = {
    log = null
    delegate = null
  }
}

private final class CachedCompiler0(
    args: Array[String],
    output: Output,
    initialLog: WeakLog
) extends CachedCompilerCompat
    with java.io.Closeable {

  /////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////// INITIALIZATION CODE ////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////

  val settings = new Settings(s => initialLog(s))
  output match {
    case multi: MultipleOutput =>
      for (out <- multi.getOutputGroups)
        settings.outputDirs
          .add(
            out.getSourceDirectoryAsPath.toAbsolutePath.toString,
            out.getOutputDirectoryAsPath.toAbsolutePath.toString
          )
    case single: SingleOutput =>
      val outputFilepath = single.getOutputDirectoryAsPath.toAbsolutePath
      settings.outputDirs.setSingleOutput(outputFilepath.toString)
    case x => throw new MatchError(x)
  }

  val command = new CompilerCommand(args.toList, settings)
  private[this] val dreporter = DelegatingReporter(settings, initialLog.reporter)
  try {
    if (!noErrors(dreporter)) {
      dreporter.printSummary()
      handleErrors(dreporter, initialLog.logger)
    }
  } finally initialLog.clear()

  /** Instance of the underlying Zinc compiler. */
  val compiler: ZincCompiler = newCompiler(command.settings, dreporter, output)

  /////////////////////////////////////////////////////////////////////////////////////////////////

  def close(): Unit = {
    compiler match {
      case c: java.io.Closeable => c.close()
      case _                    =>
    }
  }

  def noErrors(dreporter: DelegatingReporter) = !dreporter.hasErrors && command.ok

  def commandArguments(sources: Array[File]): Array[String] =
    (command.settings.recreateArgs ++ sources.map(_.getAbsolutePath)).toArray[String]

  import scala.tools.nsc.Properties.versionString
  def infoOnCachedCompiler(compilerId: String): String =
    s"[zinc] Running cached compiler $compilerId for Scala compiler $versionString"

  def run(
      sources: List[VirtualFile],
      changes: DependencyChanges,
      callback: AnalysisCallback,
      log: Logger,
      delegate: Reporter,
      progress: CompileProgress
  ): Unit = synchronized {
    debug(log, infoOnCachedCompiler(hashCode().toLong.toHexString))
    val dreporter = DelegatingReporter(settings, delegate)
    try {
      run(sources.sortBy(_.id).map(AbstractZincFile(_)), callback, log, dreporter, progress)
    } finally {
      dreporter.dropDelegate()
    }
  }

  private def prettyPrintCompilationArguments(args: Array[String]) =
    args.mkString("[zinc] The Scala compiler is invoked with:\n\t", "\n\t", "")

  private val StopInfoError = "Compiler option supplied that disabled Zinc compilation."

  private[this] def run(
      sources: List[AbstractFile],
      callback: AnalysisCallback,
      log: Logger,
      underlyingReporter: DelegatingReporter,
      compileProgress: CompileProgress
  ): Unit = {

    lazy val callback2 =
      try callback.asInstanceOf[AnalysisCallback2]
      catch { case _: NoClassDefFoundError => null}

    def callbackProblem(p: Problem) =
      if (callback2 != null) callback2.problem2(p.category, p.position, p.message, p.severity, true, p.rendered, p.diagnosticCode, p.diagnosticRelatedInformation, p.actions)
      else callback.problem(p.category, p.position, p.message, p.severity, true)

    compiler.set(callback, underlyingReporter)
    if (command.shouldStopWithInfo) {
      underlyingReporter.echo(command.getInfoMessage(compiler))
      throw new InterfaceCompileFailed(args, Array(), StopInfoError)
    }

    if (noErrors(underlyingReporter)) {
      debug(log, prettyPrintCompilationArguments(args))
      val run = new compiler.ZincRun(compileProgress)

      run.compileFiles(sources)
      underlyingReporter.problems.foreach(callbackProblem)
    }

    underlyingReporter.printSummary()
    if (!noErrors(underlyingReporter))
      handleErrors(underlyingReporter, log)

    // the case where we cancelled compilation _after_ some compilation errors got reported
    // will be handled by line above so errors still will be reported properly just potentially not
    // all of them (because we cancelled the compilation)
    if (underlyingReporter.cancelled)
      handleCompilationCancellation(underlyingReporter, log)
  }

  def handleErrors(dreporter: DelegatingReporter, log: Logger): Nothing = {
    debug(log, "Compilation failed (CompilerInterface)")
    throw new InterfaceCompileFailed(args, dreporter.problems, "Compilation failed")
  }

  def handleCompilationCancellation(dreporter: DelegatingReporter, log: Logger): Nothing = {
    assert(dreporter.cancelled, "We should get here only if when compilation got cancelled")
    debug(log, "Compilation cancelled (CompilerInterface)")
    throw new InterfaceCompileCancelled(args, "Compilation has been cancelled")
  }
}
