/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package xsbt

import java.io.File

import xsbti.compile._
import xsbti.{AnalysisCallback, Logger, Problem, Reporter}

import scala.collection.mutable
import scala.tools.nsc.Settings
import scala.tools.nsc.incremental.Log.debug
import scala.tools.nsc.incremental._

final class CompilerInterface {
  def newCompiler(
      options: Array[String],
      output: Output,
      initialLog: Logger,
      initialDelegate: Reporter
  ): CachedCompiler = {
    val bridgeLoader = this.getClass.getClassLoader
    val fixedLoader = CompilerClassLoader.fixBridgeLoader(bridgeLoader)
    val ccClass = fixedLoader.loadClass("xsbt.CachedCompiler0")
    ccClass.getConstructors.apply(0).newInstance(options, output, initialLog, initialDelegate).asInstanceOf[CachedCompiler]
  }

  def run(
      sources: Array[File],
      changes: DependencyChanges,
      callback: AnalysisCallback,
      log: Logger,
      delegate: Reporter,
      progress: CompileProgress,
      cached: CachedCompiler
  ): Unit =
    cached.run(sources, changes, callback, log, delegate, progress)
}

class InterfaceCompileFailed(
    val arguments: Array[String],
    val problems: Array[Problem],
    override val toString: String
) extends xsbti.CompileFailed

class InterfaceCompileCancelled(val arguments: Array[String], override val toString: String)
    extends xsbti.CompileCancelled

final class CachedCompiler0(args: Array[String], output: Output, log: Logger, delegate: Reporter)
  extends CachedCompiler
    with CachedCompilerCompat
    with java.io.Closeable {

  /////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////// INITIALIZATION CODE ////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////

  private val initialLog = new WeakLog(log, delegate)

  val settings = new Settings(s => initialLog(s))
  output match {
    case multi: MultipleOutput =>
      for (out <- multi.getOutputGroups)
        settings.outputDirs
          .add(out.getSourceDirectory.getAbsolutePath, out.getOutputDirectory.getAbsolutePath)
    case single: SingleOutput =>
      val outputFilepath = single.getOutputDirectory.getAbsolutePath
      settings.outputDirs.setSingleOutput(outputFilepath)
  }

  val command = Command(args.toList, settings)
  private[this] val dreporter = ZincDelegatingReporter(settings, initialLog.reporter)
  try {
    if (!noErrors(dreporter)) {
      dreporter.printSummary()
      handleErrors(dreporter, initialLog.logger)
    }
  } finally initialLog.clear()

  /** Instance of the underlying Zinc compiler. */
  val compiler: ZincGlobal = newCompiler(command.settings, dreporter, output)

  /////////////////////////////////////////////////////////////////////////////////////////////////

  def close(): Unit = {
    compiler match {
      case c: java.io.Closeable => c.close()
      case _                    =>
    }
  }

  def noErrors(dreporter: ZincDelegatingReporter) = !dreporter.hasErrors && command.ok

  def commandArguments(sources: Array[File]): Array[String] =
    (command.settings.recreateArgs ++ sources.map(_.getAbsolutePath)).toArray[String]

  import scala.tools.nsc.Properties.versionString
  def infoOnCachedCompiler(compilerId: String): String =
    s"[zinc] Running cached compiler $compilerId for Scala compiler $versionString"

  def run(
      sources: Array[File],
      changes: DependencyChanges,
      callback: AnalysisCallback,
      log: Logger,
      delegate: Reporter,
      progress: CompileProgress
  ): Unit = synchronized {
    debug(log, infoOnCachedCompiler(hashCode().toLong.toHexString))
    val dreporter = ZincDelegatingReporter(settings, delegate)
    try {
      run(sources.toList, changes, callback, log, dreporter, progress)
    } finally {
      dreporter.dropDelegate()
    }
  }

  private def prettyPrintCompilationArguments(args: Array[String]) =
    args.mkString("[zinc] The Scala compiler is invoked with:\n\t", "\n\t", "")
  private val StopInfoError = "Compiler option supplied that disabled Zinc compilation."
  private[this] def run(
      sources: List[File],
      changes: DependencyChanges,
      callback: AnalysisCallback,
      log: Logger,
      underlyingReporter: ZincDelegatingReporter,
      compileProgress: CompileProgress
  ): Unit = {

    if (command.shouldStopWithInfo) {
      underlyingReporter.info(null, command.getInfoMessage(compiler), true)
      throw new InterfaceCompileFailed(args, Array(), StopInfoError)
    }

    if (noErrors(underlyingReporter)) {
      debug(log, prettyPrintCompilationArguments(args))
      compiler.set(callback, underlyingReporter)
      val run = new compiler.ZincRun(compileProgress)
      val sortedSourceFiles = sources.map(_.getAbsolutePath).sortWith(_ < _)
      run.compile(sortedSourceFiles)
      processUnreportedWarnings(run)
      underlyingReporter.problems.foreach(
        p => callback.problem(p.category, p.position, p.message, p.severity, true)
      )
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

  def handleErrors(dreporter: ZincDelegatingReporter, log: Logger): Nothing = {
    debug(log, "Compilation failed (CompilerInterface)")
    throw new InterfaceCompileFailed(args, dreporter.problems, "Compilation failed")
  }

  def handleCompilationCancellation(dreporter: ZincDelegatingReporter, log: Logger): Nothing = {
    assert(dreporter.cancelled, "We should get here only if when compilation got cancelled")
    debug(log, "Compilation cancelled (CompilerInterface)")
    throw new InterfaceCompileCancelled(args, "Compilation has been cancelled")
  }

  def processUnreportedWarnings(run: compiler.Run): Unit = {
    // allConditionalWarnings and the ConditionalWarning class are only in 2.10+
    final class CondWarnCompat(
        val what: String,
        val warnings: mutable.ListBuffer[(compiler.Position, String)]
    )
    implicit def compat(run: AnyRef): Compat = new Compat
    final class Compat { def allConditionalWarnings = List[CondWarnCompat]() }

    val warnings = run.allConditionalWarnings
    if (warnings.nonEmpty)
      compiler.logUnreportedWarnings(warnings.map(cw => ("" /*cw.what*/, cw.warnings.toList)))
  }
}

final class WeakLog(private[this] var log: Logger, private[this] var delegate: Reporter) {
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
