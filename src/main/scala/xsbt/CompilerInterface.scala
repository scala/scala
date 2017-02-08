/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

import xsbti.{ AnalysisCallback, Logger, Problem, Reporter, Severity }
import xsbti.compile._
import scala.tools.nsc.{ backend, io, reporters, symtab, util, Phase, Global, Settings, SubComponent }
import scala.tools.nsc.interactive.RangePositions
import backend.JavaPlatform
import scala.tools.util.PathResolver
import symtab.SymbolLoaders
import util.{ ClassPath, DirectoryClassPath, MergedClassPath, JavaClassPath }
import ClassPath.{ ClassPathContext, JavaContext }
import io.AbstractFile
import scala.annotation.tailrec
import scala.collection.mutable
import Log.debug
import java.io.File

final class CompilerInterface {
  def newCompiler(options: Array[String], output: Output, initialLog: Logger, initialDelegate: Reporter, resident: Boolean): CachedCompiler =
    new CachedCompiler0(options, output, new WeakLog(initialLog, initialDelegate), resident)

  def run(sources: Array[File], changes: DependencyChanges, callback: AnalysisCallback, log: Logger, delegate: Reporter, progress: CompileProgress, cached: CachedCompiler): Unit =
    cached.run(sources, changes, callback, log, delegate, progress)
}
// for compatibility with Scala versions without Global.registerTopLevelSym (2.8.1 and earlier)
sealed trait GlobalCompat { self: Global =>
  def registerTopLevelSym(sym: Symbol): Unit
  sealed trait RunCompat {
    def informUnitStarting(phase: Phase, unit: CompilationUnit): Unit = ()
  }
}
sealed abstract class CallbackGlobal(settings: Settings, reporter: reporters.Reporter, output: Output) extends Global(settings, reporter) with GlobalCompat {
  def callback: AnalysisCallback
  def findClass(name: String): Option[(AbstractFile, Boolean)]
  lazy val outputDirs: Iterable[File] = {
    output match {
      case single: SingleOutput  => List(single.outputDirectory)
      case multi: MultipleOutput => multi.outputGroups.toStream map (_.outputDirectory)
    }
  }
  // sbtDependency is exposed to `localToNonLocalClass` for sanity checking
  // the lookup performed by the `localToNonLocalClass` can be done only if
  // we're running at earlier phase, e.g. an sbtDependency phase
  private[xsbt] val sbtDependency: SubComponent
  /*
   * A map from local classes to non-local class that contains it.
   *
   * This map is used by both Dependency and Analyzer phase so it has to be
   * exposed here. The Analyzer phase uses the cached lookups performed by
   * the Dependency phase. By the time Analyzer phase is run (close to backend
   * phases), original owner chains are lost so Analyzer phase relies on
   * information saved before.
   *
   * The LocalToNonLocalClass duplicates the tracking that Scala compiler does
   * internally for backed purposes (generation of EnclosingClass attributes) but
   * that internal mapping doesn't have a stable interface we could rely on.
   */
  private[xsbt] val localToNonLocalClass = new LocalToNonLocalClass[this.type](this)
}
class InterfaceCompileFailed(val arguments: Array[String], val problems: Array[Problem], override val toString: String) extends xsbti.CompileFailed

class InterfaceCompileCancelled(val arguments: Array[String], override val toString: String) extends xsbti.CompileCancelled

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

private final class CachedCompiler0(args: Array[String], output: Output, initialLog: WeakLog, resident: Boolean) extends CachedCompiler {
  val settings = new Settings(s => initialLog(s))
  output match {
    case multi: MultipleOutput =>
      for (out <- multi.outputGroups)
        settings.outputDirs.add(out.sourceDirectory.getAbsolutePath, out.outputDirectory.getAbsolutePath)
    case single: SingleOutput =>
      settings.outputDirs.setSingleOutput(single.outputDirectory.getAbsolutePath)
  }

  val command = Command(args.toList, settings)
  private[this] val dreporter = DelegatingReporter(settings, initialLog.reporter)
  try {
    if (!noErrors(dreporter)) {
      dreporter.printSummary()
      handleErrors(dreporter, initialLog.logger)
    }
  } finally
    initialLog.clear()

  def noErrors(dreporter: DelegatingReporter) = !dreporter.hasErrors && command.ok

  def commandArguments(sources: Array[File]): Array[String] =
    (command.settings.recreateArgs ++ sources.map(_.getAbsolutePath)).toArray[String]

  def run(sources: Array[File], changes: DependencyChanges, callback: AnalysisCallback, log: Logger, delegate: Reporter, progress: CompileProgress): Unit = synchronized {
    debug(log, "Running cached compiler " + hashCode.toLong.toHexString + ", interfacing (CompilerInterface) with Scala compiler " + scala.tools.nsc.Properties.versionString)
    val dreporter = DelegatingReporter(settings, delegate)
    try { run(sources.toList, changes, callback, log, dreporter, progress) }
    finally { dreporter.dropDelegate() }
  }
  private[this] def run(sources: List[File], changes: DependencyChanges, callback: AnalysisCallback, log: Logger, dreporter: DelegatingReporter, compileProgress: CompileProgress): Unit = {
    if (command.shouldStopWithInfo) {
      dreporter.info(null, command.getInfoMessage(compiler), true)
      throw new InterfaceCompileFailed(args, Array(), "Compiler option supplied that disabled actual compilation.")
    }
    if (noErrors(dreporter)) {
      debug(log, args.mkString("Calling Scala compiler with arguments  (CompilerInterface):\n\t", "\n\t", ""))
      compiler.set(callback, dreporter)
      val run = new compiler.Run with compiler.RunCompat {
        override def informUnitStarting(phase: Phase, unit: compiler.CompilationUnit): Unit = {
          compileProgress.startUnit(phase.name, unit.source.path)
        }
        override def progress(current: Int, total: Int): Unit = {
          if (!compileProgress.advance(current, total))
            cancel
        }
      }
      val sortedSourceFiles = sources.map(_.getAbsolutePath).sortWith(_ < _)
      run compile sortedSourceFiles
      processUnreportedWarnings(run)
      dreporter.problems foreach { p => callback.problem(p.category, p.position, p.message, p.severity, true) }
    }
    dreporter.printSummary()
    if (!noErrors(dreporter)) handleErrors(dreporter, log)
    // the case where we cancelled compilation _after_ some compilation errors got reported
    // will be handled by line above so errors still will be reported properly just potentially not
    // all of them (because we cancelled the compilation)
    if (dreporter.cancelled) handleCompilationCancellation(dreporter, log)
  }
  def handleErrors(dreporter: DelegatingReporter, log: Logger): Nothing =
    {
      debug(log, "Compilation failed (CompilerInterface)")
      throw new InterfaceCompileFailed(args, dreporter.problems, "Compilation failed")
    }
  def handleCompilationCancellation(dreporter: DelegatingReporter, log: Logger): Nothing = {
    assert(dreporter.cancelled, "We should get here only if when compilation got cancelled")
    debug(log, "Compilation cancelled (CompilerInterface)")
    throw new InterfaceCompileCancelled(args, "Compilation has been cancelled")
  }
  def processUnreportedWarnings(run: compiler.Run): Unit = {
    // allConditionalWarnings and the ConditionalWarning class are only in 2.10+
    final class CondWarnCompat(val what: String, val warnings: mutable.ListBuffer[(compiler.Position, String)])
    implicit def compat(run: AnyRef): Compat = new Compat
    final class Compat { def allConditionalWarnings = List[CondWarnCompat]() }

    val warnings = run.allConditionalWarnings
    if (warnings.nonEmpty)
      compiler.logUnreportedWarnings(warnings.map(cw => ("" /*cw.what*/ , cw.warnings.toList)))
  }

  val compiler: Compiler = new Compiler()
  class Compiler extends CallbackGlobal(command.settings, dreporter, output) {
    object dummy // temporary fix for #4426
    object sbtAnalyzer extends {
      val global: Compiler.this.type = Compiler.this
      val phaseName = Analyzer.name
      val runsAfter = List("jvm")
      override val runsBefore = List("terminal")
      val runsRightAfter = None
    } with SubComponent {
      val analyzer = new Analyzer(global)
      def newPhase(prev: Phase) = analyzer.newPhase(prev)
      def name = phaseName
    }

    /** Phase that extracts dependency information */
    object sbtDependency extends {
      val global: Compiler.this.type = Compiler.this
      val phaseName = Dependency.name
      val runsAfter = List(API.name)
      override val runsBefore = List("refchecks")
      // keep API and dependency close to each other
      // we might want to merge them in the future and even if don't
      // do that then it makes sense to run those phases next to each other
      val runsRightAfter = Some(API.name)
    } with SubComponent {
      val dependency = new Dependency(global)
      def newPhase(prev: Phase) = dependency.newPhase(prev)
      def name = phaseName
    }

    /**
     * This phase walks trees and constructs a representation of the public API, which is used for incremental recompilation.
     *
     * We extract the api after picklers, since that way we see the same symbol information/structure
     * irrespective of whether we were typechecking from source / unpickling previously compiled classes.
     */
    object apiExtractor extends {
      val global: Compiler.this.type = Compiler.this
      val phaseName = API.name
      val runsAfter = List("typer")
      override val runsBefore = List("erasure")
      // allow apiExtractor's phase to be overridden using the sbt.api.phase property
      // (in case someone would like the old timing, which was right after typer)
      // TODO: consider migrating to simply specifying "pickler" for `runsAfter` and "uncurry" for `runsBefore`
      val runsRightAfter = Option(System.getProperty("sbt.api.phase")) orElse Some("pickler")
    } with SubComponent {
      val api = new API(global)
      def newPhase(prev: Phase) = api.newPhase(prev)
      def name = phaseName
    }

    override lazy val phaseDescriptors =
      {
        phasesSet += sbtAnalyzer
        if (callback.enabled()) {
          phasesSet += sbtDependency
          phasesSet += apiExtractor
        }
        superComputePhaseDescriptors
      }
    // Required because computePhaseDescriptors is private in 2.8 (changed to protected sometime later).
    private[this] def superComputePhaseDescriptors() = superCall("computePhaseDescriptors").asInstanceOf[List[SubComponent]]
    private[this] def superDropRun(): Unit =
      try { superCall("dropRun"); () } catch { case e: NoSuchMethodException => () } // dropRun not in 2.8.1
    private[this] def superCall(methodName: String): AnyRef =
      {
        val meth = classOf[Global].getDeclaredMethod(methodName)
        meth.setAccessible(true)
        meth.invoke(this)
      }
    def logUnreportedWarnings(seq: Seq[(String, List[(Position, String)])]): Unit = // Scala 2.10.x and later
      {
        val drep = reporter.asInstanceOf[DelegatingReporter]
        for ((what, warnings) <- seq; (pos, msg) <- warnings) yield callback.problem(what, drep.convert(pos), msg, Severity.Warn, false)
        ()
      }

    final def set(callback: AnalysisCallback, dreporter: DelegatingReporter): Unit = {
      this.callback0 = callback
      reporter = dreporter
    }
    def clear(): Unit = {
      callback0 = null
      superDropRun()
      reporter = null
    }

    def findClass(name: String): Option[(AbstractFile, Boolean)] =
      getOutputClass(name).map(f => (f, true)) orElse findOnClassPath(name).map(f => (f, false))

    def getOutputClass(name: String): Option[AbstractFile] =
      {
        // This could be improved if a hint where to look is given.
        val className = name.replace('.', '/') + ".class"
        outputDirs map (new File(_, className)) find (_.exists) map (AbstractFile.getFile(_))
      }

    def findOnClassPath(name: String): Option[AbstractFile] =
      classPath.findClass(name).flatMap(_.binary.asInstanceOf[Option[AbstractFile]])

    private[this] var callback0: AnalysisCallback = null
    def callback: AnalysisCallback = callback0
  }
}
