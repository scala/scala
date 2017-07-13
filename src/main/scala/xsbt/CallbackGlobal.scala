/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

import xsbti.{ AnalysisCallback, Severity }
import xsbti.compile._

import scala.tools.nsc._
import io.AbstractFile
import java.io.File

/** Defines the interface of the incremental compiler hiding implementation details. */
sealed abstract class CallbackGlobal(settings: Settings,
                                     reporter: reporters.Reporter,
                                     output: Output)
    extends Global(settings, reporter) {

  def callback: AnalysisCallback
  def findClass(name: String): Option[(AbstractFile, Boolean)]

  lazy val outputDirs: Iterable[File] = {
    output match {
      case single: SingleOutput => List(single.getOutputDirectory)
      // Use Stream instead of List because Analyzer maps intensively over the directories
      case multi: MultipleOutput => multi.getOutputGroups.toStream map (_.getOutputDirectory)
    }
  }

  /**
   * Defines the sbt phase in which the dependency analysis is performed.
   * The reason why this is exposed in the callback global is because it's used
   * in [[xsbt.LocalToNonLocalClass]] to make sure the we don't resolve local
   * classes before we reach this phase.
   */
  private[xsbt] val sbtDependency: SubComponent

  /**
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

/** Defines the implementation of Zinc with all its corresponding phases. */
sealed class ZincCompiler(settings: Settings, dreporter: DelegatingReporter, output: Output)
    extends CallbackGlobal(settings, dreporter, output)
    with ZincGlobalCompat {

  final class ZincRun(compileProgress: CompileProgress) extends Run {
    override def informUnitStarting(phase: Phase, unit: CompilationUnit): Unit =
      compileProgress.startUnit(phase.name, unit.source.path)
    override def progress(current: Int, total: Int): Unit =
      if (!compileProgress.advance(current, total)) cancel else ()
  }

  object dummy // temporary fix for #4426

  /** Phase that analyzes the generated class files and maps them to sources. */
  object sbtAnalyzer extends {
    val global: ZincCompiler.this.type = ZincCompiler.this
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
    val global: ZincCompiler.this.type = ZincCompiler.this
    val phaseName = Dependency.name
    val runsAfter = List(API.name)
    override val runsBefore = List("refchecks")
    // Keep API and dependency close to each other -- we may want to merge them in the future.
    override val runsRightAfter = Some(API.name)
  } with SubComponent {
    val dependency = new Dependency(global)
    def newPhase(prev: Phase) = dependency.newPhase(prev)
    def name = phaseName
  }

  /**
   * Phase that walks the trees and constructs a representation of the public API.
   *
   * @note It extracts the API information after picklers to see the same symbol information
   *       irrespective of whether we typecheck from source or unpickle previously compiled classes.
   */
  object apiExtractor extends {
    val global: ZincCompiler.this.type = ZincCompiler.this
    val phaseName = API.name
    val runsAfter = List("typer")
    override val runsBefore = List("erasure")
    // TODO: Consider migrating to "uncurry" for `runsBefore`.
    // TODO: Consider removing the system property to modify which phase is used for API extraction.
    val runsRightAfter = Option(System.getProperty("sbt.api.phase")) orElse Some("pickler")
  } with SubComponent {
    val api = new API(global)
    def newPhase(prev: Phase) = api.newPhase(prev)
    def name = phaseName
  }

  override lazy val phaseDescriptors = {
    phasesSet += sbtAnalyzer
    if (callback.enabled()) {
      phasesSet += sbtDependency
      phasesSet += apiExtractor
    }
    this.computePhaseDescriptors
  }

  /** Returns the class file location of a fully qualified name and whether it's on the classpath. */
  def findClass(fqn: String): Option[(AbstractFile, Boolean)] = {
    def getOutputClass(name: String): Option[AbstractFile] = {
      // This could be improved if a hint where to look is given.
      val className = name.replace('.', '/') + ".class"
      outputDirs.map(new File(_, className)).find((_.exists)).map((AbstractFile.getFile(_)))
    }

    def findOnClassPath(name: String): Option[AbstractFile] =
      classPath.findClass(name).flatMap(_.binary.asInstanceOf[Option[AbstractFile]])

    getOutputClass(fqn).map(f => (f, true)).orElse(findOnClassPath(fqn).map(f => (f, false)))
  }

  private[this] var callback0: AnalysisCallback = null

  /** Returns the active analysis callback, set by [[set]] and cleared by [[clear]]. */
  def callback: AnalysisCallback = callback0

  final def set(callback: AnalysisCallback, dreporter: DelegatingReporter): Unit = {
    this.callback0 = callback
    reporter = dreporter
  }

  final def clear(): Unit = {
    callback0 = null
    superDropRun()
    reporter = null
  }

  // Scala 2.10.x and later
  private[xsbt] def logUnreportedWarnings(seq: Seq[(String, List[(Position, String)])]): Unit = {
    for ((what, warnings) <- seq; (pos, msg) <- warnings)
      yield callback.problem(what, DelegatingReporter.convert(pos), msg, Severity.Warn, false)
    ()
  }
}

import scala.tools.nsc.interactive.RangePositions
final class ZincCompilerRangePos(settings: Settings, dreporter: DelegatingReporter, output: Output)
    extends ZincCompiler(settings, dreporter, output)
    with RangePositions
