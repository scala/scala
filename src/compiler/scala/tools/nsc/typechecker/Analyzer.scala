/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package typechecker

import scala.collection.mutable.ArrayDeque

/** Defines the sub-components for the namer, packageobjects, and typer phases.
 */
trait Analyzer extends AnyRef
            with Contexts
            with Namers
            with Typers
            with Infer
            with Implicits
            with EtaExpansion
            with SyntheticMethods
            with Unapplies
            with Macros
            with NamesDefaults
            with TypeDiagnostics
            with ContextErrors
            with StdAttachments
            with MacroAnnotationAttachments
            with AnalyzerPlugins
            with ImportTracking
{
  val global : Global
  import global._

  object namerFactory extends {
    val global: Analyzer.this.global.type = Analyzer.this.global
  } with SubComponent {
    val phaseName = "namer"
    val runsAfter = List[String]("parser")
    val runsRightAfter = None
    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      override val checkable = false
      override def keepsTypeParams = false

      def apply(unit: CompilationUnit): Unit = newNamer(rootContext(unit)).enterSym(unit.body)
    }
  }

  object packageObjects extends {
    val global: Analyzer.this.global.type = Analyzer.this.global
  } with SubComponent {
    val deferredOpen = perRunCaches.newSet[Symbol]()
    val phaseName = "packageobjects"
    val runsAfter = List[String]()
    val runsRightAfter= Some("namer")

    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      override val checkable = false
      import global._

      val openPackageObjectsTraverser = new InternalTraverser {
        override def traverse(tree: Tree): Unit = tree match {
          case ModuleDef(_, _, _) =>
            if (tree.symbol.name == nme.PACKAGEkw) {
              // we've actually got a source file
              deferredOpen.subtractOne(tree.symbol.owner)

              openPackageModule(tree.symbol, tree.symbol.owner)
            }
          case ClassDef(_, _, _, _) => () // make it fast
          case _ => tree.traverse(this)
        }
      }

      def apply(unit: CompilationUnit): Unit = {
        openPackageObjectsTraverser(unit.body)
        deferredOpen.foreach(openPackageModule(_))
        deferredOpen.clear()
      }
    }
  }

  object typerFactory extends {
    val global: Analyzer.this.global.type = Analyzer.this.global
  } with SubComponent {
    import global.statistics
    val phaseName = "typer"
    val runsAfter = List[String]()
    val runsRightAfter = Some("packageobjects")
    def newPhase(prev: Phase): StdPhase = new TyperPhase(prev)
    final class TyperPhase(prev: Phase) extends StdPhase(prev) {
      override def keepsTypeParams = false
      override def shouldSkipThisPhaseForJava: Boolean = !settings.YpickleJava.value && !createJavadoc
      resetTyper()
      // the log accumulates entries over time, even though it should not (Adriaan, Martin said so).
      // Lacking a better fix, we clear it here (before the phase is created, meaning for each
      // compiler run). This is good enough for the resident compiler, which was the most affected.
      undoLog.clear()
      private val toCheckAfterTyper = ArrayDeque.empty[CompilationUnit.ToCheckAfterTyper]
      def addCheckAfterTyper(check: CompilationUnit.ToCheckAfterTyper): Unit = toCheckAfterTyper.append(check)
      override def run(): Unit = {
        val start = if (settings.areStatisticsEnabled) statistics.startTimer(statistics.typerNanos) else null
        global.echoPhaseSummary(this)
        val units = currentRun.units

        while (units.hasNext) {
          applyPhase(units.next())
          undoLog.clear()
        }
        finishComputeParamAlias()
        try while (toCheckAfterTyper.nonEmpty) toCheckAfterTyper.removeHead().apply()
        finally toCheckAfterTyper.clearAndShrink()
        // defensive measure in case the bookkeeping in deferred macro expansion is buggy
        clearDelayed()
        if (settings.areStatisticsEnabled) statistics.stopTimer(statistics.typerNanos, start)
      }
      def apply(unit: CompilationUnit): Unit = {
        try {
          val typer = newTyper(rootContext(unit))
          unit.body = typer.typed(unit.body)
          // interactive typed may finish by throwing a `TyperResult`
          if (!settings.Youtline.value) {
            while (unit.toCheck.nonEmpty) {
              unit.toCheck.removeHead() match {
                case now: CompilationUnit.ToCheckAfterUnit => now()
                case later: CompilationUnit.ToCheckAfterTyper => addCheckAfterTyper(later)
              }
            }
            if (!settings.isScaladoc && settings.warnUnusedImport)
              warnUnusedImports(unit)
            if (!settings.isScaladoc && settings.warnUnused.isSetByUser)
              new checkUnused(typer).apply(unit)
          }
        }
        finally {
          runReporting.reportSuspendedMessages(unit)
          unit.toCheck.clearAndShrink()
        }
      }
    }
  }
}
