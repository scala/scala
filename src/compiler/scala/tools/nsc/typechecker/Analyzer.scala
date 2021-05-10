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

package scala.tools.nsc
package typechecker

/** The main attribution phase.
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
            with AnalyzerPlugins
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

      def apply(unit: CompilationUnit) {
        newNamer(rootContext(unit)).enterSym(unit.body)
      }
    }
  }

  object packageObjects extends {
    val global: Analyzer.this.global.type = Analyzer.this.global
  } with SubComponent {
    val phaseName = "packageobjects"
    val runsAfter = List[String]()
    val runsRightAfter= Some("namer")

    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      override val checkable = false
      import global._

      val openPackageObjectsTraverser = new Traverser {
        override def traverse(tree: Tree): Unit = tree match {
          case ModuleDef(_, _, _) =>
            if (tree.symbol.name == nme.PACKAGEkw) {
              openPackageModule(tree.symbol, tree.symbol.owner)
            }
          case ClassDef(_, _, _, _) => () // make it fast
          case _ => super.traverse(tree)
        }
      }

      def apply(unit: CompilationUnit) {
        openPackageObjectsTraverser(unit.body)
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
      override def shouldSkipThisPhaseForJava: Boolean = !(settings.YpickleJava || createJavadoc)
      resetTyper()
      // the log accumulates entries over time, even though it should not (Adriaan, Martin said so).
      // Lacking a better fix, we clear it here (before the phase is created, meaning for each
      // compiler run). This is good enough for the resident compiler, which was the most affected.
      undoLog.clear()
      override def run(): Unit = {
        val start = if (settings.areStatisticsEnabled) statistics.startTimer(statistics.typerNanos) else null
        global.echoPhaseSummary(this)
        val units = currentRun.units
        while (units.hasNext) {
          applyPhase(units.next())
          undoLog.clear()
        }
        finishComputeParamAlias()
        // defensive measure in case the bookkeeping in deferred macro expansion is buggy
        clearDelayed()
        if (settings.areStatisticsEnabled) statistics.stopTimer(statistics.typerNanos, start)
      }

      def apply(unit: CompilationUnit) {
        try {
          val typer = newTyper(rootContext(unit))
          unit.body = typer.typed(unit.body)
          // interactive typed may finish by throwing a `TyperResult`
          if (!settings.Youtline.value) {
            for (workItem <- unit.toCheck) workItem()
            if (settings.warnUnusedImport)
              warnUnusedImports(unit)
            if (settings.warnUnused.isSetByUser)
              new checkUnused(typer).apply(unit)
          }
        }
        finally {
          runReporting.reportSuspendedMessages(unit)
          unit.toCheck.clear()
        }
      }
    }
  }
}
