/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.reflect.internal.util.StatisticsStatics

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
            with MacroAnnotationAttachments
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

      def apply(unit: CompilationUnit): Unit = {
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

      val openPackageObjectsTraverser = new InternalTraverser {
        override def traverse(tree: Tree): Unit = tree match {
          case ModuleDef(_, _, _) =>
            if (tree.symbol.name == nme.PACKAGEkw) {
              openPackageModule(tree.symbol, tree.symbol.owner)
            }
          case ClassDef(_, _, _, _) => () // make it fast
          case _ => tree.traverse(this)
        }
      }

      def apply(unit: CompilationUnit): Unit = {
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
      resetTyper()
      // the log accumulates entries over time, even though it should not (Adriaan, Martin said so).
      // Lacking a better fix, we clear it here (before the phase is created, meaning for each
      // compiler run). This is good enough for the resident compiler, which was the most affected.
      undoLog.clear()
      override def run(): Unit = {
        val start = if (StatisticsStatics.areSomeColdStatsEnabled) statistics.startTimer(statistics.typerNanos) else null
        global.echoPhaseSummary(this)
        val units = currentRun.units
        while (units.hasNext) {
          applyPhase(units.next())
          undoLog.clear()
        }
        finishComputeParamAlias()
        // defensive measure in case the bookkeeping in deferred macro expansion is buggy
        clearDelayed()
        if (StatisticsStatics.areSomeColdStatsEnabled) statistics.stopTimer(statistics.typerNanos, start)
      }
      def apply(unit: CompilationUnit): Unit = {
        try {
          val typer = newTyper(rootContext(unit))
          unit.body = typer.typed(unit.body)
          for (workItem <- unit.toCheck) workItem()
          if (settings.warnUnusedImport)
            warnUnusedImports(unit)
          if (settings.warnUnused.isSetByUser)
            new checkUnused(typer).apply(unit)
        }
        finally {
          unit.toCheck.clear()
        }
      }
    }
  }
}
