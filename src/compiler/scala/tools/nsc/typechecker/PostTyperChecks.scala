/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.reflect.internal.util.StatisticsStatics

/** The main attribution phase.
  */
trait PostTyperChecks extends AnyRef
  //            with Contexts
  //            with Namers
  //            with Typers
  //            with Infer
  //            with Implicits
  //            with EtaExpansion
  //            with SyntheticMethods
  //            with Unapplies
  //            with Macros
  //            with NamesDefaults
  //            with TypeDiagnostics
  //            with ContextErrors
  //            with StdAttachments
  //            with AnalyzerPlugins
{
  val global: Global

  import global._

  object postTyperFactory extends {
    val global: PostTyperChecks.this.global.type = PostTyperChecks.this.global
  } with SubComponent {

    import global.statistics

    val phaseName = "typerchecks"
    val runsAfter = List[String]("typer")
    val runsRightAfter = None

    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      override def run(): Unit = {
        val start = if (StatisticsStatics.areSomeColdStatsEnabled) statistics.startTimer(statistics.postTyperNanos) else null
        global.echoPhaseSummary(this)
        for (unit <- currentRun.units) {
          applyPhase(unit)
        }
        if (StatisticsStatics.areSomeColdStatsEnabled) statistics.stopTimer(statistics.postTyperNanos, start)
      }

      def apply(unit: CompilationUnit): Unit = {
        if (global.settings.Yrangepos) global.validatePositions(unit.body)
        if (settings.warnUnusedImport)
          analyzer.warnUnusedImports(unit)
        if (settings.warnUnused.isSetByUser) {
          val ignoreNames: Set[TermName] = Set(
            "readResolve", "readObject", "writeObject", "writeReplace"
          ).map(TermName(_))
          new global.analyzer.checkUnused(typer, ignoreNames).apply(unit)
        }
      }
    }
  }
}
