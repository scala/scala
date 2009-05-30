/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.typechecker

/** The main attribution phase.
 */
trait Analyzer extends AnyRef
            with Contexts
            with Namers
            with Typers
            with Infer
            with Implicits
            with Variances
            with EtaExpansion
            with SyntheticMethods
            with Unapplies
            with NamesDefaults
{
  val global : Global
  import global._

  object namerFactory extends SubComponent {
    val global: Analyzer.this.global.type = Analyzer.this.global
    val phaseName = "namer"
    val runsAfter = List[String]("parser")
    val runsRightAfter = None
    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      override val checkable = false
      def apply(unit: CompilationUnit) {
        newNamer(rootContext(unit)).enterSym(unit.body)
      }
    }
  }

  var typerTime = 0L

  object typerFactory extends SubComponent {
    val global: Analyzer.this.global.type = Analyzer.this.global
    val phaseName = "typer"
    val runsAfter = List[String]()
    val runsRightAfter = Some("namer")
    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      resetTyper()
      override def run {
        val start = System.nanoTime()
        currentRun.units foreach applyPhase
        /*
        typerTime += System.nanoTime() - start
        def show(time: Long) = "%2.1f".format(time.toDouble / typerTime * 100)+" / "+time+"ns"
        println("time spent typechecking: "+show(typerTime))
        println("time spent in implicits: "+show(implicitTime))
        println("    successful in scope: "+show(inscopeSucceed))
        println("        failed in scope: "+show(inscopeFail))
        println("     successful of type: "+show(oftypeSucceed))
        println("         failed of type: "+show(oftypeFail))
        println("    successful manifest: "+show(manifSucceed))
        println("        failed manifest: "+show(manifFail))
        println("implicit cache hitratio: "+"%2.1f".format(hits.toDouble / (hits + misses) * 100))
        println("time spent in failed   : "+show(failedSilent))
        println("       failed op=      : "+show(failedOpEqs))
        println("       failed applu    : "+show(failedApplies))
        */
        typerTime = 0L
        implicitTime = 0L
        inscopeSucceed = 0L
        inscopeFail = 0L
        oftypeSucceed = 0L
        oftypeFail = 0L
        manifSucceed = 0L
        manifFail = 0L
        hits = 0
        misses = 0
      }
      def apply(unit: CompilationUnit) {
        try {
          unit.body = newTyper(rootContext(unit)).typed(unit.body)
          for (workItem <- unit.toCheck) workItem()
        } finally {
          unit.toCheck.clear()
        }
      }
    }
  }
}

