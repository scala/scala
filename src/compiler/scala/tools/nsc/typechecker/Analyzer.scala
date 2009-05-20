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

  object typerFactory extends SubComponent {
    val global: Analyzer.this.global.type = Analyzer.this.global
    val phaseName = "typer"
    val runsAfter = List[String]()
    val runsRightAfter = Some("namer")
    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      resetTyper()
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

