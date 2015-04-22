package scala.tools.nsc.transform.patmat

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.mutable
import scala.reflect.internal.util.Position
import scala.tools.nsc.{Global, Settings}

object TestSolver extends Logic with Solving {

  val global: Global = new Global(new Settings())

  // disable max recursion depth in order to get all solutions
  global.settings.YpatmatExhaustdepth.tryToSet("off" :: Nil)

  object TestSolver extends Solver {

    class Const {
      override def toString: String = "Const"
    }

    val NullConst = new Const
    type Type = Int

    case class TypeConst(i: Int) extends Const

    object TypeConst extends TypeConstExtractor

    case class ValueConst(i: Int) extends Const

    object ValueConst extends ValueConstExtractor {
      def apply(t: Tree): Const = ???
    }

    case class Tree(name: String)

    class Var(val x: Tree) extends AbsVar {

      override def equals(other: scala.Any): Boolean = other match {
        case that: Var => this.x == that.x
        case _         => false
      }

      override def hashCode(): Int = x.hashCode()

      override def toString: String = {
        s"Var($x)"
      }

      def domainSyms = None

      def groupedDomains: List[Set[TestSolver.Sym]] = Nil

      def implications = Nil

      def mayBeNull = false

      def propForEqualsTo(c: Const): Prop = ???

      def registerEquality(c: Const) = ()

      def registerNull() = ()

      def symForStaticTp = None
    }

    object Var extends VarExtractor {
      def apply(x: Tree): Var = new Var(x)

      def unapply(v: Var): Some[Tree] = Some(v.x)
    }

    def prepareNewAnalysis() = {}

    def uncheckedWarning(pos: Position, msg: String) = sys.error(msg)

    def reportWarning(msg: String) = sys.error(msg)

    /**
     * The DPLL procedure only returns a minimal mapping from literal to value
     * such that the CNF formula is satisfied.
     * E.g. for:
     * `(a \/ b)`
     * The DPLL procedure will find either {a = true} or {b = true}
     * as solution.
     *
     * The expansion step will amend both solutions with the unassigned variable
     * i.e., {a = true} will be expanded to {a = true, b = true} and
     * {a = true, b = false}.
     */
    def expandUnassigned(solution: Solution): List[Model] = {
      import solution._

      // the number of solutions is doubled for every unassigned variable
      val expandedModels = 1 << unassigned.size
      var current = mutable.ArrayBuffer[Model]()
      var next = mutable.ArrayBuffer[Model]()
      current.sizeHint(expandedModels)
      next.sizeHint(expandedModels)

      current += model

      // we use double buffering:
      // read from `current` and create a two models for each model in `next`
      for {
        s <- unassigned
      } {
        for {
          model <- current
        } {
          def force(s: Sym, pol: Boolean) = model + (s -> pol)

          next += force(s, pol = true)
          next += force(s, pol = false)
        }

        val tmp = current
        current = next
        next = tmp

        next.clear()
      }

      current.toList
    }

    /**
     * Old CNF conversion code, used for reference:
     * - convert formula into NNF
     *   (i.e., no negated terms, only negated variables)
     * - use distributive laws to convert into CNF
     */
    def eqFreePropToSolvableViaDistribution(p: Prop) = {
      val symbolMapping = new SymbolMapping(gatherSymbols(p))

      type Formula = Array[TestSolver.Clause]

      def formula(c: Clause*): Formula = c.toArray

      def merge(a: Clause, b: Clause) = a ++ b

      def negationNormalFormNot(p: Prop): Prop = p match {
        case And(ps) => Or(ps map negationNormalFormNot)
        case Or(ps)  => And(ps map negationNormalFormNot)
        case Not(p)  => negationNormalForm(p)
        case True    => False
        case False   => True
        case s: Sym  => Not(s)
      }

      def negationNormalForm(p: Prop): Prop = p match {
        case Or(ps)       => Or(ps map negationNormalForm)
        case And(ps)      => And(ps map negationNormalForm)
        case Not(negated) => negationNormalFormNot(negated)
        case True
             | False
             | (_: Sym)   => p
      }

      val TrueF: Formula = Array()
      val FalseF = Array(clause())
      def lit(sym: Sym) = Array(clause(symbolMapping.lit(sym)))
      def negLit(sym: Sym) = Array(clause(-symbolMapping.lit(sym)))

      def conjunctiveNormalForm(p: Prop): Formula = {
        def distribute(a: Formula, b: Formula): Formula =
          (a, b) match {
            // true \/ _ = true
            // _ \/ true = true
            case (trueA, trueB) if trueA.size == 0 || trueB.size == 0 => TrueF
            // lit \/ lit
            case (a, b) if a.size == 1 && b.size == 1 => formula(merge(a(0), b(0)))
            // (c1 /\ ... /\ cn) \/ d = ((c1 \/ d) /\ ... /\ (cn \/ d))
            // d \/ (c1 /\ ... /\ cn) = ((d \/ c1) /\ ... /\ (d \/ cn))
            case (cs, ds) =>
              val (big, small) = if (cs.size > ds.size) (cs, ds) else (ds, cs)
              big flatMap (c => distribute(formula(c), small))
          }

        p match {
          case True        => TrueF
          case False       => FalseF
          case s: Sym      => lit(s)
          case Not(s: Sym) => negLit(s)
          case And(ps)     =>
            ps.toArray flatMap conjunctiveNormalForm
          case Or(ps)      =>
            ps map conjunctiveNormalForm reduceLeft { (a, b) =>
              distribute(a, b)
            }
        }
      }
      val cnf = conjunctiveNormalForm(negationNormalForm(p))
      Solvable(cnf, symbolMapping)
    }

  }

}

/**
 * Testing CNF conversion via Tseitin vs NNF & expansion.
 */
@RunWith(classOf[JUnit4])
class SolvingTest {

  import scala.tools.nsc.transform.patmat.TestSolver.TestSolver._

  object SymName {
    def unapply(s: Sym): Option[String] = {
      val Var(Tree(name)) = s.variable
      Some(name)
    }
  }

  implicit val ModelOrd: Ordering[TestSolver.TestSolver.Model] = Ordering.by {
    _.toSeq.sortWith {
      case ((sym1, v1), (sym2, v2)) =>
        val SymName(name1) = sym1
        val SymName(name2) = sym2
        if (name1 < name2)
          true
        else if (name1 > name2)
          false
        else
          v1 < v2
    }.toIterable
  }

  implicit val SolutionOrd: Ordering[TestSolver.TestSolver.Solution] =
    Ordering.by(_.model)

  def formatSolution(solution: Solution): String = {
    formatModel(solution.model)
  }

  def formatModel(model: Model): String = {
    (for {
      (SymName(name), value) <- model
    } yield {
      val v = if (value) "T" else "F"
      s"$name -> $v"
    }).mkString(", ")
  }

  def sym(name: String) = Sym(Var(Tree(name)), NullConst)

  @Test
  def testSymCreation() {
    val s1 = sym("hello")
    val s2 = sym("hello")
    assertEquals(s1, s2)
  }

  /**
   * Simplest possible test: solve a formula and check the solution(s)
   */
  @Test
  def testUnassigned() {
    val pSym = sym("p")
    val solvable = propToSolvable(Or(pSym, Not(pSym)))
    val solutions = TestSolver.TestSolver.findAllModelsFor(solvable)
    val expected = List(Solution(Map(), List(pSym)))
    assertEquals(expected, solutions)
  }

  /**
   * Unassigned variables must be expanded
   * for stable results
   */
  @Test
  def testNoUnassigned() {
    val pSym = sym("p")
    val qSym = sym("q")
    val solvable = propToSolvable(Or(pSym, Not(qSym)))
    val solutions = findAllModelsFor(solvable)
    val expanded = solutions.flatMap(expandUnassigned).sorted
    val expected = Seq(
      Map(pSym -> false, qSym -> false),
      Map(pSym -> true, qSym -> false),
      Map(pSym -> true, qSym -> true)
    ).sorted

    assertEquals(expected, expanded)
  }

  @Test
  def testTseitinVsExpansionFrom_t7020() {
    val formulas = Seq(
      And(And(And(Not(sym("V1=null")),
        sym("V1=scala.collection.immutable.::[?]")), And(Not(sym("V1=null")),
        And(Or(sym("V2=4"), Or(sym("V2=5"), sym("V2=6"))), sym("V3=Nil")))),
        And(And(Or(Not(sym("V1=scala.collection.immutable.::[?]")),
          Not(sym("V1=null"))), And(Or(sym("V3=scala.collection.immutable.::[?]"),
          Or(sym("V3=Nil"), sym("V3=null"))), And(Or(Not(sym("V3=Nil")),
          Not(sym("V3=null"))),
          And(Or(Not(sym("V3=scala.collection.immutable.::[?]")),
            Not(sym("V3=null"))), And(Or(Not(sym("V1=Nil")), Not(sym("V1=null"))),
            Or(sym("V1=scala.collection.immutable.::[?]"), Or(sym("V1=Nil"),
              sym("V1=null")))))))), And(Or(Or(sym("V1=null"),
          Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
          Or(Not(sym("V2=1")), Not(sym("V3=Nil"))))), Or(Or(sym("V1=null"),
          Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
          Or(Not(sym("V2=2")), Not(sym("V3=Nil")))))))),

      And(And(And(Not(sym("V1=null")),
        sym("V1=scala.collection.immutable.::[?]")), And(Not(sym("V1=null")),
        And(sym("V2=7"), sym("V3=Nil")))),
        And(And(Or(Not(sym("V1=scala.collection.immutable.::[?]")),
          Not(sym("V1=null"))), And(Or(sym("V3=scala.collection.immutable.::[?]"),
          Or(sym("V3=Nil"), sym("V3=null"))), And(Or(Not(sym("V3=Nil")),
          Not(sym("V3=null"))),
          And(Or(Not(sym("V3=scala.collection.immutable.::[?]")),
            Not(sym("V3=null"))), And(Or(Not(sym("V1=Nil")), Not(sym("V1=null"))),
            Or(sym("V1=scala.collection.immutable.::[?]"), Or(sym("V1=Nil"),
              sym("V1=null")))))))), And(And(Or(Or(sym("V1=null"),
          Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
          Or(Not(sym("V2=1")), Not(sym("V3=Nil"))))), Or(Or(sym("V1=null"),
          Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
          Or(Not(sym("V2=2")), Not(sym("V3=Nil")))))), Or(Or(sym("V1=null"),
          Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
          Or(And(Not(sym("V2=4")), And(Not(sym("V2=5")), Not(sym("V2=6")))),
            Not(sym("V3=Nil")))))))),

      And(And(Not(sym("V1=null")),
        sym("V1=scala.collection.immutable.::[?]")), And(Not(sym("V1=null")),
        And(Or(sym("V2=4"), Or(sym("V2=5"), sym("V2=6"))), sym("V3=Nil")))),

      And(And(Not(sym("V1=null")), sym("V1=scala.collection.immutable.::[?]")),
        And(Not(sym("V1=null")), And(sym("V2=7"), sym("V3=Nil")))),

      And(And(Or(Not(sym("V1=scala.collection.immutable.::[?]")),
        Not(sym("V1=null"))), And(Or(sym("V3=scala.collection.immutable.::[?]"),
        Or(sym("V3=Nil"), sym("V3=null"))), And(Or(Not(sym("V3=Nil")),
        Not(sym("V3=null"))),
        And(Or(Not(sym("V3=scala.collection.immutable.::[?]")),
          Not(sym("V3=null"))), And(Or(Not(sym("V1=Nil")), Not(sym("V1=null"))),
          Or(sym("V1=scala.collection.immutable.::[?]"), Or(sym("V1=Nil"),
            sym("V1=null")))))))), And(And(Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(Not(sym("V2=1")), Not(sym("V3=Nil"))))), Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(Not(sym("V2=2")), Not(sym("V3=Nil")))))), Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(And(Not(sym("V2=4")), And(Not(sym("V2=5")), Not(sym("V2=6")))),
          Not(sym("V3=Nil"))))))),

      And(And(Or(Not(sym("V1=scala.collection.immutable.::[?]")),
        Not(sym("V1=null"))), And(Or(sym("V3=scala.collection.immutable.::[?]"),
        Or(sym("V3=Nil"), sym("V3=null"))), And(Or(Not(sym("V3=Nil")),
        Not(sym("V3=null"))),
        And(Or(Not(sym("V3=scala.collection.immutable.::[?]")),
          Not(sym("V3=null"))), And(Or(Not(sym("V1=Nil")), Not(sym("V1=null"))),
          Or(sym("V1=scala.collection.immutable.::[?]"), Or(sym("V1=Nil"),
            sym("V1=null")))))))), And(Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(Not(sym("V2=1")), Not(sym("V3=Nil"))))), Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(Not(sym("V2=2")), Not(sym("V3=Nil"))))))),

      And(And(Or(Not(sym("V1=scala.collection.immutable.::[?]")),
        Not(sym("V1=null"))), And(Or(sym("V3=scala.collection.immutable.::[?]"),
        Or(sym("V3=Nil"), sym("V3=null"))), And(Or(Not(sym("V3=Nil")),
        Not(sym("V3=null"))),
        And(Or(Not(sym("V3=scala.collection.immutable.::[?]")),
          Not(sym("V3=null"))), And(Or(Not(sym("V1=Nil")), Not(sym("V1=null"))),
          Or(sym("V1=scala.collection.immutable.::[?]"), Or(sym("V1=Nil"),
            sym("V1=null")))))))), And(sym("V1=Nil"), And(Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(And(Not(sym("V2=4")), And(Not(sym("V2=5")), Not(sym("V2=6")))),
          Not(sym("V3=Nil"))))), And(Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(Not(sym("V2=1")), Not(sym("V3=Nil"))))), Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(Not(sym("V2=2")), Not(sym("V3=Nil"))))))))),

      And(And(Or(Or(False,
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(False,
        Or(Not(sym("V2=1")), Not(sym("V3=Nil"))))), Or(Or(False,
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(False,
        Or(Not(sym("V2=2")), Not(sym("V3=Nil")))))), And(Or(Or(False,
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(False,
        Or(And(Not(sym("V2=4")), And(Not(sym("V2=5")), Not(sym("V2=6")))),
          Not(sym("V3=Nil"))))), And(Or(Or(False,
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(False,
        Or(Not(sym("V2=7")), Not(sym("V3=Nil"))))), Not(sym("V1=Nil"))))),

      And(And(Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(Not(sym("V2=1")), Not(sym("V3=Nil"))))), Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(Not(sym("V2=2")), Not(sym("V3=Nil")))))), Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(And(Not(sym("V2=4")), And(Not(sym("V2=5")), Not(sym("V2=6")))),
          Not(sym("V3=Nil")))))),

      And(And(Or(sym("V3=scala.collection.immutable.::[?]"), sym("V3=Nil")),
        Or(sym("V1=scala.collection.immutable.::[?]"), sym("V1=Nil"))),
        And(And(Or(Or(False, Not(sym("V1=scala.collection.immutable.::[?]"))),
          Or(False, Or(Not(sym("V2=1")), Not(sym("V3=Nil"))))), Or(Or(False,
          Not(sym("V1=scala.collection.immutable.::[?]"))), Or(False,
          Or(Not(sym("V2=2")), Not(sym("V3=Nil")))))), And(Or(Or(False,
          Not(sym("V1=scala.collection.immutable.::[?]"))), Or(False,
          Or(And(Not(sym("V2=4")), And(Not(sym("V2=5")), Not(sym("V2=6")))),
            Not(sym("V3=Nil"))))), And(Or(Or(False,
          Not(sym("V1=scala.collection.immutable.::[?]"))), Or(False,
          Or(Not(sym("V2=7")), Not(sym("V3=Nil"))))), Not(sym("V1=Nil")))))),

      And(Not(sym("V1=null")), And(Or(sym("V2=4"), Or(sym("V2=5"), sym("V2=6"))),
        sym("V3=Nil"))),

      And(Not(sym("V1=null")), And(sym("V2=7"), sym("V3=Nil"))),

      And(Not(sym("V1=null")), sym("V1=scala.collection.immutable.::[?]")),

      And(Not(sym("V2=4")), And(Not(sym("V2=5")), Not(sym("V2=6")))),

      And(Not(sym("V2=5")), Not(sym("V2=6"))),

      And(Or(Not(sym("V1=Nil")), Not(sym("V1=null"))),
        Or(sym("V1=scala.collection.immutable.::[?]"), Or(sym("V1=Nil"),
          sym("V1=null")))),

      And(Or(Not(sym("V1=scala.collection.immutable.::[?]")),
        Not(sym("V1=null"))), And(Or(sym("V3=scala.collection.immutable.::[?]"),
        Or(sym("V3=Nil"), sym("V3=null"))), And(Or(Not(sym("V3=Nil")),
        Not(sym("V3=null"))),
        And(Or(Not(sym("V3=scala.collection.immutable.::[?]")),
          Not(sym("V3=null"))), And(Or(Not(sym("V1=Nil")), Not(sym("V1=null"))),
          Or(sym("V1=scala.collection.immutable.::[?]"), Or(sym("V1=Nil"),
            sym("V1=null")))))))),

      And(Or(Not(sym("V3=Nil")), Not(sym("V3=null"))),
        And(Or(Not(sym("V3=scala.collection.immutable.::[?]")),
          Not(sym("V3=null"))), And(Or(Not(sym("V1=Nil")), Not(sym("V1=null"))),
          Or(sym("V1=scala.collection.immutable.::[?]"), Or(sym("V1=Nil"),
            sym("V1=null")))))),

      And(Or(Not(sym("V3=scala.collection.immutable.::[?]")),
        Not(sym("V3=null"))), And(Or(Not(sym("V1=Nil")), Not(sym("V1=null"))),
        Or(sym("V1=scala.collection.immutable.::[?]"), Or(sym("V1=Nil"),
          sym("V1=null"))))),

      And(Or(Or(False,
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(False,
        Or(And(Not(sym("V2=4")), And(Not(sym("V2=5")), Not(sym("V2=6")))),
          Not(sym("V3=Nil"))))), And(Or(Or(False,
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(False,
        Or(Not(sym("V2=7")), Not(sym("V3=Nil"))))), Not(sym("V1=Nil")))),

      And(Or(Or(False, Not(sym("V1=scala.collection.immutable.::[?]"))), Or(False,
        Or(Not(sym("V2=1")), Not(sym("V3=Nil"))))), Or(Or(False,
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(False,
        Or(Not(sym("V2=2")), Not(sym("V3=Nil")))))),

      And(Or(Or(False,
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(False,
        Or(Not(sym("V2=7")), Not(sym("V3=Nil"))))), Not(sym("V1=Nil"))),

      And(Or(Or(sym("V1=null"), Not(sym("V1=scala.collection.immutable.::[?]"))),
        Or(sym("V1=null"), Or(And(Not(sym("V2=4")), And(Not(sym("V2=5")),
          Not(sym("V2=6")))), Not(sym("V3=Nil"))))), And(Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(Not(sym("V2=1")), Not(sym("V3=Nil"))))), Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(Not(sym("V2=2")), Not(sym("V3=Nil"))))))),

      And(Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(Not(sym("V2=1")), Not(sym("V3=Nil"))))), Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(Not(sym("V2=2")), Not(sym("V3=Nil")))))),

      And(Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(Not(sym("V2=7")), Not(sym("V3=Nil"))))),
        And(And(Or(Not(sym("V1=scala.collection.immutable.::[?]")),
          Not(sym("V1=null"))), And(Or(sym("V3=scala.collection.immutable.::[?]"),
          Or(sym("V3=Nil"), sym("V3=null"))), And(Or(Not(sym("V3=Nil")),
          Not(sym("V3=null"))),
          And(Or(Not(sym("V3=scala.collection.immutable.::[?]")),
            Not(sym("V3=null"))), And(Or(Not(sym("V1=Nil")), Not(sym("V1=null"))),
            Or(sym("V1=scala.collection.immutable.::[?]"), Or(sym("V1=Nil"),
              sym("V1=null")))))))), And(sym("V1=Nil"), And(Or(Or(sym("V1=null"),
          Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
          Or(And(Not(sym("V2=4")), And(Not(sym("V2=5")), Not(sym("V2=6")))),
            Not(sym("V3=Nil"))))), And(Or(Or(sym("V1=null"),
          Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
          Or(Not(sym("V2=1")), Not(sym("V3=Nil"))))), Or(Or(sym("V1=null"),
          Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
          Or(Not(sym("V2=2")), Not(sym("V3=Nil")))))))))),

      And(Or(sym("V2=4"), Or(sym("V2=5"), sym("V2=6"))), sym("V3=Nil")),

      And(Or(sym("V3=scala.collection.immutable.::[?]"), Or(sym("V3=Nil"),
        sym("V3=null"))), And(Or(Not(sym("V3=Nil")), Not(sym("V3=null"))),
        And(Or(Not(sym("V3=scala.collection.immutable.::[?]")),
          Not(sym("V3=null"))), And(Or(Not(sym("V1=Nil")), Not(sym("V1=null"))),
          Or(sym("V1=scala.collection.immutable.::[?]"), Or(sym("V1=Nil"),
            sym("V1=null"))))))),

      And(Or(sym("V3=scala.collection.immutable.::[?]"),
        sym("V3=Nil")), Or(sym("V1=scala.collection.immutable.::[?]"),
        sym("V1=Nil"))),

      And(sym("V1=Nil"), And(Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(And(Not(sym("V2=4")), And(Not(sym("V2=5")), Not(sym("V2=6")))),
          Not(sym("V3=Nil"))))), And(Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(Not(sym("V2=1")), Not(sym("V3=Nil"))))), Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(Not(sym("V2=2")), Not(sym("V3=Nil")))))))),

      And(sym("V2=7"), sym("V3=Nil")),

      False,

      Not(sym("V1=Nil")),

      Or(And(Not(sym("V2=4")),
        And(Not(sym("V2=5")), Not(sym("V2=6")))), Not(sym("V3=Nil"))),

      Or(False, Not(sym("V1=scala.collection.immutable.::[?]"))),

      Or(False,
        Or(And(Not(sym("V2=4")), And(Not(sym("V2=5")), Not(sym("V2=6")))),
          Not(sym("V3=Nil")))),

      Or(False, Or(Not(sym("V2=1")), Not(sym("V3=Nil")))),

      Or(Not(sym("V1=Nil")), Not(sym("V1=null"))),

      Or(Not(sym("V3=scala.collection.immutable.::[?]")), Not(sym("V3=null"))),

      Or(Or(False, Not(sym("V1=scala.collection.immutable.::[?]"))), Or(False,
        Or(And(Not(sym("V2=4")), And(Not(sym("V2=5")), Not(sym("V2=6")))),
          Not(sym("V3=Nil"))))),

      Or(Or(False,
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(False,
        Or(Not(sym("V2=1")), Not(sym("V3=Nil"))))),

      Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(And(Not(sym("V2=4")), And(Not(sym("V2=5")), Not(sym("V2=6")))),
          Not(sym("V3=Nil"))))),

      Or(Or(sym("V1=null"),
        Not(sym("V1=scala.collection.immutable.::[?]"))), Or(sym("V1=null"),
        Or(Not(sym("V2=1")), Not(sym("V3=Nil"))))),

      Or(sym("V1=null"), Not(sym("V1=scala.collection.immutable.::[?]"))),

      Or(sym("V1=null"),
        Or(And(Not(sym("V2=4")), And(Not(sym("V2=5")), Not(sym("V2=6")))),
          Not(sym("V3=Nil")))),

      Or(sym("V1=null"), Or(Not(sym("V2=1")), Not(sym("V3=Nil")))),

      Or(sym("V1=scala.collection.immutable.::[?]"),
        Or(sym("V1=Nil"), sym("V1=null"))),

      Or(sym("V1=scala.collection.immutable.::[?]"), sym("V1=Nil")),

      Or(sym("V2=4"), Or(sym("V2=5"), sym("V2=6"))),

      sym("V3=scala.collection.immutable.::[?]")
    )

    formulas foreach {
      f =>
        // build CNF
        val tseitinCnf = propToSolvable(f)
        val expansionCnf = eqFreePropToSolvableViaDistribution(f)

        // ALL-SAT
        val tseitinSolutions = findAllModelsFor(tseitinCnf)
        val expansionSolutins = findAllModelsFor(expansionCnf)

        // expand unassigned variables
        // (otherwise solutions can not be compared)
        val tseitinNoUnassigned = tseitinSolutions.flatMap(expandUnassigned).sorted
        val expansionNoUnassigned = expansionSolutins.flatMap(expandUnassigned).sorted
        assertEquals(tseitinNoUnassigned, expansionNoUnassigned)
    }
  }

  def pairWiseEncoding(ops: List[Sym]) = {
    And(ops.combinations(2).collect {
      case a :: b :: Nil => Or(Not(a), Not(b))
    }.toSet[TestSolver.TestSolver.Prop])
  }

  @Test
  def testAtMostOne() {
    val dummySym = sym("dummy")
    val syms = "pqrstu".map(c => sym(c.toString)).toList
    // expand unassigned variables
    // (otherwise solutions can not be compared)
    val expected = TestSolver.TestSolver.findAllModelsFor(propToSolvable(And(dummySym, pairWiseEncoding(syms)))).flatMap(expandUnassigned)
    val actual = TestSolver.TestSolver.findAllModelsFor(propToSolvable(And(dummySym, AtMostOne(syms)))).flatMap(expandUnassigned)
    assertEquals(expected.toSet, actual.toSet)
  }
}


