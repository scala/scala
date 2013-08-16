/* NSC -- new Scala compiler
 *
 * @author Gerard Basler
 */

package scala.tools.nsc.transform.patmat

/** A literal.
 */
class Lit(val v: Int) extends AnyVal {
  def unary_- : Lit = Lit(-v)
  def variable: Int = Math.abs(v)
  def positive = v >= 0
}

object Lit {
  def apply(v: Int): Lit = new Lit(v)
}

/** Conjunctive normal form (of a Boolean formula).
 *  A formula in this form is amenable to a SAT solver
 *  (i.e., solver that decides satisfiability of a formula).
 */
class CNFBuilder {

  import scala.collection.mutable.ArrayBuffer

  // a clause is a disjunction of distinct literals
  type Clause = Set[Lit]
  type ClauseBuilder = ArrayBuffer[Clause]
  private[this] val buff = ArrayBuffer[Clause]()
  def clauses: Array[Clause] = buff.toArray

  private[this] var myLiteralCount = 0
  def allLiterals: Set[Lit] = (1 to myLiteralCount).map(Lit(_)).toSet
  def newLiteral(): Lit = {
    myLiteralCount += 1
    Lit(myLiteralCount)
  }

  lazy val constTrue: Lit = {
    val constTrue = newLiteral()
    addClauseProcessed(constTrue)
    constTrue
  }

  lazy val constFalse: Lit = -constTrue

  def isConst(l: Lit): Boolean = l == constTrue || l == constFalse

  def addClauseRaw(clause: Clause): Unit = buff += clause

  /** Add literals vector, ignores clauses that are trivially satisfied
   *
   *  @param bv
   */
  def addClauseProcessed(bv: Lit*) {
    val clause = processClause(bv: _*)
    if (clause.nonEmpty)
      addClauseRaw(clause)
  }

  /** @return empty clause, if clause trivially satisfied
   */
  private def processClause(bv: Lit*): Clause = {
    val clause = bv.distinct

    val isTautology = clause.combinations(2).exists {
      case Seq(a, b) => a == -b
    }

    if (isTautology)
      Set()
    else
      clause.toSet
  }

  override def toString: String = {
    for {
      clause <- buff
    } yield {
      clause.mkString("(", " ", ")")
    }
  }.mkString("\n")

  def dimacs: String = {
    val header = s"p cnf ${myLiteralCount} ${buff.length}\n"
    header + {
      for {
        clause <- buff
      } yield {
        clause.toSeq.sortBy(_.variable) mkString ("", " ", " 0")
      }
    }.mkString("\n")
  }
}
