/* NSC -- new Scala compiler
 *
 * @author Gerard Basler
 */

package scala.tools.nsc.transform.patmat

/**
 * Conjunctive normal form (of a Boolean formula).
 * A formula in this form is amenable to a SAT solver
 * (i.e., solver that decides satisfiability of a formula).
 */
class CNF {

  import scala.collection.mutable.ArrayBuffer

  // a clause is a disjunction of distinct literals
  type Clause = Set[Lit]
  type ClauseBuilder = ArrayBuffer[Clause]

  var noLiterals = 0

  val buff = ArrayBuffer[Clause]()

  def clauses: Array[Clause] = buff.toArray

  def newLiteral(): Lit = {
    noLiterals += 1
    Lit(noLiterals, false)
  }

  val constTrue: Lit = {
    val constTrue = newLiteral()
    addClauseProcessed(constTrue.pos)
    constTrue
  }

  val constFalse: Lit = -constTrue

  def isConst(l: Lit): Boolean = l == constTrue || l == constFalse

  def addClauseRaw(clause: Clause): Unit = buff += clause

  /**
   * Add literals vector, ignores clauses that are trivially satisfied
   *
   * @param bv
   */
  def addClauseProcessed(bv: Lit*) {
    val clause = processClause(bv: _*)
    if (clause.nonEmpty)
      addClauseRaw(clause)
  }

  /**
   * @return empty clause, if clause trivially satisfied
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
    val header = s"p cnf ${noLiterals} ${buff.length}\n"
    header + {
      for {
        clause <- buff
      } yield {
        clause.toSeq.sortBy(l => math.abs(l.v)) mkString("", " ", " 0")
      }
    }.mkString("\n")
  }
}
