/* NSC -- new Scala compiler
 *
 * @author Gerard Basler
 */

package scala.tools.nsc.transform.patmat

/**
 * A pair of a variable number and a sign, encoded as follows:
 *
 * @param sign 'false' means positive
 *             'true' means negative
 */
case class Lit(v: Int, sign: Boolean) {
  require(v > 0, "literal must be strictly positive")

  override def toString = s"${
    if (sign) "-" else "+"
  }$v"

  def unary_- : Lit = Lit(v, !sign)

  def neg: Lit = -this

  def pos: Lit = this

  def dimacs = if (sign) -v else v
}
