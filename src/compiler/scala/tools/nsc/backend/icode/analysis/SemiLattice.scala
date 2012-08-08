/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.icode
package analysis

/** A complete lattice.
 */
trait SemiLattice {
  type Elem <: AnyRef

  /** Hold together local variable and stack state. The
   *  equals method uses reference equality for top and bottom,
   *  and structural equality for other values.
   */
  final case class IState[V, S](vars: V, stack: S) {
    override def hashCode = vars.hashCode + stack.hashCode
    override def equals(other: Any): Boolean = other match {
      case x: IState[_, _]  =>
        if ((this eq bottom) || (this eq top) || (x eq bottom) || (x eq top)) this eq x
        else stack == x.stack && vars == x.vars
      case _ =>
        false
    }
    private def tstring(x: Any): String = x match {
      case xs: TraversableOnce[_] => xs map tstring mkString " "
      case _                      => "" + x
    }
    override def toString = "IState(" + tstring(vars) + ", " + tstring(stack) + ")"
  }

  /** Return the least upper bound of a and b. */
  def lub2(exceptional: Boolean)(a: Elem, b: Elem): Elem

  /** Return the top element. */
  def top: Elem

  /** Return the bottom element. */
  def bottom: Elem

  /** Compute the least upper bound of a list of elements. */
  def lub(xs: List[Elem], exceptional: Boolean): Elem =
    if (xs.isEmpty) bottom
    else try xs reduceLeft lub2(exceptional)
    catch { case e: LubException  => Console.println("Lub on blocks: " + xs) ; throw e }
}
