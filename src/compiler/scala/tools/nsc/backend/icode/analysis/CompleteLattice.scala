/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc
package backend.icode.analysis

/** A complete lattice.
 */
trait CompleteLattice {
  type Elem <: AnyRef

  /** Hold together local variable and stack state. The
   *  equals method uses reference equality for top and bottom,
   *  and structural equality for other values.
   */
  case class IState[V, S](val vars: V, val stack: S) {
    override def equals(other: Any): Boolean = other match {
      case that: IState[_, _] =>
        if ((this eq bottom) || (that eq bottom)) this eq that
        else if ((this eq top) || (that eq top)) this eq that
        else (stack == that.stack && vars == that.vars)
      case _ => false
    }
  }

  /** Return the least upper bound of <code>a</code> and <code>b</code> */
  def lub2(exceptional: Boolean)(a: Elem, b: Elem): Elem

  /** Return the top element. */
  def top: Elem

  /** Return the bottom element. */
  def bottom: Elem

  /** Compute the least upper bound of a list of elements. */
  def lub(xs: List[Elem], exceptional: Boolean): Elem = try {
    if (xs == Nil) bottom else xs reduceLeft lub2(exceptional)
  } catch {
      case e: LubException =>
        Console.println("Lub on blocks: " + xs)
        throw e
  }
}
