/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode.analysis

/** A complete lattice.
 */
trait CompleteLattice {
  type Elem <: AbstractState[A, B] forSome {type A; type B;}

  /** Abstract states for icode. Pairs stack and local variable state
   *  Equality is structural, except for the <code>bottom</code> value.
   */
  abstract class AbstractState[+A, +B](val stack: A, val vars: B) {
    override def equals(other: Any): Boolean = other match {
      case that: AbstractState[_, _] =>
        if ((that eq bottom) || (this eq bottom)) this eq that
        else (stack == that.stack && vars == that.vars)
      case _ => false
    }
  }

  /** Return the least upper bound of <code>a</code> and <code>b</code> */
  def lub2(a: Elem, b: Elem): Elem

  /** Return the top element. */
  def top: Elem

  /** Return the bottom element. */
  def bottom: Elem

  /** Compute the least upper bound of a list of elements. */
  def lub(xs: List[Elem]): Elem = try {
    if (xs == Nil) bottom else xs reduceLeft lub2
  } catch {
      case e: LubError =>
        Console.println("Lub on blocks: " + xs)
        throw e
  }
}
