/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode.analysis

/** A complete lattice.
 */
trait CompleteLattice {
  type Elem

  /** Return the least upper bound of <code>a</code> and <code>b</code> */
  def lub2(a: Elem, b: Elem): Elem

  /** Return the top element. */
  def top: Elem

  /** Return the bottom element. */
  def bottom: Elem

  /** Compute the least upper bound of a list of elements. */
  def lub(xs: List[Elem]): Elem = if (xs == Nil) bottom else xs reduceLeft lub2
}
