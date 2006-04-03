package scala.tools.nsc.backend.icode.analysis;

/** A complete lattice.
 */
trait CompleteLattice {
	type Elem;

  /** Return the least upper bound of `a' and `b' */
  def lub2(a: Elem, b: Elem): Elem;

  /** Return the top element. */
  def top: Elem;

  /** Return the bottom element. */
  def bottom: Elem;

  /** Compute the least upper bound of a list of elements. */
  def lub(xs: List[Elem]): Elem =  xs reduceLeft lub2;
}
