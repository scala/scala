/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** Any collection (including maps) whose keys (or elements) are ordered.
 *
 *  @author Sean McDirmid
 */
trait Sorted[K,A] extends MutableIterable[A] {
  protected type SortedSelf <: Sorted[K,A];

  /** Returns the first key of the collection. */
  def first: K;

  /** Returns the last key of the collection. */
  def last: K;

  /** Comparison function that orders keys. */
  def compare(k0: K, k1: K): Int;

  /** Creates a ranged projection of this collection. Any mutations in the
   *  ranged projection will update this collection and vice versa.
   *
   *  @param from  The lower-bound (inclusive) of the ranged projection.
   *               <code>None</code> if there is no lower bound.
   *  @param until The upper-bound (exclusive) of the ranged projection.
   *               <code>None</code> if there is no upper bound.
   */
  def rangeImpl(from: Option[K], until: Option[K]) : SortedSelf;

  /** Creates a ranged projection of this collection with no upper-bound.
   ** @param from The lower-bound (inclusive) of the ranged projection.
   **/
  final def from(from: K): SortedSelf = rangeImpl(Some(from), None);

  /** Creates a ranged projection of this collection with no lower-bound.
   ** @param from The upper-bound (exclusive) of the ranged projection.
   **/
  final def until(until: K): SortedSelf = rangeImpl(None, Some(until));

  /** Creates a ranged projection of this collection with both a lower-bound and an upper-bound.
   ** @param from The upper-bound (exclusive) of the ranged projection.
   **/
  final def range(from: K, until: K) : SortedSelf = rangeImpl(Some(from),Some(until));

  /** A wrapper around Java comparators. */
  protected class Comparator[K <% Ordered[K]] extends java.util.Comparator {
    def compare(x0 : Any, x1 : Any) = {
      val k0 = x0.asInstanceOf[K];
      val k1 = x1.asInstanceOf[K];
      k0.compare(k1);
    }
  }
}
