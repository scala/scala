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
trait Sorted[K,A] extends scala.collection.Sorted[K,A] with Ranged[K,A] {
  override protected type SortedSelf <: Sorted[K,A];
  /** return as a projection the set of keys in this collection */
  override def keySet : SortedSet[K];

  /** Creates a ranged projection of this collection. Any mutations in the
   *  ranged projection will update this collection and vice versa.  Keys
   *  are garuanteed to be consistent between the collection and its projection.
   *
   *  @param from  The lower-bound (inclusive) of the ranged projection.
   *               <code>None</code> if there is no lower bound.
   *  @param until The upper-bound (exclusive) of the ranged projection.
   *               <code>None</code> if there is no upper bound.
   */
  override def rangeImpl(from: Option[K], until: Option[K]) : SortedSelf;

  /** Create a range projection of this collection with no lower-bound.
   ** @param to The upper-bound (inclusive) of the ranged projection.
   **/
  final override def to(to : K): SortedSelf = {
    // tough!
    val i = keySet.from(to).elements;
    if (!i.hasNext) return this.asInstanceOf[SortedSelf];
    val next = i.next;
    if (next == to) {
      if (!i.hasNext) return this.asInstanceOf[SortedSelf];
      else return until(i.next);
    } else return until(next);
  }
}
