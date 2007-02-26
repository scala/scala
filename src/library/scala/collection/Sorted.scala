/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection;

/** Any collection (including maps) whose keys (or elements) are ordered.
 *
 *  @author Sean McDirmid
 */
trait Sorted[K,+A] extends Ranged[K,A] {
  /** return as a projection the set of keys in this collection */
  def keySet : SortedSet[K];

  /** Creates a ranged projection of this collection. Any mutations in the
   *  ranged projection will update this collection and vice versa.  Keys
   *  are garuanteed to be consistent between the collection and its projection.
   *
   *  @param from  The lower-bound (inclusive) of the ranged projection.
   *               <code>None</code> if there is no lower bound.
   *  @param until The upper-bound (exclusive) of the ranged projection.
   *               <code>None</code> if there is no upper bound.
   */
  override def rangeImpl(from: Option[K], until: Option[K]) : Sorted[K,A];
  override def from(from: K) = rangeImpl(Some(from), None);
  override def until(until: K) = rangeImpl(None, Some(until));
  override def range(from: K, until: K) = rangeImpl(Some(from),Some(until));

  /** Create a range projection of this collection with no lower-bound.
   ** @param to The upper-bound (inclusive) of the ranged projection.
   **/
  def to(to : K): Sorted[K,A] = {
    // tough!
    val i = keySet.from(to).elements;
    if (!i.hasNext) return this;
    val next = i.next;
    if (next == to) {
      if (!i.hasNext) return this;
      else return until(i.next);
    } else return until(next);
  }
  protected def hasAll(j : Iterator[K]) : Boolean = {
    val i = keySet.elements;
    if (!i.hasNext) return !j.hasNext;
    var in = i.next;
    while (j.hasNext) {
      val jn = j.next;
      while ({
        val n = compare(jn, in);
        if (n == 0) false;
        else if (n < 0) return false;
        else if (!i.hasNext) return false;
        else true;
      }) in = i.next;
    }
    return true;
  }
}
