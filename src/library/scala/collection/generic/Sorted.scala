/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Sorted.scala 17537 2009-04-20 18:37:37Z odersky $

package scala.collection.generic

/** Any collection (including maps) whose keys (or elements) are ordered.
 *
 *  @author Sean McDirmid
 */
trait Sorted[K, +This <: Sorted[K, This]] extends Ranged[K, This] {

  /** The current collection */
  protected def thisCollection: This

  /** return as a projection the set of keys in this collection */
  def keySet: SortedSet[K]

  /** Create a range projection of this collection with no lower-bound.
   *  @param to The upper-bound (inclusive) of the ranged projection.
   */
  def to(to: K): This = {
    // tough!
    val i = keySet.from(to).iterator;
    if (!i.hasNext) return thisCollection
    val next = i.next;
    if (next == to) {
      if (!i.hasNext) return thisCollection
      else return until(i.next)
    } else return until(next)
  }

  protected def hasAll(j: Iterator[K]): Boolean = {
    val i = keySet.iterator;
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
    true
  }

}
