/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection

/** Analogous to a Java sorted set.
 *
 *  @author Sean McDirmid
 */
trait SortedSet[A] extends Set[A] with Sorted[A, A] {

  override def keySet = this

  override def firstKey: A = {
    val i = elements
    if (i.hasNext) i.next
    else throw new NoSuchElementException
  }

  override def lastKey: A = {
    var last: A = null.asInstanceOf[A]
    val i = elements
    while (i.hasNext) last = i.next;
    if (last == null) throw new NoSuchElementException
    else last
  }

  override def rangeImpl(from: Option[A], until: Option[A]): SortedSet[A]

  override def from(from: A) = rangeImpl(Some(from), None)

  override def until(until: A) = rangeImpl(None, Some(until))

  override def range(from: A, until: A) = rangeImpl(Some(from),Some(until))

  override def subsetOf(that: Set[A]): Boolean = that match {
    case that: SortedSet[_] => that.hasAll(elements)
    case that => super.subsetOf(that)
  }

}
