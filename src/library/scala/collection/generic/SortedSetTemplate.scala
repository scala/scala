/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: SortedSet.scala 16893 2009-01-13 13:09:22Z cunei $
// !!! todo: integrate in new collections library

package scala.collection.generic

/** A template for sets which are sorted.
 *
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 */
trait SortedSetTemplate[A, +This <: SortedSet[A] with SortedSetTemplate[A, This]] extends Sorted[A, This] with SetTemplate[A, This] {
self =>

  override def keySet = thisCollection

  override def firstKey: A = head
  override def lastKey: A = last

  def rangeImpl(from: Option[A], until: Option[A]): This

  override def from(from: A): This = rangeImpl(Some(from), None)
  override def until(until: A): This = rangeImpl(None, Some(until))
  override def range(from: A, until: A): This = rangeImpl(Some(from), Some(until))

  override def subsetOf(that: Set[A]): Boolean = that match {
    case that: SortedSet[_] => that.hasAll(elements)
    case that => super.subsetOf(that)
  }
}
