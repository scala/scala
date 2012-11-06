/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection
import generic._

/** A template for sets which are sorted.
 *
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait SortedSetLike[A, +This <: SortedSet[A] with SortedSetLike[A, This]] extends Sorted[A, This] with SetLike[A, This] {
self =>

  implicit def ordering: Ordering[A]

  override def keySet = repr

  override def firstKey: A = head
  override def lastKey: A = last

  def rangeImpl(from: Option[A], until: Option[A]): This

  override def from(from: A): This = rangeImpl(Some(from), None)
  override def until(until: A): This = rangeImpl(None, Some(until))
  override def range(from: A, until: A): This = rangeImpl(Some(from), Some(until))

  override def subsetOf(that: GenSet[A]): Boolean = that match {
    // TODO: It may actually be pretty rare that the guard here ever
    // passes. Is this really worth keeping? If it is, we should add
    // more sensible implementations of == to Ordering.
    case that: SortedSet[_] if that.ordering == ordering => that.hasAll(this.iterator)
    case that => super.subsetOf(that)
  }
}
