/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable

import scala.annotation.migration

/** This extensible class may be used as a basis for implementing double
 *  linked lists. Type variable `A` refers to the element type
 *  of the list, type variable `This` is used to model self
 *  types of linked lists.
 *
 *  The invariant of this data structure is that `prev` is always a reference to
 *  the previous node in the list. If `this` is the first node of the list, `prev`
 *  will be `null`.
 *  Field `next` is set to `this` iff the list is empty.
 *
 *  Examples (right arrow represents `next`, left arrow represents `prev`,
 *  `_` represents no value):
 *
 *  {{{
 *
 *     Empty:
 *
 *     null <-- [ _ ] --,
 *              [   ] <-`
 *
 *     Single element:
 *
 *     null <-- [ x ] --> [ _ ] --,
 *              [   ] <-- [   ] <-`
 *
 *     More elements:
 *
 *     null <-- [ x ] --> [ y ] --> [ z ] --> [ _ ] --,
 *              [   ] <-- [   ] <-- [   ] <-- [   ] <-`
 *
 *  }}}
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 *  @since   2.8
 *
 *  @tparam A    type of the elements contained in the double linked list
 *  @tparam This the type of the actual linked list holding the elements
 *
 *  @define Coll `DoubleLinkedList`
 *  @define coll double linked list
 */
@deprecated("low-level linked lists are deprecated due to idiosyncrasies in interface and incomplete features", "2.11.0")
trait DoubleLinkedListLike[A, This <: Seq[A] with DoubleLinkedListLike[A, This]] extends SeqLike[A, This] with LinkedListLike[A, This] { self =>

  /** A reference to the node in the linked list preceding the current node. */
  var prev: This = _

  // returns that list if this list is empty
  // otherwise modifies this list
  override def append(that: This): This =
    if (isEmpty)
      that
    else {
      if (next.isEmpty) {
        next = that
        if (that.nonEmpty) that.prev = repr
      } else {
        next.append(that)
      }
      repr
    }

  // cannot be called on empty lists
  override def insert(that: This): Unit = {
    super.insert(that)
    if (that.nonEmpty) that.prev = repr
  }

  /** Removes the current node from the double linked list.
   *  If the node was chained into a double linked list, it will no longer
   *  be a part of it.
   *  If the node was the last node in the list, i.e. a sentinel, this method
   *  does nothing.
   *
   *  '''Note:''' this method will not set the fields `elem`, `next` or `prev` of the
   *  current node, i.e. `this` node itself will still point "into" the list it
   *  was in.
   */
  @migration("Double linked list now removes the current node from the list.", "2.9.0")
  def remove(): Unit = if (nonEmpty) {
    next.prev = prev
    if (prev ne null) prev.next = next // because this could be the first node
  }

  private def atLocation[T](n: Int)(f: This => T)(onOutOfBounds: => T) = if (isEmpty) onOutOfBounds else {
    var loc = repr
    var left = n
    while (left > 0) {
      loc = loc.next
      left -= 1
      if (loc.isEmpty) onOutOfBounds
    }
    f(loc)
  }

  private def outofbounds(n: Int) = throw new IndexOutOfBoundsException(n.toString)

  override def drop(n: Int): This         = super[SeqLike].drop(n)
  override def tail                       = drop(1)
  override def apply(n: Int): A           = atLocation(n)(_.elem)(outofbounds(n))
  override def update(n: Int, x: A): Unit = atLocation(n)(_.elem = x)(outofbounds(n))
  override def get(n: Int): Option[A]     = atLocation[Option[A]](n)(x => Some(x.elem))(None)
}
