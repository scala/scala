/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: DoubleLinkedList.scala 16893 2009-01-13 13:09:22Z cunei $


package scala.collection.generic

/** This extensible class may be used as a basis for implementing double
 *  linked lists. Type variable <code>A</code> refers to the element type
 *  of the list, type variable <code>This</code> is used to model self
 *  types of linked lists.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
trait DoubleLinkedListTemplate[A, This >: Null <: LinearSequence[A] with DoubleLinkedListTemplate[A, This]] extends LinkedListTemplate[A, This] { self =>

  var prev: This = _

  override def append(that: This): Unit =
    if (next eq null) {
      next = that
      if (that ne null) that.prev = thisCollection
    } else
      next.append(that)

  override def insert(that: This): Unit = if (that ne null) {
    that.append(next)
    next = that
    that.prev = thisCollection
  }

  def remove() {
    if (next ne null)
      next.prev = prev
    if (prev ne null)
      prev.next = next
    prev = null
    next = null
  }
}
