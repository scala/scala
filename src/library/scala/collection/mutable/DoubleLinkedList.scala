/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


/** This extensible class may be used as a basis for implementing double
 *  linked lists. Type variable <code>A</code> refers to the element type
 *  of the list, type variable <code>This</code> is used to model self
 *  types of linked lists.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
abstract class DoubleLinkedList[A, This >: Null <: DoubleLinkedList[A, This]]
         extends SingleLinkedList[A, This]
{ self: This =>

  var prev: This

  override def append(that: This): Unit =
    if (that eq null)
      ()
    else if (next eq null) {
      next = that
      that.prev = this
    } else
      next.append(that)

  override def insert(that: This): Unit = if (that ne null) {
    that.append(next)
    next = that
    that.prev = this
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
