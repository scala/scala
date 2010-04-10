/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

/** This extensible class may be used as a basis for implementing double
 *  linked lists. Type variable `A` refers to the element type
 *  of the list, type variable `This` is used to model self
 *  types of linked lists.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 *  @since   2.8
 *
 *  @tparam A    type of the elements contained in the double linked list
 *  @tparam This the type of the actual linked list holding the elements
 *
 *  @define Coll DoubleLinkedList
 *  @define coll double linked list
 */
trait DoubleLinkedListLike[A, This <: Seq[A] with DoubleLinkedListLike[A, This]] extends LinkedListLike[A, This] { self =>

  /** A reference to the node in the linked list preceeding the current node. */
  var prev: This = _

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

  override def insert(that: This): Unit = {
    super.insert(that)
    if (that.nonEmpty) that.prev = repr
  }

  def remove() {
    if (next.nonEmpty)
      next.prev = prev
    if (prev.nonEmpty)
      prev.next = next
  }
}
