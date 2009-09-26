/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

/** This extensible class may be used as a basis for implementing double
 *  linked lists. Type variable <code>A</code> refers to the element type
 *  of the list, type variable <code>This</code> is used to model self
 *  types of linked lists.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 *  @since   2.8
 */
trait DoubleLinkedListLike[A, This >: Null <: LinearSequence[A] with DoubleLinkedListLike[A, This]] extends LinkedListLike[A, This] { self =>

  var prev: This = _

  override def append(that: This): Unit =
    if (next eq null) {
      next = that
      if (that ne null) that.prev = repr
    } else
      next.append(that)

  override def insert(that: This): Unit = if (that ne null) {
    that.append(next)
    next = that
    that.prev = repr
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
