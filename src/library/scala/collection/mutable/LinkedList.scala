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

import generic._

/** This class implements single linked lists where both the head (<code>elem</code>)
 *  and the tail (<code>next</code>) are mutable.
 *
 *  @author Matthias Zenger
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   1
 */
@serializable @SerialVersionUID(-7308240733518833071L)
class LinkedList[A]() extends LinearSeq[A]
                         with GenericTraversableTemplate[A, LinkedList]
                         with LinkedListLike[A, LinkedList[A]] {
  next = this

  def this(elem: A, next: LinkedList[A]) {
    this()
    if (next != null) {
      this.elem = elem
      this.next = next
    }
  }

  override def companion: GenericCompanion[LinkedList] = LinkedList
}

object LinkedList extends SeqFactory[LinkedList] {

  override def empty[A]: LinkedList[A] = new LinkedList[A]

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LinkedList[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, LinkedList[A]] =
    (new MutableList) mapResult ((l: MutableList[A]) => l.toLinkedList)
}
