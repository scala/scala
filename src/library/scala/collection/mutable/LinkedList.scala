/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

import generic._

/** This class implements single linked lists where both the head (<code>elem</code>)
 *  and the tail (<code>next</code>) are mutable.
 *
 *  @author Matthias Zenger
 *  @author Martin Odersky
 *  @version 2.8
 */
@serializable
class LinkedList[A](_elem: A, _next: LinkedList[A]) extends LinearSequence[A] with LinkedListTemplate[A, LinkedList[A]] {
  elem = _elem
  next = _next
  override protected[this] def newBuilder = LinkedList.newBuilder
  override def traversibleBuilder[B]: Builder[B, LinkedList[B], Any] = LinkedList.newBuilder[B]
}

object LinkedList extends SequenceFactory[LinkedList] {
  type Coll = LinkedList[_]
  implicit def builderFactory[A]: BuilderFactory[A, LinkedList[A], Coll] = new BuilderFactory[A, LinkedList[A], Coll] { def apply(from: Coll) = from.traversibleBuilder[A] }
  def newBuilder[A]: Builder[A, LinkedList[A], Any] = (new MutableList) mapResult ((l: MutableList[A]) => l.toLinkedList)
}



