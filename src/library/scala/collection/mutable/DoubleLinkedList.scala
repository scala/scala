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
class DoubleLinkedList[A]/*(_elem: A, _next: DoubleLinkedList[A])*/ extends LinearSequence[A] with DoubleLinkedListTemplate[A, DoubleLinkedList[A]] {
  override protected[this] def newBuilder = DoubleLinkedList.newBuilder
  override def traversableBuilder[B]: Builder[B, DoubleLinkedList[B], Any] = DoubleLinkedList.newBuilder[B]
}

object DoubleLinkedList extends SequenceFactory[DoubleLinkedList] {
  type Coll = DoubleLinkedList[_]
  implicit def builderFactory[A]: BuilderFactory[A, DoubleLinkedList[A], Coll] = new BuilderFactory[A, DoubleLinkedList[A], Coll] { def apply(from: Coll) = from.traversableBuilder[A] }
  def newBuilder[A]: Builder[A, DoubleLinkedList[A], Any] = null // !!!
}



