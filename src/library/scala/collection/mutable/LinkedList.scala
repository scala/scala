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

import generic._

/** This class implements single linked lists where both the head (<code>elem</code>)
 *  and the tail (<code>next</code>) are mutable.
 *
 *  @author Matthias Zenger
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   1
 */
@serializable
class LinkedList[A](_elem: A, _next: LinkedList[A]) extends LinearSequence[A]
                                                       with GenericTraversableTemplate[A, LinkedList]
                                                       with LinkedListLike[A, LinkedList[A]] {
  elem = _elem
  next = _next
  override def companion: GenericCompanion[LinkedList] = LinkedList
}

object LinkedList extends SequenceFactory[LinkedList] {
  implicit def builderFactory[A]: BuilderFactory[A, LinkedList[A], Coll] =
    new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, LinkedList[A]] =
    (new MutableList) mapResult ((l: MutableList[A]) => l.toLinkedList)
}
