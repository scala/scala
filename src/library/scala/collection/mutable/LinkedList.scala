/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package mutable

import generic._

/** This class implements single linked lists where both the head (`elem`)
 *  and the tail (`next`) are mutable.
 *
 *  @author Matthias Zenger
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   1
 *
 *  @tparam A     the type of the elements contained in this linked list.
 *
 *  @define Coll LinkedList
 *  @define coll linked list
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is always `LinkedList[B]` because an implicit of type `CanBuildFrom[LinkedList, B, LinkedList[B]]`
 *    is defined in object `LinkedList`.
 *  @define $bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`. This is usually the `canBuildFrom` value
 *    defined in object `LinkedList`.
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(-7308240733518833071L)
class LinkedList[A]() extends LinearSeq[A]
                         with GenericTraversableTemplate[A, LinkedList]
                         with LinkedListLike[A, LinkedList[A]]
                         with Serializable {
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

/** $factoryInfo
 *  @define Coll LinkedList
 *  @define coll linked list
 */
object LinkedList extends SeqFactory[LinkedList] {
  override def empty[A]: LinkedList[A] = new LinkedList[A]
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LinkedList[A]] = new GenericCanBuildFrom[A]

  def newBuilder[A]: Builder[A, LinkedList[A]] =
    (new MutableList) mapResult ((l: MutableList[A]) => l.toLinkedList)
}
