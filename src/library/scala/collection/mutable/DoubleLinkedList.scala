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

import generic._

/** This class implements double linked lists where both the head (`elem`),
 *  the tail (`next`) and a reference to the previous node (`prev`) are mutable.
 *
 *  @author Matthias Zenger
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   1
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#double_linked_lists "Scala's Collection Library overview"]]
 *  section on `Double Linked Lists` for more information.

 *
 *  @tparam A     the type of the elements contained in this double linked list.
 *
 *  @define Coll `DoubleLinkedList`
 *  @define coll double linked list
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is always `DoubleLinkedList[B]` because an implicit of type `CanBuildFrom[DoubleLinkedList, B, DoubleLinkedList[B]]`
 *    is defined in object `DoubleLinkedList`.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`. This is usually the `canBuildFrom` value
 *    defined in object `DoubleLinkedList`.
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@deprecated("low-level linked lists are deprecated due to idiosyncrasies in interface and incomplete features", "2.11.0")
@SerialVersionUID(-8144992287952814767L)
class DoubleLinkedList[A]() extends AbstractSeq[A]
                            with LinearSeq[A]
                            with GenericTraversableTemplate[A, DoubleLinkedList]
                            with DoubleLinkedListLike[A, DoubleLinkedList[A]]
                            with Serializable {
  next = this

  /** Creates a node for the double linked list.
   *
   *  @param elem    the element this node contains.
   *  @param next    the next node in the double linked list.
   */
  def this(elem: A, next: DoubleLinkedList[A]) {
    this()
    if (next != null) {
      this.elem = elem
      this.next = next
      this.next.prev = this
    }
  }

  override def companion: GenericCompanion[DoubleLinkedList] = DoubleLinkedList

  // Accurately clone this collection.  See scala/bug#6296
  override def clone(): DoubleLinkedList[A] = {
    val builder = newBuilder
    builder ++= this
    builder.result()
  }
}

/** $factoryInfo
 *  @define coll double linked list
 *  @define Coll `DoubleLinkedList`
 */
@deprecated("low-level linked lists are deprecated", "2.11.0")
object DoubleLinkedList extends SeqFactory[DoubleLinkedList] {
  /** $genericCanBuildFromInfo */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, DoubleLinkedList[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]

  def newBuilder[A]: Builder[A, DoubleLinkedList[A]] =
    new Builder[A, DoubleLinkedList[A]] {
      def emptyList() = new DoubleLinkedList[A]()
      var current = emptyList()

      def +=(elem: A): this.type = {
        if (current.isEmpty)
          current = new DoubleLinkedList(elem, emptyList())
        else
          current append new DoubleLinkedList(elem, emptyList())

        this
      }

      def clear(): Unit = current = emptyList()
      def result() = current
    }
}
