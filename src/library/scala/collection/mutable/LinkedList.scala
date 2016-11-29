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

/** A more traditional/primitive style of linked list where the "list" is also the "head" link. Links can be manually
  * created and manipulated, though the use of the API, when possible, is recommended.
  *
  * The danger of directly manipulating next:
  * {{{
  *     scala> val b = LinkedList(1)
  *     b: scala.collection.mutable.LinkedList[Int] = LinkedList(1)
  *
  *     scala> b.next = null
  *
  *     scala> println(b)
  *     java.lang.NullPointerException
  * }}}
  *
  *  $singleLinkedListExample
  *
  *  @author Matthias Zenger
  *  @author Martin Odersky
  *  @version 2.8
  *  @since   1
  *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#linked_lists "Scala's Collection Library overview"]]
  *  section on `Linked Lists` for more information.
  *
  *  @tparam A     the type of the elements contained in this linked list.
  *
  *  @constructor Creates an "empty" list, defined as a single node with no data element and next pointing to itself.

  *  @define Coll `LinkedList`
  *  @define coll linked list
  *  @define thatinfo the class of the returned collection. In the standard library configuration,
  *    `That` is always `LinkedList[B]` because an implicit of type `CanBuildFrom[LinkedList, B, LinkedList[B]]`
  *    is defined in object `LinkedList`.
  *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
  *    result class `That` from the current representation type `Repr`
  *    and the new element type `B`. This is usually the `canBuildFrom` value
  *    defined in object `LinkedList`.
  *  @define orderDependent
  *  @define orderDependentFold
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  *  @define collectExample Example:
  *  {{{
  *    scala>     val a = LinkedList(1, 2, 3)
  *    a: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2, 3)
  *
  *    scala>     val addOne: PartialFunction[Any, Float] = {case i: Int => i + 1.0f}
  *    addOne: PartialFunction[Any,Float] = <function1>
  *
  *    scala>     val b = a.collect(addOne)
  *    b: scala.collection.mutable.LinkedList[Float] = LinkedList(2.0, 3.0, 4.0)
  *
  *    scala> val c = LinkedList('a')
  *    c: scala.collection.mutable.LinkedList[Char] = LinkedList(a)
  *
  *    scala> val d = a ++ c
  *    d: scala.collection.mutable.LinkedList[AnyVal] = LinkedList(1, 2, 3, a)
  *
  *    scala> val e = d.collect(addOne)
  *    e: scala.collection.mutable.LinkedList[Float] = LinkedList(2.0, 3.0, 4.0)
  *  }}}
  */
@SerialVersionUID(-7308240733518833071L)
@deprecated("low-level linked lists are deprecated due to idiosyncrasies in interface and incomplete features", "2.11.0")
class LinkedList[A]() extends AbstractSeq[A]
                         with LinearSeq[A]
                         with GenericTraversableTemplate[A, LinkedList]
                         with LinkedListLike[A, LinkedList[A]]
                         with Serializable {
  next = this

  /** Creates a new list. If the parameter next is null, the result is an empty list. Otherwise, the result is
   * a list with elem at the head, followed by the contents of next.
   *
   * Note that next is part of the new list, as opposed to the +: operator,
   * which makes a new copy of the original list.
   *
   * @example
   * {{{
   *     scala> val m = LinkedList(1)
   *     m: scala.collection.mutable.LinkedList[Int] = LinkedList(1)
   *
   *     scala> val n = new LinkedList[Int](2, m)
   *     n: scala.collection.mutable.LinkedList[Int] = LinkedList(2, 1)
   * }}}
   */
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
 *  @define Coll `LinkedList`
 *  @define coll linked list
 */
@deprecated("low-level linked lists are deprecated", "2.11.0")
object LinkedList extends SeqFactory[LinkedList] {
  override def empty[A]: LinkedList[A] = new LinkedList[A]
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LinkedList[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]

  def newBuilder[A]: Builder[A, LinkedList[A]] =
    (new MutableList) mapResult ((l: MutableList[A]) => l.toLinkedList)
}
