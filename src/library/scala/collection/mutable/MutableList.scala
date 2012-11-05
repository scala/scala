/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package mutable

import generic._
import immutable.{List, Nil}

// !!! todo: convert to LinkedListBuffer?
/**
 *  This class is used internally to represent mutable lists. It is the
 *  basis for the implementation of the class `Queue`.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   1
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#mutable_lists "Scala's Collection Library overview"]]
 *  section on `Mutable Lists` for more information.
 */
@SerialVersionUID(5938451523372603072L)
class MutableList[A]
extends AbstractSeq[A]
   with LinearSeq[A]
   with LinearSeqOptimized[A, MutableList[A]]
   with GenericTraversableTemplate[A, MutableList]
   with Builder[A, MutableList[A]]
   with Serializable
{
  override def companion: GenericCompanion[MutableList] = MutableList

  override protected[this] def newBuilder: Builder[A, MutableList[A]] = new MutableList[A]

  protected var first0: LinkedList[A] = new LinkedList[A]
  protected var last0: LinkedList[A] = first0
  protected var len: Int = 0

  def toQueue = new Queue(first0, last0, len)

  /** Is the list empty?
   */
  override def isEmpty = len == 0

  /** Returns the first element in this list
   */
  override def head: A = if (nonEmpty) first0.head else throw new NoSuchElementException

  /** Returns the rest of this list
   */
  override def tail: MutableList[A] = {
    require(nonEmpty, "tail of empty list")
    val tl = new MutableList[A]
    tl.first0 = first0.tail
    tl.last0 = last0
    tl.len = len - 1
    tl
  }

  /** Prepends a single element to this list. This operation takes constant
   *  time.
   *  @param elem  the element to prepend.
   *  @return   this $coll.
   */
  def +=: (elem: A): this.type = { prependElem(elem); this }

  /** Returns the length of this list.
   */
  override def length: Int = len

  /** Returns the `n`-th element of this list.
   *  @throws IndexOutOfBoundsException if index does not exist.
   */
  override def apply(n: Int): A = first0.apply(n)

  /** Updates the `n`-th element of this list to a new value.
   *  @throws IndexOutOfBoundsException if index does not exist.
   */
  def update(n: Int, x: A): Unit = first0.update(n, x)

  /** Returns the `n`-th element of this list or `None`
   *  if index does not exist.
   */
  def get(n: Int): Option[A] = first0.get(n)

  protected def prependElem(elem: A) {
    first0 = new LinkedList[A](elem, first0)
    if (len == 0) last0 = first0
    len = len + 1
  }

  protected def appendElem(elem: A) {
    if (len == 0) {
      prependElem(elem)
    } else {
      last0.next = new LinkedList[A]
      last0 = last0.next
      last0.elem = elem
      last0.next = new LinkedList[A] // for performance, use sentinel `object` instead?
      len = len + 1
    }
  }

  /** Returns an iterator over all elements of this list.
   */
  override def iterator: Iterator[A] = first0.iterator

  override def last = {
    if (isEmpty) throw new NoSuchElementException("MutableList.empty.last")
    last0.elem
  }

  /** Returns an instance of [[scala.List]] containing the same
   *  sequence of elements.
   */
  override def toList: List[A] = first0.toList

  /** Returns the current list of elements as a linked List
   *  sequence of elements.
   */
  private[mutable] def toLinkedList: LinkedList[A] = first0

  /** Appends a single element to this buffer. This takes constant time.
   *
   *  @param elem  the element to append.
   */
  def +=(elem: A): this.type = { appendElem(elem); this }

  def clear() {
    first0 = new LinkedList[A]
    last0 = first0
    len = 0
  }

  def result = this

  override def clone(): MutableList[A]  = {
    val bf = newBuilder
    bf ++= seq
    bf.result
  }

}


object MutableList extends SeqFactory[MutableList] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, MutableList[A]] =
    ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]

  def newBuilder[A]: Builder[A, MutableList[A]] = new MutableList[A]
}
