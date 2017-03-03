/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package mutable

import scala.annotation.tailrec

/** This extensible class may be used as a basis for implementing linked
 *  list. Type variable `A` refers to the element type of the
 *  list, type variable `This` is used to model self types of
 *  linked lists.
 *
 *  $singleLinkedListExample
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 1.0, 08/07/2003
 *  @since   2.8
 *
 *  @tparam A    type of the elements contained in the linked list
 *  @tparam This the type of the actual linked list holding the elements
 *
 *  @define Coll `LinkedList`
 *  @define coll linked list
 *
 *  @define singleLinkedListExample
 *  If the list is empty `next` must be set to `this`. The last node in every
 *  mutable linked list is empty.
 *
 *  Examples (`_` represents no value):
 *
 *  {{{
 *
 *     Empty:
 *
 *     [ _ ] --,
 *     [   ] <-`
 *
 *     Single element:
 *
 *     [ x ] --> [ _ ] --,
 *               [   ] <-`
 *
 *     More elements:
 *
 *     [ x ] --> [ y ] --> [ z ] --> [ _ ] --,
 *                                   [   ] <-`
 *
 *  }}}
 */
@deprecated("low-level linked lists are deprecated due to idiosyncrasies in interface and incomplete features", "2.11.0")
trait LinkedListLike[A, This <: Seq[A] with LinkedListLike[A, This]] extends SeqLike[A, This] { self =>

  var elem: A = _
  var next: This = _

  override def isEmpty = next eq this

  /** Determines the length of this $coll by traversing and counting every
    * node.
    */
  override def length: Int = length0(repr, 0)

  @tailrec private def length0(elem: This, acc: Int): Int =
    if (elem.isEmpty) acc else length0(elem.next, acc + 1)

  override def head: A =
    if (isEmpty) throw new NoSuchElementException
    else elem

  override def tail: This = {
    require(nonEmpty, "tail of empty list")
    next
  }

  /** If `this` is empty then it does nothing and returns `that`. Otherwise, appends `that` to `this`. The append
   * requires a full traversal of `this`.
   *
   * Examples:
   *
   * {{{
   *      scala> val a = LinkedList(1, 2)
   *      a: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2)
   *
   *      scala> val b = LinkedList(1, 2)
   *      b: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2)
   *
   *      scala> a.append(b)
   *      res0: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2, 1, 2)
   *
   *      scala> println(a)
   *      LinkedList(1, 2, 1, 2)
   * }}}
   *
   * {{{
   *    scala> val a = new LinkedList[Int]()
   *    a: scala.collection.mutable.LinkedList[Int] = LinkedList()
   *
   *    scala> val b = LinkedList(1, 2)
   *    b: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2)
   *
   *    scala> val c = a.append(b)
   *    c: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2)
   *
   *    scala> println(a)
   *    LinkedList()
   * }}}
   *
   *  @return the list after append (this is the list itself if nonempty,
   *  or list `that` if list this is empty. )
   */
  def append(that: This): This = {
    @tailrec
    def loop(x: This) {
      if (x.next.isEmpty) x.next = that
      else loop(x.next)
    }
    if (isEmpty) that
    else { loop(repr); repr }
  }

  /** Insert linked list `that` at current position of this linked list
   *  @note this linked list must not be empty
   */
  def insert(that: This): Unit = {
    require(nonEmpty, "insert into empty list")
    if (that.nonEmpty) {
      that append next
      next = that
    }
  }

  override def drop(n: Int): This = {
    var i = 0
    var these: This = repr
    while (i < n && !these.isEmpty) {
      these = these.next
      i += 1
    }
    these
  }

  private def atLocation[T](n: Int)(f: This => T) = {
    val loc = drop(n)
    if (loc.nonEmpty) f(loc)
    else throw new IndexOutOfBoundsException(n.toString)
  }

  override def apply(n: Int): A   = atLocation(n)(_.elem)
  def update(n: Int, x: A): Unit  = atLocation(n)(_.elem = x)

  def get(n: Int): Option[A] = {
    val loc = drop(n)
    if (loc.nonEmpty) Some(loc.elem)
    else None
  }

  override def iterator: Iterator[A] = new AbstractIterator[A] {
    var elems = self
    def hasNext = elems.nonEmpty
    def next = {
      val res = elems.elem
      elems = elems.next
      res
    }
  }

  override def foreach[U](f: A => U) {
    var these = this
    while (these.nonEmpty) {
      f(these.elem)
      these = these.next
    }
  }

  /** Return a clone of this list.
   *
   *  @return a `LinkedList` with the same elements.
   */
  override def clone(): This = {
    val bf = newBuilder
    bf ++= this
    bf.result()
  }
}
