/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: SingleLinkedList.scala 16893 2009-01-13 13:09:22Z cunei $


package scala.collection.generic

/** This extensible class may be used as a basis for implementing linked
 *  list. Type variable <code>A</code> refers to the element type of the
 *  list, type variable <code>This</code> is used to model self types of
 *  linked lists.
 *  !!! todo: integrate with LinearSequence, need to drop null then.
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 */
trait LinkedListTemplate[A, This >: Null <: Sequence[A] with LinkedListTemplate[A, This]] extends SequenceTemplate[A, This] { self =>

  var elem: A = _
  var next: This = _

  override def isEmpty = false

  override def length: Int = 1 + (if (next eq null) 0 else next.length)

  override def head: A = elem

  override def tail: This = next

  def append(that: This): Unit =
    if (next eq null) next = that else next.append(that)

  def insert(that: This): Unit = if (that ne null) {
    that.append(next)
    next = that
  }

  override def drop(n: Int): This = {
    var i = 0
    var these: This = thisCollection
    while (i < n && (these ne null)) {
      these = these.next.asInstanceOf[This] // !!! concrete overrides abstract problem
      i += 1
    }
    these
  }

  override def apply(n: Int): A = {
    val loc = drop(n)
    if (loc ne null) loc.elem
    else throw new IndexOutOfBoundsException(n.toString)
  }

  def update(n: Int, x: A) {
    val loc = drop(n)
    if (loc ne null) loc.elem = x
    else throw new IndexOutOfBoundsException(n.toString)
  }

  def get(n: Int): Option[A] = {
    val loc = drop(n)
    if (loc ne null) Some(loc.elem)
    else None
  }

  override def elements: Iterator[A] = new Iterator[A] {
    var elems = self
    def hasNext = (elems ne null)
    def next = {
      val res = elems.elem
      elems = elems.next
      res;
    }
  }

  override def foreach[B](f: A => B): Unit = {
    var these = this
    while (these ne null) {
      f(these.elem);
      these = these.next
    }
  }
}
