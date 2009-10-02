/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._
import annotation.tailrec

/** This extensible class may be used as a basis for implementing linked
 *  list. Type variable <code>A</code> refers to the element type of the
 *  list, type variable <code>This</code> is used to model self types of
 *  linked lists.
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait LinkedListLike[A, This >: Null <: Seq[A] with LinkedListLike[A, This]] extends SeqLike[A, This] { self =>

  var elem: A = _
  var next: This = _

  override def isEmpty = false

  override def length: Int = 1 + (if (next eq null) 0 else next.length)

  override def head: A    = elem
  override def tail: This = next

  def append(that: This): Unit = {
    @tailrec
    def loop(x: This): Unit = {
      if (x.next eq null) x.next = that
      else loop(x.next)
    }
    loop(repr)
  }

  def insert(that: This): Unit = if (that ne null) {
    that.append(next)
    next = that
  }

  override def drop(n: Int): This = {
    var i = 0
    var these: This = repr
    while (i < n && (these ne null)) {
      these = these.next.asInstanceOf[This] // !!! concrete overrides abstract problem
      i += 1
    }
    these
  }
  private def atLocation[T](n: Int)(f: This => T) = {
    val loc = drop(n)
    if (loc ne null) f(loc)
    else throw new IndexOutOfBoundsException(n.toString)
  }

  override def apply(n: Int): A   = atLocation(n)(_.elem)
  def update(n: Int, x: A): Unit  = atLocation(n)(_.elem = x)

  def get(n: Int): Option[A] = {
    val loc = drop(n)
    if (loc ne null) Some(loc.elem)
    else None
  }

  override def iterator: Iterator[A] = new Iterator[A] {
    var elems = self
    def hasNext = (elems ne null)
    def next = {
      val res = elems.elem
      elems = elems.next
      res;
    }
  }

  override def foreach[B](f: A => B) {
    var these = this
    while (these ne null) {
      f(these.elem);
      these = these.next
    }
  }
}
