/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.util

class ListBuffer[T] extends Iterator[T] {

  private var first = new LinkedList[T]
  private var limit = first

  def +=(x: T): unit = {
    limit.elem = x
    limit.next = new LinkedList[T]
    limit = limit.next
  }

  def ++=(xs: Iterable[T]): unit =
    for (val x <- xs.elements) +=(x)

  def +(x: T): ListBuffer[T] = { +=(x); this }
  def ++(xs: Iterable[T]): ListBuffer[T] = { ++=(xs); this }

  def hasNext: boolean =
    first != limit

  def next: T = {
    assert(hasNext)
    val x = first.elem
    first = first.next
    x
  }

  def elements: Iterator[T] = new Iterator[T] {
    var first = ListBuffer.this.first

    def hasNext: boolean =
      first != limit

    def next: T = {
      assert(hasNext)
      val x = first.elem
      first = first.next
      x
    }
  }

  def clear: unit = { first = limit }

  /** override for efficiency */
  override def toList: List[T] = {
    def mkList(p: LinkedList[T]): List[T] =
      if (p == limit) List() else p.elem :: mkList(p.next)
    mkList(first)
  }

  override def toString(): String = toList.mkString("", ",", "")
}
