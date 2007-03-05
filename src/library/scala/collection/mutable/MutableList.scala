/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


//import Predef.NoSuchElementException

/** This class is used internally to represent mutable lists. It is the
 *  basis for the implementation of the classes <code>Buffer</code>,
 *  <code>Stack</code>, and <code>Queue</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
trait MutableList[A] extends Seq[A] with PartialFunction[Int, A] {

  protected var first: LinkedList[A] = null
  protected var last: LinkedList[A] = null
  protected var len: Int = 0

  /** Returns the length of this list.
   */
  def length: Int = len

  /** Returns the <code>n</code>th element of this list. This method
   *  yields an error if the element does not exist.
   */
  def apply(n: Int): A = get(n) match {
    case None => throw new NoSuchElementException("element not found")
    case Some(value) => value
  }

  /** Returns the <code>n</code>th element of this list or <code>None</code>
   *  if this element does not exist.
   */
  def get(n: Int): Option[A] = first.get(n)

  protected def prependElem(elem: A): Unit = {
    first = new LinkedList[A](elem, first)
    if (len == 0)
      last = first
    len = len + 1
  }

  protected def appendElem(elem: A): Unit =
    if (len == 0)
      prependElem(elem)
    else {
      last.next = new LinkedList[A](elem, null)
      last = last.next
      len = len + 1
    }

  protected def reset() {
    first = null
    last = null
    len = 0
  }

  /** Returns an iterator over all elements of this list.
   */
  def elements: Iterator[A] =
    if (first eq null) Nil.elements else first.elements

  /** Returns an instance of <code>scala.List</code> containing the same
   *  sequence of elements.
   */
  override def toList: List[A] = if (first eq null) Nil else first.toList

  override protected def stringPrefix: String = "MutableList"
}
