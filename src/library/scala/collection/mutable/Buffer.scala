/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


import Predef._

/** Buffers are used to create sequences of elements incrementally by
 *  appending, prepending, or inserting new elements. It is also
 *  possible to access and modify elements in a random access fashion
 *  via the index of the element in the current sequence.
 *
 *  @author  Matthias Zenger
 *  @version 1.1, 02/03/2004
 */
[cloneable]
trait Buffer[A] extends AnyRef
      with Seq[A]
      with Scriptable[Message[Pair[Location, A]]]
{

  /** Append a single element to this buffer and return
   *  the identity of the buffer.
   *
   *  @param elem  the element to append.
   */
  def +(elem: A): Buffer[A] = {
    this += elem
    this
  }

  /** Append a single element to this buffer.
   *
   *  @param elem  the element to append.
   */
  def +=(elem: A): Unit

  /** Appends a number of elements provided by an iterable object
   *  via its <code>elements</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter  the iterable object.
   *  @return      the updated buffer.
   */
  def ++(elems: Iterable[A]): Buffer[A] = this ++ elems.elements

  /** Appends a number of elements provided by an iterable object
   *  via its <code>elements</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter  the iterable object.
   *  @return      the updated buffer.
   */
  def ++(iter: Iterator[A]): Buffer[A] = {
    iter.foreach(e => this += e)
    this
  }

  /** Appends a number of elements provided by an iterable object
   *  via its <code>elements</code> method.
   *
   *  @param iter  the iterable object.
   */
  def ++=(elems: Iterable[A]): Unit = this ++ elems.elements

  /** Appends a number of elements provided by an iterable object
   *  via its <code>elements</code> method.
   *
   *  @param iter  the iterable object.
   */
  def ++=(it: Iterator[A]): Unit = this ++ it

  /** Appends a sequence of elements to this buffer.
   *
   *  @param elems  the elements to append.
   */
  def append(elems: A*): Unit = this ++ elems

  /** Appends a number of elements provided by an iterable object
   *  via its <code>elements</code> method.
   *
   *  @param iter  the iterable object.
   */
  def appendAll(iter: Iterable[A]): Unit = this ++ iter

  /** Prepend a single element to this buffer and return
   *  the identity of the buffer.
   *
   *  @param elem  the element to append.
   */
  def +:(elem: A): Buffer[A]

  /** Removes a single element from this buffer, at its first occurrence.
   *  If the list does not contain that element, it is unchanged
   *
   *  @param x  the element to remove.
   */
  def -= (x: A): unit = {
    val i = indexOf(x)
    if(i != -1) remove(i)
  }

  /** Prepends a number of elements provided by an iterable object
   *  via its <code>elements</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter  the iterable object.
   */
  def ++:(iter: Iterable[A]): Buffer[A] = {
    iter.elements.toList.reverse.foreach(e => e +: this)
    this
  }

  /** Prepend an element to this list.
   *
   *  @param elem  the element to prepend.
   */
  def prepend(elems: A*): Unit = elems ++: this

  /** Prepends a number of elements provided by an iterable object
   *  via its <code>elements</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter  the iterable object.
   */
  def prependAll(elems: Iterable[A]): Unit = elems ++: this

  /** Inserts new elements at the index <code>n</code>. Opposed to method
   *  <code>update</code>, this method will not replace an element with a
   *  one. Instead, it will insert the new elements at index <code>n</code>.
   *
   *  @param n      the index where a new element will be inserted.
   *  @param elems  the new elements to insert.
   */
  def insert(n: Int, elems: A*): Unit = insertAll(n, elems)

  /** Inserts new elements at the index <code>n</code>. Opposed to method
   *  <code>update</code>, this method will not replace an element with a
   *  one. Instead, it will insert a new element at index <code>n</code>.
   *
   *  @param n     the index where a new element will be inserted.
   *  @param iter  the iterable object providing all elements to insert.
   */
  def insertAll(n: Int, iter: Iterable[A]): Unit

  /** Replace element at index <code>n</code> with the new element
   *  <code>newelem</code>.
   *
   *  @param n       the index of the element to replace.
   *  @param newelem the new element.
   */
  def update(n: Int, newelem: A): Unit

  /** Removes the element on a given index position.
   *
   *  @param n  the index which refers to the element to delete.
   */
  def remove(n: Int): A

  /** Removes the first <code>n</code> elements.
   *
   *  @param n  the number of elements to remove from the beginning
   *            of this buffer.
   */
  def trimStart(n: Int): Unit = {
    var i = n
    while (i > 0) { remove(0); i = i - 1 }
  }

  /** Removes the last <code>n</code> elements.
   *
   *  @param n  the number of elements to remove from the end
   *            of this buffer.
   */
  def trimEnd(n: Int): Unit = {
    var i = n
    while (i > 0) { remove(length - 1); i = i - 1 }
  }

  /** Clears the buffer contents.
   */
  def clear: Unit

  /** Send a message to this scriptable object.
   *
   *  @param cmd  the message to send.
   */
  def <<(cmd: Message[Pair[Location, A]]): Unit = cmd match {
    case Include(Pair(l, elem)) => l match {
      case Start => prepend(elem)
      case End => append(elem)
      case Index(n) => insert(n, elem)
      case _ => error("message " + cmd + " not understood")
    }
    case Update(Pair(l, elem)) => l match {
      case Start => update(0, elem)
      case End => update(length - 1, elem)
      case Index(n) => update(n, elem)
      case _ => error("message " + cmd + " not understood")
    }
    case Remove(Pair(l, _)) => l match {
      case Start => remove(0)
      case End => remove(length - 1)
      case Index(n) => remove(n)
      case _ => error("message " + cmd + " not understood")
    }
    case Reset() => clear
    case s: Script[Pair[Location, A]] => s.elements foreach <<
    case _ => error("message " + cmd + " not understood")
  }

  /** Return a clone of this buffer.
   *
   *  @return a buffer with the same elements.
   */
  override def clone(): Buffer[A] = super.clone().asInstanceOf[Buffer[A]]

  /** The hashCode method always yields an error, since it is not
   *  safe to use buffers as keys in hash tables.
   *
   *  @return never.
   */
  override def hashCode(): Int = error("unsuitable as hash key")

  /** Defines the prefix of the string representation.
   */
  override protected def stringPrefix: String = "Buffer"
}
