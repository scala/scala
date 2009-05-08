/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

import generic._

/** This class is used internally to implement data structures that
 *  are based on resizable arrays.
 *
 *  @author  Matthias Zenger, Burak Emir
 *  @author Martin Odersky
 *  @version 2.8
 */
trait ResizableArray[A] extends Vector[A] with VectorTemplate[A, ResizableArray[A]] { self =>

  import scala.Array // !!!
  import collection.Iterable // !!!

  override protected[this] def newBuilder = ResizableArray.newBuilder
  override def traversableBuilder[B]: Builder[B, ResizableArray[B], Any] = ResizableArray.newBuilder[B]

  protected def initialSize: Int = 16
  protected var array: Array[AnyRef] = new Array[AnyRef](initialSize max 1)

  protected var size0: Int = 0

  //##########################################################################
  // implement/override methods of Vector[A]

  /** Returns the length of this resizable array.
   */
  def length: Int = size0

  def apply(idx: Int) = {
    if (idx >= size0) throw new IndexOutOfBoundsException(idx.toString)
    array(idx).asInstanceOf[A]
  }

  def update(idx: Int, elem: A) {
    if (idx >= size0) throw new IndexOutOfBoundsException(idx.toString)
    array(idx) = elem.asInstanceOf[AnyRef]
  }

  /** Fills the given array <code>xs</code> with the elements of
   *  this sequence starting at position <code>start</code>.
   *
   *  @param  xs the array to fill.
   *  @param  start starting index.
   */
  override def copyToArray[B >: A](xs: Array[B], start: Int) {
    Array.copy(array, 0, xs, start, size0)
  }

  /** Copy all elements to a buffer
   *  @param   The buffer to which elements are copied
  override def copyToBuffer[B >: A](dest: Buffer[B]) {
    dest ++= (array: Sequence[AnyRef]).asInstanceOf[Sequence[B]]
  }
   */

  override def foreach(f: A => Unit) {
    var i = 0
    while (i < size) {
      f(array(i).asInstanceOf[A])
      i += 1
    }
  }

  //##########################################################################

  /** remove elements of this array at indices after <code>sz</code>
   */
  def reduceToSize(sz: Int) {
    require(sz <= size0)
    while (size0 > sz) {
      size0 -= 1
      array(size0) = null
    }
  }

  /** ensure that the internal array has at n cells */
  protected def ensureSize(n: Int) {
    if (n > array.length) {
      var newsize = array.length * 2
      while (n > newsize)
        newsize = newsize * 2
      val newar: Array[AnyRef] = new Array(newsize)
      Array.copy(array, 0, newar, 0, size0)
      array = newar
    }
  }

  /** Swap two elements of this array.
   */
  protected def swap(a: Int, b: Int) {
    val h = array(a)
    array(a) = array(b)
    array(b) = h
  }

  /** Move parts of the array.
   */
  protected def copy(m: Int, n: Int, len: Int) {
    Array.copy(array, m, array, n, len)
  }
}

object ResizableArray extends SequenceFactory[ResizableArray] {
  type Coll = Vector[_]
  implicit def builderFactory[A]: BuilderFactory[A, Vector[A], Coll] = new BuilderFactory[A, Vector[A], Coll] { def apply(from: Coll) = from.traversableBuilder[A] }
  def newBuilder[A]: Builder[A, ResizableArray[A], Any] = new ArrayBuffer[A]
}
