/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._

/** This class is used internally to implement data structures that
 *  are based on resizable arrays.
 *
 *  @author  Matthias Zenger, Burak Emir
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   1
 */
trait ResizableArray[A] extends IndexedSeq[A]
                           with GenericTraversableTemplate[A, ResizableArray]
                           with IndexedSeqLike[A, ResizableArray[A]] {

  override def companion: GenericCompanion[ResizableArray] = ResizableArray

  protected def initialSize: Int = 16
  protected var array: Array[AnyRef] = new Array[AnyRef](initialSize max 1)

  protected var size0: Int = 0

  //##########################################################################
  // implement/override methods of IndexedSeq[A]

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

  override def foreach[U](f: A =>  U) {
    var i = 0
    while (i < size) {
      f(array(i).asInstanceOf[A])
      i += 1
    }
  }

  /** Fills the given array <code>xs</code> with at most `len` elements of
   *  this traversable starting at position `start`.
   *  Copying will stop once either the end of the current traversable is reached or
   *  `len` elements have been copied or the end of the array is reached.
   *
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @param  len number of elements to copy
   */
   override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) {
     val len1 = len min (xs.length - start) min length
     Array.copy(array, 0, xs, start, len1)
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

object ResizableArray extends SeqFactory[ResizableArray] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, ResizableArray[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, ResizableArray[A]] = new ArrayBuffer[A]
}
