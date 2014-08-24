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

/** This class is used internally to implement data structures that
 *  are based on resizable arrays.
 *
 *  @tparam A    type of the elements contained in this resizable array.
 *
 *  @author  Matthias Zenger, Burak Emir
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   1
 */
trait ResizableArray[A] extends IndexedSeq[A]
                           with GenericTraversableTemplate[A, ResizableArray]
                           with IndexedSeqOptimized[A, ResizableArray[A]] {

  override def companion: GenericCompanion[ResizableArray] = ResizableArray

  protected def initialSize: Int = 16
  protected var array: Array[AnyRef] = new Array[AnyRef](math.max(initialSize, 1))
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

  override def foreach[U](f: A => U) {
    var i = 0
    // size is cached here because profiling reports a lot of time spent calling
    // it on every iteration.  I think it's likely a profiler ghost but it doesn't
    // hurt to lift it into a local.
    val top = size
    while (i < top) {
      f(array(i).asInstanceOf[A])
      i += 1
    }
  }

  /** Fills the given array `xs` with at most `len` elements of this
   *  traversable starting at position `start`.
   *
   *  Copying will stop once either the end of the current traversable is
   *  reached or `len` elements have been copied or the end of the array
   *  is reached.
   *
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @param  len number of elements to copy
   */
   override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) {
     val len1 = len min (xs.length - start) min length
     if (len1 > 0) Array.copy(array, 0, xs, start, len1)
   }

  //##########################################################################

  /** Remove elements of this array at indices after `sz`.
   */
  def reduceToSize(sz: Int) {
    require(sz <= size0)
    while (size0 > sz) {
      size0 -= 1
      array(size0) = null
    }
  }

  /** Ensure that the internal array has at least `n` cells. */
  protected def ensureSize(n: Int) {
    // Use a Long to prevent overflows
    val arrayLength: Long = array.length
    if (n > arrayLength) {
      var newSize: Long = arrayLength * 2
      while (n > newSize)
        newSize = newSize * 2
      // Clamp newSize to Int.MaxValue
      if (newSize > Int.MaxValue) newSize = Int.MaxValue

      val newArray: Array[AnyRef] = new Array(newSize.toInt)
      scala.compat.Platform.arraycopy(array, 0, newArray, 0, size0)
      array = newArray
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
    scala.compat.Platform.arraycopy(array, m, array, n, len)
  }
}

object ResizableArray extends SeqFactory[ResizableArray] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, ResizableArray[A]] =
    ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]

  def newBuilder[A]: Builder[A, ResizableArray[A]] = new ArrayBuffer[A]
}
