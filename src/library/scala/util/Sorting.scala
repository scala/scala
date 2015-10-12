/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2015, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package util

import scala.reflect.ClassTag
import scala.math.Ordering

/** The `Sorting` object provides convenience wrappers for `java.util.Arrays.sort`.
  * Methods that defer to `java.util.Arrays.sort` say that they do or under what
  * conditions that they do.
  *
  * `Sorting` also implements a general-purpose quicksort and stable (merge) sort
  * for those cases where `java.util.Arrays.sort` could only be used at the cost
  * of a large memory penalty.  If performance rather than memory usage is the
  * primary concern, one may wish to find alternate strategies to use
  * `java.util.Arrays.sort` directly e.g. by boxing primitives to use
  * a custom ordering on them.
  *
  * `Sorting` provides methods where you can provide a comparison function, or
  * can request a sort of items that are [[scala.math.Ordered]] or that
  * otherwise have an implicit or explicit [[scala.math.Ordering]].
  *
  * Note also that high-performance non-default sorts for numeric types
  * are not provided.  If this is required, it is advisable to investigate
  * other libraries that cover this use case.
  *
  * @author  Ross Judson
  * @author  Adriaan Moors
  * @author  Rex Kerr
  * @version 1.1
  */
object Sorting {
  /** Sort an array of Doubles using `java.util.Arrays.sort`. */
  def quickSort(a: Array[Double]): Unit = java.util.Arrays.sort(a)

  /** Sort an array of Ints using `java.util.Arrays.sort`. */
  def quickSort(a: Array[Int]): Unit    = java.util.Arrays.sort(a)

  /** Sort an array of Floats using `java.util.Arrays.sort`. */
  def quickSort(a: Array[Float]): Unit  = java.util.Arrays.sort(a)
  
  private final val qsortThreshold = 16

  /** Sort array `a` with quicksort, using the Ordering on its elements.
    * This algorithm sorts in place, so no additional memory is used aside from
    * what might be required to box individual elements during comparison.
    */
  def quickSort[K: Ordering](a: Array[K]): Unit = {
    // Must have iN >= i0 or math will fail.  Also, i0 >= 0.
    def inner(a: Array[K], i0: Int, iN: Int, ord: Ordering[K]): Unit = {
      if (iN - i0 < qsortThreshold) insertionSort(a, i0, iN, ord)
      else {
        var iK = (i0 + iN) >>> 1    // Unsigned div by 2
        // Find index of median of first, central, and last elements
        var pL = 
          if (ord.compare(a(i0), a(iN - 1)) <= 0)
            if (ord.compare(a(i0), a(iK)) < 0)
              if (ord.compare(a(iN - 1), a(iK)) < 0) iN - 1 else iK
            else i0
          else
            if (ord.compare(a(i0), a(iK)) < 0) i0
            else
              if (ord.compare(a(iN - 1), a(iK)) <= 0) iN - 1
              else iK
        val pivot = a(pL)
        // pL is the start of the pivot block; move it into the middle if needed
        if (pL != iK) { a(pL) = a(iK); a(iK) = pivot; pL = iK }
        // Elements equal to the pivot will be in range pL until pR
        var pR = pL + 1
        // Items known to be less than pivot are below iA (range i0 until iA)
        var iA = i0
        // Items known to be greater than pivot are at or above iB (range iB until iN)
        var iB = iN
        // Scan through everything in the buffer before the pivot(s)
        while (pL - iA > 0) {
          val current = a(iA)
          ord.compare(current, pivot) match {
            case 0 =>
              // Swap current out with pivot block
              a(iA) = a(pL - 1)
              a(pL - 1) = current
              pL -= 1
            case x if x < 0 =>
              // Already in place.  Just update indices.
              iA += 1
            case _ if iB > pR =>
              // Wrong side.  There's room on the other side, so swap
              a(iA) = a(iB - 1)
              a(iB - 1) = current
              iB -= 1
            case _ =>
              // Wrong side and there is no room.  Swap by rotating pivot block.
              a(iA) = a(pL - 1)
              a(pL - 1) = a(pR - 1)
              a(pR - 1) = current
              pL -= 1
              pR -= 1
              iB -= 1
          }
        }
        // Get anything remaining in buffer after the pivot(s)
        while (iB - pR > 0) {
          val current = a(iB - 1)
          ord.compare(current, pivot) match {
            case 0 =>
              // Swap current out with pivot block
              a(iB - 1) = a(pR)
              a(pR) = current
              pR += 1
            case x if x > 0 =>
              // Already in place.  Just update indices.
              iB -= 1
            case _ =>
              // Wrong side and we already know there is no room.  Swap by rotating pivot block.
              a(iB - 1) = a(pR)
              a(pR) = a(pL)
              a(pL) = current
              iA += 1
              pL += 1
              pR += 1
          }
        }
        // Use tail recursion on large half (Sedgewick's method) so we don't blow up the stack if pivots are poorly chosen
        if (iA - i0 < iN - iB) {
          inner(a, i0, iA, ord)  // True recursion
          inner(a, iB, iN, ord)  // Should be tail recursion
        }
        else {
          inner(a, iB, iN, ord)  // True recursion
          inner(a, i0, iA, ord)  // Should be tail recursion
        }
      }
    }
    inner(a, 0, a.length, implicitly[Ordering[K]])
  }
  
  private final val mergeThreshold = 32
  
  // Ordering[T] might be slow especially for boxed primitives, so use binary search variant of insertion sort
  // Caller must pass iN >= i0 or math will fail.  Also, i0 >= 0.
  private def insertionSort[@specialized T](a: Array[T], i0: Int, iN: Int, ord: Ordering[T]): Unit = {
    val n = iN - i0
    if (n < 2) return
    if (ord.compare(a(i0), a(i0+1)) > 0) {
      val temp = a(i0)
      a(i0) = a(i0+1)
      a(i0+1) = temp
    }
    var m = 2
    while (m < n) {
      // Speed up already-sorted case by checking last element first
      val next = a(i0 + m)
      if (ord.compare(next, a(i0+m-1)) < 0) {
        var iA = i0
        var iB = i0 + m - 1
        while (iB - iA > 1) {
          val ix = (iA + iB) >>> 1    // Use bit shift to get unsigned div by 2
          if (ord.compare(next, a(ix)) < 0) iB = ix
          else iA = ix
        }
        val ix = iA + (if (ord.compare(next, a(iA)) < 0) 0 else 1)
        var i = i0 + m
        while (i > ix) {
          a(i) = a(i-1)
          i -= 1
        }
        a(ix) = next
      }
      m += 1
    }
  }
  
  // Caller is required to pass iN >= i0, else math will fail.  Also, i0 >= 0.
  private def mergeSort[@specialized T: ClassTag](a: Array[T], i0: Int, iN: Int, ord: Ordering[T], scratch: Array[T] = null): Unit = {
    if (iN - i0 < mergeThreshold) insertionSort(a, i0, iN, ord)
    else {
      val iK = (i0 + iN) >>> 1   // Bit shift equivalent to unsigned math, no overflow
      val sc = if (scratch eq null) new Array[T](iK - i0) else scratch
      mergeSort(a, i0, iK, ord, sc)
      mergeSort(a, iK, iN, ord, sc)
      mergeSorted(a, i0, iK, iN, ord, sc)
    }
  }
  
  // Must have 0 <= i0 < iK < iN
  private def mergeSorted[@specialized T](a: Array[T], i0: Int, iK: Int, iN: Int, ord: Ordering[T], scratch: Array[T]): Unit = {
    // Check to make sure we're not already in order
    if (ord.compare(a(iK-1), a(iK)) > 0) {
      var i = i0
      val jN = iK - i0
      var j = 0
      while (i < iK) {
        scratch (j) = a(i)
        i += 1
        j += 1
      }
      var k = i0
      j = 0
      while (i < iN && j < jN) {
        if (ord.compare(a(i), scratch(j)) < 0) { a(k) = a(i); i += 1 }
        else { a(k) = scratch(j); j += 1 }
        k += 1
      }
      while (j < jN) { a(k) = scratch(j); j += 1; k += 1 }
      // Don't need to finish a(i) because it's already in place, k = i
    }
  }
  
  // Why would you even do this?
  private def booleanSort(a: Array[Boolean]): Unit = {
    var i = 0
    var n = 0
    while (i < a.length) {
      if (!a(i)) n += 1
      i += 1
    }
    i = 0
    while (i < n) {
      a(i) = false
      i += 1
    }
    while (i < a.length) {
      a(i) = true
      i += 1
    }
  }

  // TODO: add upper bound: T <: AnyRef, propagate to callers below (not binary compatible)
  // Maybe also rename all these methods to `sort`.
  @inline private def sort[T](a: Array[T], ord: Ordering[T]): Unit = a match {
    case _: Array[AnyRef]  => 
      // Note that runtime matches are covariant, so could actually be any Array[T] s.t. T is not primitive (even boxed value classes)
      if (a.length > 1 && (ord eq null)) throw new NullPointerException("Ordering")
      java.util.Arrays.sort(a, ord)
    case a: Array[Int]     => if (ord eq Ordering.Int) java.util.Arrays.sort(a) else mergeSort[Int](a, 0, a.length, ord)
    case a: Array[Double]  => mergeSort[Double](a, 0, a.length, ord)  // Because not all NaNs are identical, stability is meaningful!
    case a: Array[Long]    => if (ord eq Ordering.Long) java.util.Arrays.sort(a) else mergeSort[Long](a, 0, a.length, ord)
    case a: Array[Float]   => mergeSort[Float](a, 0, a.length, ord)   // Because not all NaNs are identical, stability is meaningful!
    case a: Array[Char]    => if (ord eq Ordering.Char) java.util.Arrays.sort(a) else mergeSort[Char](a, 0, a.length, ord)
    case a: Array[Byte]    => if (ord eq Ordering.Byte) java.util.Arrays.sort(a) else mergeSort[Byte](a, 0, a.length, ord)
    case a: Array[Short]   => if (ord eq Ordering.Short) java.util.Arrays.sort(a) else mergeSort[Short](a, 0, a.length, ord)
    case a: Array[Boolean] => if (ord eq Ordering.Boolean) booleanSort(a) else mergeSort[Boolean](a, 0, a.length, ord)
    // Array[Unit] is matched as an Array[AnyRef] due to covariance in runtime matching.  Not worth catching it as a special case.
    case null => throw new NullPointerException
  }

  // TODO: remove unnecessary ClassTag (not binary compatible)
  /** Sort array `a` using the Ordering on its elements, preserving the original ordering where possible.  Uses `java.util.Arrays.sort` unless `K` is a primitive type. */
  def stableSort[K: ClassTag: Ordering](a: Array[K]): Unit = sort(a, Ordering[K])

  // TODO: Remove unnecessary ClassTag (not binary compatible)
  // TODO: make this fast for primitive K (could be specialized if it didn't go through Ordering)
  /** Sort array `a` using function `f` that computes the less-than relation for each element.  Uses `java.util.Arrays.sort` unless `K` is a primitive type. */
  def stableSort[K: ClassTag](a: Array[K], f: (K, K) => Boolean): Unit = sort(a, Ordering fromLessThan f)

  /** A sorted Array, using the Ordering for the elements in the sequence `a`.  Uses `java.util.Arrays.sort` unless `K` is a primitive type. */
  def stableSort[K: ClassTag: Ordering](a: Seq[K]): Array[K] = {
    val ret = a.toArray
    sort(ret, Ordering[K])
    ret
  }

  // TODO: make this fast for primitive K (could be specialized if it didn't go through Ordering)
  /** A sorted Array, given a function `f` that computes the less-than relation for each item in the sequence `a`.  Uses `java.util.Arrays.sort` unless `K` is a primitive type. */
  def stableSort[K: ClassTag](a: Seq[K], f: (K, K) => Boolean): Array[K] = {
    val ret = a.toArray
    sort(ret, Ordering fromLessThan f)
    ret
  }

  /** A sorted Array, given an extraction function `f` that returns an ordered key for each item in the sequence `a`.  Uses `java.util.Arrays.sort` unless `K` is a primitive type. */
  def stableSort[K: ClassTag, M: Ordering](a: Seq[K], f: K => M): Array[K] = {
    val ret = a.toArray
    sort(ret, Ordering[M] on f)
    ret
  }
}
