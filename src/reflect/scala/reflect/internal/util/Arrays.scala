package scala
package reflect.internal.util

import scala.reflect.ClassTag

trait Arrays {

  final def listToArray[A: ClassTag](xs: List[A]): Array[A] = {
    def loop(xs: List[A], ix: Int): Array[A] =
      if (xs.isEmpty)
        new Array[A](ix)
      else {
        val arr = loop(xs.tail, ix+1)
        arr(ix) = xs.head
        arr
      }
    loop(xs, 0)
  }

  /* Partition: in-place partition the elements of arr and returns a boundary b, such that
   *  the segment arr(0..b) contains elements for which pred is true, 
   *  the segment arr(b..L), where l is the length of the array, are elements for which pred is false.
   * This is base on the partition step for Quick-Sort
   */
  final def partitionInPlace[A](arr: Array[A])(pred: A => Boolean): Int = {
    var beg = 0
    var end = arr.length
    while (beg < end) {
      while (beg < end &&   pred(arr(beg)))
        beg += 1

      while (beg < end && ! pred(arr(end-1)))
        end -= 1

      if (beg < end){
        val x = arr(beg)
        arr(beg) = arr(end-1)
        arr(end-1) = x
        beg += 1
      }
    }
    beg
  }

  final def arrayToList[A](arr: Array[A], beg: Int, end: Int): List[A] = {
    var xs: List[A] = Nil
    var ix = end
    while (ix > beg){
      ix -= 1
      xs = arr(ix) :: xs
    }
    xs
  }

  final def maxByPartialOrder[A](arr: Array[A], po: (A, A) => Boolean): Int  = {
    // dominating set by po
    var admitted: Int = 0
    var pending = arr.length
    while (admitted < pending){
      val curr = arr(admitted)
      admitted += 1
      // arr[ix+1..pending).filterNot( po(_, curr)
      var jx = admitted
      var kx = jx
      while (jx < pending){
        if ( ! po(arr(jx), curr)) {
          arr(kx) = arr(jx)
          kx += 1
        }
        jx += 1
      }
      pending = kx
    }
    // Now: arr[0..admitted) are those admitted

    var confirmed = admitted  // arr[confirmed..admitted) are confirmed 
    pending = 0  // arr[pending..confirmed) are pending to evaluate

    while (confirmed > pending) {  // 1 > 0
      confirmed -= 1
      val curr = arr(confirmed)
      // [pending..confirmed).filter ( ! po(_, curr) )
      var jx = confirmed
      var kx = jx
      while (jx > pending) {
        jx -= 1
        if (! po(arr(jx), curr)) {
          kx -= 1
          arr(kx) = arr(jx)
        }
      }
      pending = kx
    }

    // Move from init..admitted
    if (confirmed > 0) {
      var xx = 0
      while (xx < admitted - confirmed) {
        arr(xx) = arr(confirmed+xx)
        xx += 1
      }
      admitted -= pending
    }
    admitted
  }

}

object Arrays extends Arrays
