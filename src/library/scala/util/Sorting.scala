/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, Ross Judson           **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: $


package scala.util

/** <p>
 *    The Sorting object provides functions that can sort various kinds of
 *    objects. You can provide a comparison function, or you can request a sort
 *    of items that are viewable as <code>Ordered</code>. Some sorts that
 *    operate directly on a subset of value types are also provided. These
 *    implementations are derived from those in the Sun JDK.
 *  </p>
 *  <p>
 *    Note that stability doesn't matter for value types, so use the quickSort
 *    variants for those. <code>stableSort</code> is intended to be used with
 *    objects when the prior ordering should be preserved, where possible.
 *  </p>
 *
 *  @author  Ross Judson
 *  @version 1.0
 */
object Sorting {

  /** Provides implicit access to sorting on arbitrary sequences of orderable
   *  items. This doesn't quite work the way that I want yet -- K should be
   *  bounded as viewable, but the compiler rejects that.
   */
  implicit def seq2RichSort[K <: Ordered[K]](s: Seq[K]) = new RichSorting[K](s)

  /** Quickly sort an array of Doubles. */
  def quickSort(a: Array[Double]) = sort1(a, 0, a.length)

  /** Quickly sort an array of items that are viewable as ordered. */
  def quickSort[K <% Ordered[K]](a: Array[K]) = sort1(a, 0, a.length)

  /** Quickly sort an array of Ints. */
  def quickSort(a: Array[Int]) = sort1(a, 0, a.length)

  /** Quickly sort an array of Floats. */
  def quickSort(a: Array[Float]) = sort1(a, 0, a.length)

  /** Sort an array of K where K is Ordered, preserving the existing order
      where the values are equal. */
  def stableSort[K <% Ordered[K]](a: Array[K]): Unit =
    stableSort(a, 0, a.length-1, new Array[K](a.length), (a:K, b:K) => a < b)

  /** Sorts an array of <code>K</code> given an ordering function
   *  <code>f</code>. <code>f</code> should return <code>true</code> iff
   *  its first parameter is strictly less than its second parameter.
   */
  def stableSort[K](a: Array[K], f: (K,K) => Boolean): Unit =
    stableSort(a, 0, a.length-1, new Array[K](a.length), f)

  /** Sorts an arbitrary sequence into an array, given a comparison function
   *  that should return <code>true</code> iff parameter one is strictly less
   *  than parameter two.
   *
   *  @param  a the sequence to be sorted.
   *  @param  f the comparison function.
   *  @return the sorted sequence of items.
   */
  def stableSort[K](a: Seq[K], f: (K,K) => Boolean): Array[K] = {
    val ret = a.toArray
    stableSort(ret, f)
    ret
  }

  /** Sorts an arbitrary sequence of items that are viewable as ordered. */
  def stableSort[K <% Ordered[K]](a: Seq[K]): Array[K] =
    stableSort(a, (a:K, b:K) => a < b)

  /** Stably sorts a sequence of items given an extraction function that will
   *  return an ordered key from an item.
   *
   *  @param  a the sequence to be sorted.
   *  @param  f the comparison function.
   *  @return the sorted sequence of items.
   */
  def stableSort[K, M <% Ordered[M]](a: Seq[K], f: K => M): Array[K] =
    stableSort(a, (a: K, b: K) => f(a) < f(b))

  private def sort1[K <% Ordered[K]](x: Array[K], off: Int, len: Int) {
    def swap(a: Int, b: Int) {
      val t = x(a)
      x(a) = x(b)
      x(b) = t
    }
    def vecswap(_a: Int, _b: Int, n: Int) {
      var a = _a
      var b = _b
      var i = 0
      while (i < n) {
        swap(a, b)
        i = i + 1
        a = a + 1
        b = b + 1
      }
    }
    def med3(a: Int, b: Int, c: Int) = {
      if (x(a) < x(b)) {
        if (x(b) < x(c)) b else if (x(a) < x(c)) c else a
      } else {
        if (x(b) > x(c)) b else if (x(a) > x(c)) c else a
      }
    }
    // Insertion sort on smallest arrays
    if (len < 7) {
      var i = off
      while (i < len + off) {
        var j = i
        while (j > off && x(j-1) > x(j)) {
          swap(j, j-1)
          j = j - 1
        }
        i = i + 1
      }
    } else {
      // Choose a partition element, v
      var m = off + (len >> 1)        // Small arrays, middle element
      if (len > 7) {
        var l = off
        var n = off + len - 1
        if (len > 40) {        // Big arrays, pseudomedian of 9
          var s = len / 8
          l = med3(l, l+s, l+2*s)
          m = med3(m-s, m, m+s)
          n = med3(n-2*s, n-s, n)
        }
        m = med3(l, m, n) // Mid-size, med of 3
      }
      val v = x(m)

      // Establish Invariant: v* (<v)* (>v)* v*
      var a = off
      var b = a
      var c = off + len - 1
      var d = c;
      var done = false
      while(!done) {
        while (b <= c && x(b) <= v) {
          if (x(b) == v) {
            swap(a, b)
            a = a + 1
          }
          b = b + 1
        }
        while (c >= b && x(c) >= v) {
          if (x(c) == v) {
            swap(c, d)
            d = d - 1
          }
          c = c - 1
        }
        if (b > c) {
          done = true
        } else {
          swap(b, c)
          c = c - 1
          b = b + 1
        }
      }

      // Swap partition elements back to middle
      val n = off + len
      var s = Math.min(a-off, b-a)
      vecswap(off, b-s, s)
      s = Math.min(d-c, n-d-1)
      vecswap(b,   n-s, s)

      // Recursively sort non-partition-elements
      s = b-a
      if (s > 1)
        sort1(x, off, s)
      s = d-c
      if (s > 1)
        sort1(x, n-s, s)
    }
  }

  private def sort1(x: Array[Int], off: Int, len: Int) {
    def swap(a: Int, b: Int) {
      val t = x(a)
      x(a) = x(b)
      x(b) = t
    }
    def vecswap(_a: Int, _b: Int, n: Int) {
      var a = _a
      var b = _b
      var i = 0
      while (i < n) {
        swap(a,b)
        i = i + 1
        a = a + 1
        b = b + 1
      }
    }
    def med3(a: Int, b: Int, c: Int) = {
      if (x(a) < x(b)) {
        if (x(b) < x(c)) b else if (x(a) < x(c)) c else a
      } else {
        if (x(b) > x(c)) b else if (x(a) > x(c)) c else a
      }
    }
    // Insertion sort on smallest arrays
    if (len < 7) {
      var i = off
      while (i < len + off) {
        var j = i
        while (j > off && x(j-1) > x(j)) {
          swap(j, j-1)
          j = j - 1
        }
        i = i + 1
      }
    } else {
      // Choose a partition element, v
      var m = off + (len >> 1)       // Small arrays, middle element
      if (len > 7) {
        var l = off
        var n = off + len - 1
        if (len > 40) {        // Big arrays, pseudomedian of 9
          var s = len/8;
          l = med3(l, l+s, l+2*s)
          m = med3(m-s, m, m+s)
          n = med3(n-2*s, n-s, n)
        }
        m = med3(l, m, n) // Mid-size, med of 3
      }
      val v = x(m)

      // Establish Invariant: v* (<v)* (>v)* v*
      var a = off
      var b = a
      var c = off + len - 1
      var d = c
      var done = false
      while(!done) {
        while (b <= c && x(b) <= v) {
          if (x(b) == v) {
            swap(a, b)
            a = a + 1
          }
          b = b + 1
        }
        while (c >= b && x(c) >= v) {
          if (x(c) == v) {
            swap(c, d)
            d = d - 1
          }
          c = c - 1
        }
        if (b > c) {
          done = true
        } else {
          swap(b, c)
          c = c - 1
          b = b + 1
        }
      }

      // Swap partition elements back to middle
      val n = off + len;
      var s = Math.min(a-off, b-a)
      vecswap(off, b-s, s)
      s = Math.min(d-c, n-d-1)
      vecswap(b,   n-s, s)

      // Recursively sort non-partition-elements
      s = b - a
      if (s > 1)
        sort1(x, off, s)
      s = d-c
      if (s > 1)
        sort1(x, n-s, s)
    }
  }

  private def sort1(x: Array[Double], off: Int, len: Int) {
    def swap(a: Int, b: Int) {
      val t = x(a)
      x(a) = x(b)
      x(b) = t
    }
    def vecswap(_a: Int, _b: Int, n: Int) {
      var a = _a
      var b = _b
      var i = 0
      while (i < n) {
        swap(a, b)
        i = i + 1
        a = a + 1
        b = b + 1
      }
    }
    def med3(a: Int, b: Int, c: Int) =
      if (x(a) < x(b)) {
        if (x(b) < x(c)) b else if (x(a) < x(c)) c else a
      } else {
        if (x(b) > x(c)) b else if (x(a) > x(c)) c else a
      }

    // Insertion sort on smallest arrays
    if (len < 7) {
      var i = off
      while (i < len + off) {
        var j = i
        while (j>off && x(j-1)>x(j)) {
          swap(j,j-1)
          j = j - 1
        }
        i = i + 1
      }
    } else {
      // Choose a partition element, v
      var m = off + (len >> 1)        // Small arrays, middle element
      if (len > 7) {
        var l = off
        var n = off + len - 1
        if (len > 40) {        // Big arrays, pseudomedian of 9
          var s = len / 8
          l = med3(l, l+s, l+2*s)
          m = med3(m-s, m, m+s)
          n = med3(n-2*s, n-s, n)
        }
        m = med3(l, m, n) // Mid-size, med of 3
      }
      val v = x(m)

      // Establish Invariant: v* (<v)* (>v)* v*
      var a = off
      var b = a
      var c = off + len - 1
      var d = c
      var done = false
      while(!done) {
        while (b <= c && x(b) <= v) {
          if (x(b) == v) {
            swap(a, b)
            a = a + 1
          }
          b = b + 1
        }
        while (c >= b && x(c) >= v) {
          if (x(c) == v) {
            swap(c, d)
            d = d - 1
          }
          c = c - 1
        }
        if (b > c) {
          done = true
        } else {
          swap(b, c)
          c = c - 1
          b = b + 1
        }
      }

      // Swap partition elements back to middle
      val n = off + len
      var s = Math.min(a-off, b-a)
      vecswap(off, b-s, s)
      s = Math.min(d-c, n-d-1)
      vecswap(b,   n-s, s)

      // Recursively sort non-partition-elements
      s = b-a
      if (s > 1)
        sort1(x, off, s)
      s = d-c
      if (s > 1)
        sort1(x, n-s, s)
    }
  }

  private def sort1(x: Array[Float], off: Int, len: Int) {
    def swap(a: Int, b: Int) {
      val t = x(a)
      x(a) = x(b)
      x(b) = t
    }
    def vecswap(_a: Int, _b: Int, n: Int) {
      var a = _a
      var b = _b
      var i = 0
      while (i < n) {
        swap(a,b)
        i = i + 1
        a = a + 1
        b = b + 1
      }
    }
    def med3(a: Int, b: Int, c: Int) =
      if (x(a) < x(b)) {
        if (x(b) < x(c)) b else if (x(a) < x(c)) c else a
      } else {
        if (x(b) > x(c)) b else if (x(a) > x(c)) c else a
      }

    // Insertion sort on smallest arrays
    if (len < 7) {
      var i = off
      while (i < len + off) {
        var j = i
        while (j > off && x(j-1) > x(j)) {
          swap(j, j-1)
          j = j - 1
        }
        i = i + 1
      }
    } else {
      // Choose a partition element, v
      var m = off + (len >> 1)       // Small arrays, middle element
      if (len > 7) {
        var l = off
        var n = off + len - 1
        if (len > 40) {        // Big arrays, pseudomedian of 9
          var s = len/8;
          l = med3(l, l+s, l+2*s)
          m = med3(m-s, m, m+s)
          n = med3(n-2*s, n-s, n)
        }
        m = med3(l, m, n) // Mid-size, med of 3
      }
      val v = x(m)

      // Establish Invariant: v* (<v)* (>v)* v*
      var a = off
      var b = a
      var c = off + len - 1
      var d = c
      var done = false
      while(!done) {
        while (b <= c && x(b) <= v) {
          if (x(b) == v) {
            swap(a, b)
            a = a + 1
          }
          b = b + 1
        }
        while (c >= b && x(c) >= v) {
          if (x(c) == v) {
            swap(c, d)
            d = d - 1
          }
          c = c - 1
        }
        if (b > c) {
          done = true
        } else {
          swap(b, c)
          c = c - 1
          b = b + 1
        }
      }

      // Swap partition elements back to middle
      val n = off + len
      var s = Math.min(a-off, b-a)
      vecswap(off, b-s, s)
      s = Math.min(d-c, n-d-1)
      vecswap(b,   n-s, s)

      // Recursively sort non-partition-elements
      s = b - a
      if (s > 1)
        sort1(x, off, s)
      s = d-c
      if (s > 1)
        sort1(x, n-s, s)
    }
  }

  private def stableSort[K](a: Array[K], lo: Int, hi: Int, scratch: Array[K], f: (K,K) => Boolean) {
    if (lo < hi) {
      val mid = (lo+hi) / 2
      stableSort(a, lo, mid, scratch, f)
      stableSort(a, mid+1, hi, scratch, f)
      var k, t_lo = lo
      var t_hi = mid + 1
      while (k <= hi) {
        if ((t_lo <= mid) && ((t_hi > hi) || (f(a(t_lo), a(t_hi))))) {
          scratch(k) = a(t_lo)
          t_lo = t_lo + 1
        } else {
          scratch(k) = a(t_hi)
          t_hi = t_hi + 1
        }
        k = k + 1
      }
      k = lo
      while (k <= hi) {
        a(k) = scratch(k)
        k = k + 1
      }
    }
  }
}

/** <p>
 *    A <code>RichSorting</code> object is generally created implicitly through
 *    the use of the <code>sort</code> function on an arbitrary sequence, where
 *    the items are ordered.
 *  </p>
 */
class RichSorting[K <: Ordered[K]](s: Seq[K]) {

  /** Returns an array with a sorted copy of the RichSorting's sequence.
   */
  def sort = Sorting.stableSort(s)
}
