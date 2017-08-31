package strawman.collection

import scala.{Any, AnyRef, Array, Boolean, Equals, IndexOutOfBoundsException, Int, NoSuchElementException, Ordering, Unit, math, throws}
import scala.Predef.intWrapper
import java.lang.Object

import strawman.collection.immutable.Range

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.util.hashing.MurmurHash3

/** Base trait for sequence collections */
trait Seq[+A] extends Iterable[A] with SeqOps[A, Seq, Seq[A]] {
  final protected[this] def coll: this.type = this
  final def toSeq: this.type = this
  def iterableFactory: SeqFactory[Seq]
}

object Seq extends SeqFactory.Delegate[Seq](immutable.Seq)

/** Base trait for Seq operations */
trait SeqOps[+A, +CC[X], +C] extends Any
  with IterableOps[A, CC, C]
  with ArrayLike[A]
  with Equals {

  /**
    * @return This collection as a `Seq[A]`. This is equivalent to `to(Seq)` but might be faster.
    */
  def toSeq: Seq[A]

  /** Selects all the elements of this $coll ignoring the duplicates.
    *
    * @return a new $coll consisting of all the elements of this $coll without duplicates.
    */
  def distinct: C = fromSpecificIterable(new View.Distinct(toIterable))

  /** Returns new $coll with elements in reversed order.
   *
   *  $willNotTerminateInf
   *
   *  @return A new $coll with all elements of this $coll in reversed order.
   */
  def reverse: C = fromSpecificIterable(reversed)

  /** An iterator yielding elements in reversed order.
   *
   *   $willNotTerminateInf
   *
   * Note: `xs.reverseIterator` is the same as `xs.reverse.iterator` but might be more efficient.
   *
   *  @return  an iterator yielding the elements of this $coll in reversed order
   */
  def reverseIterator(): Iterator[A] = reversed.iterator()

  /** Tests whether this $coll contains the given sequence at a given index.
    *
    * '''Note''': If the both the receiver object `this` and the argument
    * `that` are infinite sequences this method may not terminate.
    *
    * @param  that    the sequence to test
    * @param  offset  the index where the sequence is searched.
    * @return `true` if the sequence `that` is contained in this $coll at
    *         index `offset`, otherwise `false`.
    */
  def startsWith[B >: A](that: Seq[B], offset: Int = 0): Boolean = {
    val i = toIterable.iterator() drop offset
    val j = that.iterator()
    while (j.hasNext && i.hasNext)
      if (i.next() != j.next())
        return false

    !j.hasNext
  }

  /** Tests whether this $coll ends with the given sequence.
    *  $willNotTerminateInf
    *  @param  that    the sequence to test
    *  @return `true` if this $coll has `that` as a suffix, `false` otherwise.
    */
  def endsWith[B >: A](that: Seq[B]): Boolean = {
    val i = toIterable.iterator().drop(length - that.length)
    val j = that.iterator()
    while (i.hasNext && j.hasNext)
      if (i.next() != j.next())
        return false

    !j.hasNext
  }

  /** A copy of this $coll with an element value appended until a given target length is reached.
   *
   *  @param   len   the target length
   *  @param   elem  the padding value
   *  @tparam B      the element type of the returned $coll.
   *  @return a new collection of type `$Coll` consisting of
   *          all elements of this $coll followed by the minimal number of occurrences of `elem` so
   *          that the resulting collection has a length of at least `len`.
   */
  def padTo[B >: A](len: Int, elem: B): CC[B] = fromIterable(View.PadTo(toIterable, len, elem))

  /** Finds index of the first element satisfying some predicate after or at some start index.
    *
    *  $mayNotTerminateInf
    *
    *  @param   p     the predicate used to test elements.
    *  @param   from   the start index
    *  @return  the index `>= from` of the first element of this $coll that satisfies the predicate `p`,
    *           or `-1`, if none exists.
    */
  def indexWhere(p: A => Boolean, from: Int = 0): Int = toIterable.iterator().indexWhere(p, from)

  /** Finds index of first occurrence of some value in this $coll after or at some start index.
    *
    *  @param   elem   the element value to search for.
    *  @tparam  B      the type of the element `elem`.
    *  @param   from   the start index
    *  @return  the index `>= from` of the first element of this $coll that is equal (as determined by `==`)
    *           to `elem`, or `-1`, if none exists.
    */
  def indexOf[B >: A](elem: B, from: Int = 0): Int = indexWhere(elem == _, from)

  /** Finds index of last occurrence of some value in this $coll before or at a given end index.
    *
    *  @param   elem   the element value to search for.
    *  @param   end    the end index.
    *  @tparam  B      the type of the element `elem`.
    *  @return  the index `<= end` of the last element of this $coll that is equal (as determined by `==`)
    *           to `elem`, or `-1`, if none exists.
    */
  def lastIndexOf[B >: A](elem: B, end: Int = length - 1): Int = lastIndexWhere(elem == _, end)

  /** Finds index of last element satisfying some predicate before or at given end index.
    *
    *  @param   p     the predicate used to test elements.
    *  @return  the index `<= end` of the last element of this $coll that satisfies the predicate `p`,
    *           or `-1`, if none exists.
    */
  def lastIndexWhere(p: A => Boolean, end: Int = length - 1): Int = {
    var i = length - 1
    val it = reverseIterator()
    while (it.hasNext && { val elem = it.next(); (i > end || !p(elem)) }) i -= 1
    i
  }

  /** Finds first index after or at a start index where this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @param  from    the start index
   *  @return  the first index `>= from` such that the elements of this $coll starting at this index
   *           match the elements of sequence `that`, or `-1` of no such subsequence exists.
   */
  // TODO Should be implemented in a way that preserves laziness
  def indexOfSlice[B >: A](that: Seq[B], from: Int = 0): Int =
    if (this.knownSize >= 0 && that.knownSize >= 0) {
      val l = length
      val tl = that.length
      val clippedFrom = math.max(0, from)
      if (from > l) -1
      else if (tl < 1) clippedFrom
      else if (l < tl) -1
      else SeqOps.kmpSearch(toSeq, clippedFrom, l, that, 0, tl, forward = true)
    }
    else {
      var i = from
      var s: Seq[A] = toSeq drop i
      while (!s.isEmpty) {
        if (s startsWith that)
          return i

        i += 1
        s = s.tail
      }
      -1
    }

  /** Finds last index before or at a given end index where this $coll contains a given sequence as a slice.
   *  @param  that    the sequence to test
   *  @param  end     the end index
   *  @return  the last index `<= end` such that the elements of this $coll starting at this index
   *           match the elements of sequence `that`, or `-1` of no such subsequence exists.
   */
  def lastIndexOfSlice[B >: A](that: Seq[B], end: Int = length - 1): Int = {
    val l = length
    val tl = that.length
    val clippedL = math.min(l-tl, end)

    if (end < 0) -1
    else if (tl < 1) clippedL
    else if (l < tl) -1
    else SeqOps.kmpSearch(toSeq, 0, clippedL+tl, that, 0, tl, forward = false)
  }

  /** Tests whether this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @return  `true` if this $coll contains a slice with the same elements
   *           as `that`, otherwise `false`.
   */
  def containsSlice[B](that: Seq[B]): Boolean = indexOfSlice(that) != -1

  /** Tests whether this $coll contains a given value as an element.
   *  $mayNotTerminateInf
   *
   *  @param elem  the element to test.
   *  @return     `true` if this $coll has an element that is equal (as
   *              determined by `==`) to `elem`, `false` otherwise.
   */
  def contains[A1 >: A](elem: A1): Boolean = exists (_ == elem)

  /** Iterates over distinct permutations.
    *
    *  @return   An Iterator which traverses the distinct permutations of this $coll.
    *  @example  `"abb".permutations = Iterator(abb, bab, bba)`
    */
  def permutations: Iterator[C] =
    if (isEmpty) Iterator(coll)
    else new PermutationsItr

  /** Iterates over combinations.  A _combination_ of length `n` is a subsequence of
    *  the original sequence, with the elements taken in order.  Thus, `"xy"` and `"yy"`
    *  are both length-2 combinations of `"xyy"`, but `"yx"` is not.  If there is
    *  more than one way to generate the same subsequence, only one will be returned.
    *
    *  For example, `"xyyy"` has three different ways to generate `"xy"` depending on
    *  whether the first, second, or third `"y"` is selected.  However, since all are
    *  identical, only one will be chosen.  Which of the three will be taken is an
    *  implementation detail that is not defined.
    *
    *  @return   An Iterator which traverses the possible n-element combinations of this $coll.
    *  @example  `"abbbc".combinations(2) = Iterator(ab, ac, bb, bc)`
    */
  def combinations(n: Int): Iterator[C] =
    if (n < 0 || n > size) Iterator.empty
    else new CombinationsItr(n)

  private class PermutationsItr extends Iterator[C] {
    private[this] val (elms, idxs) = init()
    private var _hasNext = true

    def hasNext = _hasNext
    @throws[NoSuchElementException]
    def next(): C = {
      if (!hasNext)
        Iterator.empty.next()

      val forcedElms = new mutable.ArrayBuffer[A](elms.size) ++= elms
      val result = (newSpecificBuilder() ++= forcedElms).result()
      var i = idxs.length - 2
      while(i >= 0 && idxs(i) >= idxs(i+1))
        i -= 1

      if (i < 0)
        _hasNext = false
      else {
        var j = idxs.length - 1
        while(idxs(j) <= idxs(i)) j -= 1
        swap(i,j)

        val len = (idxs.length - i) / 2
        var k = 1
        while (k <= len) {
          swap(i+k, idxs.length - k)
          k += 1
        }
      }
      result
    }
    private def swap(i: Int, j: Int): Unit = {
      val tmpI = idxs(i)
      idxs(i) = idxs(j)
      idxs(j) = tmpI
      val tmpE = elms(i)
      elms(i) = elms(j)
      elms(j) = tmpE
    }

    private[this] def init() = {
      val m = mutable.HashMap[A, Int]()
      val (es, is) = (toSeq map (e => (e, m.getOrElseUpdate(e, m.size))) sortBy (_._2)).unzip

      (es.to(mutable.ArrayBuffer), is.toArray)
    }
  }

  private class CombinationsItr(n: Int) extends Iterator[C] {
    // generating all nums such that:
    // (1) nums(0) + .. + nums(length-1) = n
    // (2) 0 <= nums(i) <= cnts(i), where 0 <= i <= cnts.length-1
    private val (elms, cnts, nums) = init()
    private val offs = cnts.scanLeft(0)(_ + _)
    private var _hasNext = true

    def hasNext = _hasNext
    def next(): C = {
      if (!hasNext)
        Iterator.empty.next()

      /* Calculate this result. */
      val buf = newSpecificBuilder()
      for(k <- 0 until nums.length; j <- 0 until nums(k))
        buf += elms(offs(k)+j)
      val res = buf.result()

      /* Prepare for the next call to next. */
      var idx = nums.length - 1
      while (idx >= 0 && nums(idx) == cnts(idx))
        idx -= 1

      idx = nums.lastIndexWhere(_ > 0, idx - 1)

      if (idx < 0)
        _hasNext = false
      else {
        // OPT: hand rolled version of `sum = nums.view(idx + 1, nums.length).sum + 1`
        var sum = 1
        var i = idx + 1
        while (i < nums.length) {
          sum += nums(i)
          i += 1
        }
        nums(idx) -= 1
        for (k <- (idx+1) until nums.length) {
          nums(k) = sum min cnts(k)
          sum -= nums(k)
        }
      }

      res
    }

    /** Rearrange seq to newSeq a0a0..a0a1..a1...ak..ak such that
      *  seq.count(_ == aj) == cnts(j)
      *
      *  @return     (newSeq,cnts,nums)
      */
    private def init(): (IndexedSeq[A], Array[Int], Array[Int]) = {
      val m = mutable.HashMap[A, Int]()

      // e => (e, weight(e))
      val (es, is) = (toSeq map (e => (e, m.getOrElseUpdate(e, m.size))) sortBy (_._2)).unzip
      val cs = new Array[Int](m.size)
      is foreach (i => cs(i) += 1)
      val ns = new Array[Int](cs.length)

      var r = n
      0 until ns.length foreach { k =>
        ns(k) = r min cs(k)
        r -= ns(k)
      }
      (es.to(IndexedSeq), cs, ns)
    }
  }

  /** Sorts this $coll according to an Ordering.
    *
    *  The sort is stable. That is, elements that are equal (as determined by
    *  `lt`) appear in the same order in the sorted sequence as in the original.
    *
    *  @see [[scala.math.Ordering]]
    *
    *  @param  ord the ordering to be used to compare elements.
    *  @return     a $coll consisting of the elements of this $coll
    *              sorted according to the ordering `ord`.
    */
  def sorted[B >: A](implicit ord: Ordering[B]): C = {
    val len = this.length
    val b = newSpecificBuilder()
    if (len == 1) b ++= toIterable
    else if (len > 1) {
      b.sizeHint(len)
      val arr = new Array[AnyRef](len)  // Previously used ArraySeq for more compact but slower code
      var i = 0
      for (x <- this) {
        arr(i) = x.asInstanceOf[AnyRef]
        i += 1
      }
      java.util.Arrays.sort(arr, ord.asInstanceOf[Ordering[Object]])
      i = 0
      while (i < arr.length) {
        b += arr(i).asInstanceOf[A]
        i += 1
      }
    }
    b.result()
  }

  /** Sorts this $coll according to a comparison function.
    *  $willNotTerminateInf
    *
    *  The sort is stable. That is, elements that are equal (as determined by
    *  `lt`) appear in the same order in the sorted sequence as in the original.
    *
    *  @param  lt  the comparison function which tests whether
    *              its first argument precedes its second argument in
    *              the desired ordering.
    *  @return     a $coll consisting of the elements of this $coll
    *              sorted according to the comparison function `lt`.
    *  @example {{{
    *    List("Steve", "Tom", "John", "Bob").sortWith(_.compareTo(_) < 0) =
    *    List("Bob", "John", "Steve", "Tom")
    *  }}}
    */
  def sortWith(lt: (A, A) => Boolean): C = sorted(Ordering.fromLessThan(lt))

  /** Sorts this $Coll according to the Ordering which results from transforming
    *  an implicitly given Ordering with a transformation function.
    *  @see [[scala.math.Ordering]]
    *  $willNotTerminateInf
    *  @param   f the transformation function mapping elements
    *           to some other domain `B`.
    *  @param   ord the ordering assumed on domain `B`.
    *  @tparam  B the target type of the transformation `f`, and the type where
    *           the ordering `ord` is defined.
    *  @return  a $coll consisting of the elements of this $coll
    *           sorted according to the ordering where `x < y` if
    *           `ord.lt(f(x), f(y))`.
    *
    *  @example {{{
    *    val words = "The quick brown fox jumped over the lazy dog".split(' ')
    *    // this works because scala.Ordering will implicitly provide an Ordering[Tuple2[Int, Char]]
    *    words.sortBy(x => (x.length, x.head))
    *    res0: Array[String] = Array(The, dog, fox, the, lazy, over, brown, quick, jumped)
    *  }}}
    */
  def sortBy[B](f: A => B)(implicit ord: Ordering[B]): C = sorted(ord on f)

  /** Produces the range of all indices of this sequence.
    *
    *  @return  a `Range` value from `0` to one less than the length of this $coll.
    */
  def indices: Range = Range(0, length)

  /** Are the elements of this collection the same (and in the same order)
    * as those of `that`?
    */
  def sameElements[B >: A](that: IterableOnce[B]): Boolean =
    toIterable.iterator().sameElements(that)

  /** Method called from equality methods, so that user-defined subclasses can
    *  refuse to be equal to other collections of the same kind.
    *  @param   that   The object with which this $coll should be compared
    *  @return  `true`, if this $coll can possibly equal `that`, `false` otherwise. The test
    *           takes into consideration only the run-time types of objects but ignores their elements.
    */
  def canEqual(that: Any): Boolean = true

  override def equals(o: scala.Any): Boolean =
    o match {
      case it: Seq[A] => (it canEqual this) && sameElements(it)
      case _ => false
    }

  override def hashCode(): Int = stableIterableHash(toIterable)

  // Temporary: TODO move to MurmurHash3.scala
  private def stableIterableHash(xs: Iterable[_]): Int = {
    var n = 0
    var h = "Seq".##
    val it = xs.iterator()
    while (it.hasNext) {
      h = MurmurHash3.mix(h, it.next().##)
      n += 1
    }
    MurmurHash3.finalizeHash(h, n)
  }
}

object SeqOps {
  // KMP search utilities

 /**  A KMP implementation, based on the undoubtedly reliable wikipedia entry.
   *  Note: I made this private to keep it from entering the API.  That can be reviewed.
   *
   *  @author paulp, Rex Kerr
   *  @since  2.10
   *  @param  S       Sequence that may contain target
   *  @param  m0      First index of S to consider
   *  @param  m1      Last index of S to consider (exclusive)
   *  @param  W       Target sequence
   *  @param  n0      First index of W to match
   *  @param  n1      Last index of W to match (exclusive)
   *  @param  forward Direction of search (from beginning==true, from end==false)
   *  @return Index of start of sequence if found, -1 if not (relative to beginning of S, not m0).
   */
  private def kmpSearch[B](S: Seq[B], m0: Int, m1: Int, W: Seq[B], n0: Int, n1: Int, forward: Boolean): Int = {
    // Check for redundant case when target has single valid element
    def clipR(x: Int, y: Int) = if (x < y) x else -1
    def clipL(x: Int, y: Int) = if (x > y) x else -1

    if (n1 == n0+1) {
      if (forward)
        clipR(S.indexOf(W(n0), m0), m1)
      else
        clipL(S.lastIndexOf(W(n0), m1-1), m0-1)
    }

    // Check for redundant case when both sequences are same size
    else if (m1-m0 == n1-n0) {
      // Accepting a little slowness for the uncommon case.
      if (S.view.slice(m0, m1) == W.view.slice(n0, n1)) m0
      else -1
    }
    // Now we know we actually need KMP search, so do it
    else S match {
      case xs: IndexedSeq[_] =>
        // We can index into S directly; it should be adequately fast
        val Wopt = kmpOptimizeWord(W, n0, n1, forward)
        val T = kmpJumpTable(Wopt, n1-n0)
        var i, m = 0
        val zero = if (forward) m0 else m1-1
        val delta = if (forward) 1 else -1
        while (i+m < m1-m0) {
          if (Wopt(i) == S(zero+delta*(i+m))) {
            i += 1
            if (i == n1-n0) return (if (forward) m+m0 else m1-m-i)
          }
          else {
            val ti = T(i)
            m += i - ti
            if (i > 0) i = ti
          }
        }
        -1
      case _ =>
        // We had better not index into S directly!
        val iter = S.iterator().drop(m0)
        val Wopt = kmpOptimizeWord(W, n0, n1, forward = true)
        val T = kmpJumpTable(Wopt, n1-n0)
        val cache = new Array[AnyRef](n1-n0)  // Ring buffer--need a quick way to do a look-behind
        var largest = 0
        var i, m = 0
        var answer = -1
        while (m+m0+n1-n0 <= m1) {
          while (i+m >= largest) {
            cache(largest%(n1-n0)) = iter.next().asInstanceOf[AnyRef]
            largest += 1
          }
          if (Wopt(i) == cache((i+m)%(n1-n0)).asInstanceOf[B]) {
            i += 1
            if (i == n1-n0) {
              if (forward) return m+m0
              else {
                i -= 1
                answer = m+m0
                val ti = T(i)
                m += i - ti
                if (i > 0) i = ti
              }
            }
          }
          else {
            val ti = T(i)
            m += i - ti
            if (i > 0) i = ti
          }
        }
        answer
    }
  }

  /** Make sure a target sequence has fast, correctly-ordered indexing for KMP.
   *
   *  @author Rex Kerr
   *  @since  2.10
   *  @param  W    The target sequence
   *  @param  n0   The first element in the target sequence that we should use
   *  @param  n1   The far end of the target sequence that we should use (exclusive)
   *  @return Target packed in an IndexedSeq (taken from iterator unless W already is an IndexedSeq)
   */
  private def kmpOptimizeWord[B](W: Seq[B], n0: Int, n1: Int, forward: Boolean): IndexedView[B] = W match {
    case iso: IndexedSeq[B] =>
      // Already optimized for indexing--use original (or custom view of original)
      if (forward && n0==0 && n1==W.length) iso.view
      else if (forward) new IndexedView[B] {
        val length = n1 - n0
        def apply(x: Int) = iso(n0 + x)
      }
      else new IndexedView[B] {
        def length = n1 - n0
        def apply(x: Int) = iso(n1 - 1 - x)
      }
    case _ =>
      // W is probably bad at indexing.  Pack in array (in correct orientation)
      // Would be marginally faster to special-case each direction
      new IndexedView[B] {
        private[this] val Warr = new Array[AnyRef](n1-n0)
        private[this] val delta = if (forward) 1 else -1
        private[this] val done = if (forward) n1-n0 else -1
        val wit = W.iterator().drop(n0)
        var i = if (forward) 0 else (n1-n0-1)
        while (i != done) {
          Warr(i) = wit.next().asInstanceOf[AnyRef]
          i += delta
        }

        val length = n1 - n0
        def apply(x: Int) = Warr(x).asInstanceOf[B]
      }
  }

 /** Make a jump table for KMP search.
   *
   *  @author paulp, Rex Kerr
   *  @since  2.10
   *  @param  Wopt The target sequence
   *  @param  wlen Just in case we're only IndexedSeq and not IndexedSeqOptimized
   *  @return KMP jump table for target sequence
   */
 private def kmpJumpTable[B](Wopt: IndexedView[B], wlen: Int) = {
    val arr = new Array[Int](wlen)
    var pos = 2
    var cnd = 0
    arr(0) = -1
    arr(1) = 0
    while (pos < wlen) {
      if (Wopt(pos-1) == Wopt(cnd)) {
        arr(pos) = cnd + 1
        pos += 1
        cnd += 1
      }
      else if (cnd > 0) {
        cnd = arr(cnd)
      }
      else {
        arr(pos) = 0
        pos += 1
      }
    }
    arr
  }
}

