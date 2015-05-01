/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

import mutable.{ ListBuffer, ArraySeq }
import immutable.{ List, Range }
import generic._
import parallel.ParSeq
import scala.math.{ min, max, Ordering }

/** A template trait for sequences of type `Seq[A]`
 *  $seqInfo
 *
 *  @define seqInfo
 *  Sequences are special cases of iterable collections of class `Iterable`.
 *  Unlike iterables, sequences always have a defined order of elements.
 *  Sequences provide a method `apply` for indexing. Indices range from `0` up to the `length` of
 *  a sequence. Sequences support a number of methods to find occurrences of elements or subsequences, including
 *  `segmentLength`, `prefixLength`, `indexWhere`, `indexOf`, `lastIndexWhere`, `lastIndexOf`,
 *  `startsWith`, `endsWith`, `indexOfSlice`.
 *
 *  Another way to see a sequence is as a `PartialFunction` from `Int` values
 *  to the element type of the sequence. The `isDefinedAt` method of a sequence
 *  returns `true` for the interval from `0` until `length`.
 *
 *  Sequences can be accessed in reverse order of their elements, using methods
 *  `reverse` and `reverseIterator`.
 *
 *  Sequences have two principal subtraits, `IndexedSeq` and `LinearSeq`, which give different guarantees for performance.
 *  An `IndexedSeq` provides fast random-access of elements and a fast `length` operation.
 *  A `LinearSeq` provides fast access only to the first element via `head`, but also
 *  has a fast `tail` operation.
 *
 *  @tparam A    the element type of the collection
 *  @tparam Repr the type of the actual collection containing the elements.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 *  @since   2.8
 *
 *  @define Coll `Seq`
 *  @define coll sequence
 *  @define thatinfo the class of the returned collection. Where possible, `That` is
 *    the same class as the current collection class `Repr`, but this
 *    depends on the element type `B` being admissible for that class,
 *    which means that an implicit instance of type `CanBuildFrom[Repr, B, That]`
 *    is found.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`.
 *  @define orderDependent
 *  @define orderDependentFold
 */
trait SeqLike[+A, +Repr] extends Any with IterableLike[A, Repr] with GenSeqLike[A, Repr] with Parallelizable[A, ParSeq[A]] { self =>

  override protected[this] def thisCollection: Seq[A] = this.asInstanceOf[Seq[A]]
  override protected[this] def toCollection(repr: Repr): Seq[A] = repr.asInstanceOf[Seq[A]]

  def length: Int

  def apply(idx: Int): A

  protected[this] override def parCombiner = ParSeq.newCombiner[A]

  /** Compares the length of this $coll to a test value.
   *
   *   @param   len   the test value that gets compared with the length.
   *   @return  A value `x` where
   *   {{{
   *        x <  0       if this.length <  len
   *        x == 0       if this.length == len
   *        x >  0       if this.length >  len
   *   }}}
   *  The method as implemented here does not call `length` directly; its running time
   *  is `O(length min len)` instead of `O(length)`. The method should be overwritten
   *  if computing `length` is cheap.
   */
  def lengthCompare(len: Int): Int = {
    if (len < 0) 1
    else {
      var i = 0
      val it = iterator
      while (it.hasNext) {
        if (i == len) return if (it.hasNext) 1 else 0
        it.next()
        i += 1
      }
      i - len
    }
  }

  override /*IterableLike*/ def isEmpty: Boolean = lengthCompare(0) == 0

  /** The size of this $coll, equivalent to `length`.
   *
   *  $willNotTerminateInf
   */
  override def size = length

  def segmentLength(p: A => Boolean, from: Int): Int = {
    var i = 0
    val it = iterator.drop(from)
    while (it.hasNext && p(it.next()))
      i += 1
    i
  }

  def indexWhere(p: A => Boolean, from: Int): Int = {
    var i = from
    val it = iterator.drop(from)
    while (it.hasNext) {
      if (p(it.next())) return i
      else i += 1
    }

    -1
  }

  def lastIndexWhere(p: A => Boolean, end: Int): Int = {
    var i = length - 1
    val it = reverseIterator
    while (it.hasNext && { val elem = it.next(); (i > end || !p(elem)) }) i -= 1
    i
  }

  /** Iterates over distinct permutations.
   *
   *  @return   An Iterator which traverses the distinct permutations of this $coll.
   *  @example  `"abb".permutations = Iterator(abb, bab, bba)`
   */
  def permutations: Iterator[Repr] =
    if (isEmpty) Iterator(repr)
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
  def combinations(n: Int): Iterator[Repr] =
    if (n < 0 || n > size) Iterator.empty
    else new CombinationsItr(n)

  private class PermutationsItr extends AbstractIterator[Repr] {
    private[this] val (elms, idxs) = init()
    private var _hasNext = true

    def hasNext = _hasNext
    def next(): Repr = {
      if (!hasNext)
        Iterator.empty.next()

      val forcedElms = new mutable.ArrayBuffer[A](elms.size) ++= elms
      val result = (self.newBuilder ++= forcedElms).result()
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
    private def swap(i: Int, j: Int) {
      val tmpI = idxs(i)
      idxs(i) = idxs(j)
      idxs(j) = tmpI
      val tmpE = elms(i)
      elms(i) = elms(j)
      elms(j) = tmpE
    }

    private[this] def init() = {
      val m = mutable.HashMap[A, Int]()
      val (es, is) = (thisCollection map (e => (e, m.getOrElseUpdate(e, m.size))) sortBy (_._2)).unzip

      (es.toBuffer, is.toArray)
    }
  }

  private class CombinationsItr(n: Int) extends AbstractIterator[Repr] {
    // generating all nums such that:
    // (1) nums(0) + .. + nums(length-1) = n
    // (2) 0 <= nums(i) <= cnts(i), where 0 <= i <= cnts.length-1
    private val (elms, cnts, nums) = init()
    private val offs = cnts.scanLeft(0)(_ + _)
    private var _hasNext = true

    def hasNext = _hasNext
    def next(): Repr = {
      if (!hasNext)
        Iterator.empty.next()

      /* Calculate this result. */
      val buf = self.newBuilder
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
        var sum = nums.slice(idx + 1, nums.length).sum + 1
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
      val (es, is) = (thisCollection map (e => (e, m.getOrElseUpdate(e, m.size))) sortBy (_._2)).unzip
      val cs = new Array[Int](m.size)
      is foreach (i => cs(i) += 1)
      val ns = new Array[Int](cs.length)

      var r = n
      0 until ns.length foreach { k =>
        ns(k) = r min cs(k)
        r -= ns(k)
      }
      (es.toIndexedSeq, cs, ns)
    }
  }

  def reverse: Repr = {
    var xs: List[A] = List()
    for (x <- this)
      xs = x :: xs
    val b = newBuilder
    b.sizeHint(this)
    for (x <- xs)
      b += x
    b.result()
  }

  def reverseMap[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    var xs: List[A] = List()
    for (x <- this)
      xs = x :: xs
    val b = bf(repr)
    for (x <- xs)
      b += f(x)

    b.result()
  }

  /** An iterator yielding elements in reversed order.
   *
   *   $willNotTerminateInf
   *
   * Note: `xs.reverseIterator` is the same as `xs.reverse.iterator` but might be more efficient.
   *
   *  @return  an iterator yielding the elements of this $coll in reversed order
   */
  def reverseIterator: Iterator[A] = toCollection(reverse).iterator

  def startsWith[B](that: GenSeq[B], offset: Int): Boolean = {
    val i = this.iterator drop offset
    val j = that.iterator
    while (j.hasNext && i.hasNext)
      if (i.next != j.next)
        return false

    !j.hasNext
  }

  def endsWith[B](that: GenSeq[B]): Boolean = {
    val i = this.iterator.drop(length - that.length)
    val j = that.iterator
    while (i.hasNext && j.hasNext)
      if (i.next != j.next)
        return false

    !j.hasNext
  }

  /** Finds first index where this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @return  the first index such that the elements of this $coll starting at this index
   *           match the elements of sequence `that`, or `-1` of no such subsequence exists.
   */
  def indexOfSlice[B >: A](that: GenSeq[B]): Int = indexOfSlice(that, 0)

  /** Finds first index after or at a start index where this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @param  from    the start index
   *  @return  the first index `>= from` such that the elements of this $coll starting at this index
   *           match the elements of sequence `that`, or `-1` of no such subsequence exists.
   */
  def indexOfSlice[B >: A](that: GenSeq[B], from: Int): Int =
    if (this.hasDefiniteSize && that.hasDefiniteSize) {
      val l = length
      val tl = that.length
      val clippedFrom = math.max(0, from)
      if (from > l) -1
      else if (tl < 1) clippedFrom
      else if (l < tl) -1
      else SeqLike.kmpSearch(thisCollection, clippedFrom, l, that.seq, 0, tl, forward = true)
    }
    else {
      var i = from
      var s: Seq[A] = thisCollection drop i
      while (!s.isEmpty) {
        if (s startsWith that)
          return i

        i += 1
        s = s.tail
      }
      -1
    }

  /** Finds last index where this $coll contains a given sequence as a slice.
   *  $willNotTerminateInf
   *  @param  that    the sequence to test
   *  @return  the last index such that the elements of this $coll starting a this index
   *           match the elements of sequence `that`, or `-1` of no such subsequence exists.
   */
  def lastIndexOfSlice[B >: A](that: GenSeq[B]): Int = lastIndexOfSlice(that, length)

  /** Finds last index before or at a given end index where this $coll contains a given sequence as a slice.
   *  @param  that    the sequence to test
   *  @param  end     the end index
   *  @return  the last index `<= end` such that the elements of this $coll starting at this index
   *           match the elements of sequence `that`, or `-1` of no such subsequence exists.
   */
  def lastIndexOfSlice[B >: A](that: GenSeq[B], end: Int): Int = {
    val l = length
    val tl = that.length
    val clippedL = math.min(l-tl, end)

    if (end < 0) -1
    else if (tl < 1) clippedL
    else if (l < tl) -1
    else SeqLike.kmpSearch(thisCollection, 0, clippedL+tl, that.seq, 0, tl, forward = false)
  }

  /** Tests whether this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @return  `true` if this $coll contains a slice with the same elements
   *           as `that`, otherwise `false`.
   */
  def containsSlice[B](that: GenSeq[B]): Boolean = indexOfSlice(that) != -1

  /** Tests whether this $coll contains a given value as an element.
   *  $mayNotTerminateInf
   *
   *  @param elem  the element to test.
   *  @return     `true` if this $coll has an element that is equal (as
   *              determined by `==`) to `elem`, `false` otherwise.
   */
  def contains[A1 >: A](elem: A1): Boolean = exists (_ == elem)

  /** Produces a new sequence which contains all elements of this $coll and also all elements of
   *  a given sequence. `xs union ys`  is equivalent to `xs ++ ys`.
   *
   *  @param that   the sequence to add.
   *  @tparam B     the element type of the returned $coll.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` which contains all elements of this $coll
   *                followed by all elements of `that`.
   *  @usecase def union(that: Seq[A]): $Coll[A]
   *    @inheritdoc
   *
   *    Another way to express this
   *    is that `xs union ys` computes the order-preserving multi-set union of `xs` and `ys`.
   *    `union` is hence a counter-part of `diff` and `intersect` which also work on multi-sets.
   *
   *    $willNotTerminateInf
   *
   *    @return       a new $coll which contains all elements of this $coll
   *                  followed by all elements of `that`.
   */
  override def union[B >: A, That](that: GenSeq[B])(implicit bf: CanBuildFrom[Repr, B, That]): That =
    this ++ that

  /** Computes the multiset difference between this $coll and another sequence.
   *
   *  @param that   the sequence of elements to remove
   *  @tparam B     the element type of the returned $coll.
   *  @return       a new collection of type `That` which contains all elements of this $coll
   *                except some of occurrences of elements that also appear in `that`.
   *                If an element value `x` appears
   *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
   *                part of the result, but any following occurrences will.
   *  @usecase def diff(that: Seq[A]): $Coll[A]
   *    @inheritdoc
   *
   *    $willNotTerminateInf
   *
   *    @return       a new $coll which contains all elements of this $coll
   *                  except some of occurrences of elements that also appear in `that`.
   *                  If an element value `x` appears
   *                  ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
   *                  part of the result, but any following occurrences will.
   */
  def diff[B >: A](that: GenSeq[B]): Repr = {
    val occ = occCounts(that.seq)
    val b = newBuilder
    for (x <- this) {
      val ox = occ(x)  // Avoid multiple map lookups
      if (ox == 0) b += x
      else occ(x) = ox - 1
    }
    b.result()
  }

  /** Computes the multiset intersection between this $coll and another sequence.
   *
   *  @param that   the sequence of elements to intersect with.
   *  @tparam B     the element type of the returned $coll.
   *  @return       a new collection of type `That` which contains all elements of this $coll
   *                which also appear in `that`.
   *                If an element value `x` appears
   *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
   *                in the result, but any following occurrences will be omitted.
   *  @usecase def intersect(that: Seq[A]): $Coll[A]
   *    @inheritdoc
   *
   *    $mayNotTerminateInf
   *
   *    @return       a new $coll which contains all elements of this $coll
   *                  which also appear in `that`.
   *                  If an element value `x` appears
   *                  ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
   *                  in the result, but any following occurrences will be omitted.
   */
  def intersect[B >: A](that: GenSeq[B]): Repr = {
    val occ = occCounts(that.seq)
    val b = newBuilder
    for (x <- this) {
      val ox = occ(x)  // Avoid multiple map lookups
      if (ox > 0) {
        b += x
        occ(x) = ox - 1
      }
    }
    b.result()
  }

  private def occCounts[B](sq: Seq[B]): mutable.Map[B, Int] = {
    val occ = new mutable.HashMap[B, Int] { override def default(k: B) = 0 }
    for (y <- sq) occ(y) += 1
    occ
  }

  /** Builds a new $coll from this $coll without any duplicate elements.
   *  $willNotTerminateInf
   *
   *  @return  A new $coll which contains the first occurrence of every element of this $coll.
   */
  def distinct: Repr = {
    val b = newBuilder
    val seen = mutable.HashSet[A]()
    for (x <- this) {
      if (!seen(x)) {
        b += x
        seen += x
      }
    }
    b.result()
  }

  def patch[B >: A, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    var i = 0
    val it = this.iterator
    while (i < from && it.hasNext) {
      b += it.next()
      i += 1
    }
    b ++= patch.seq
    i = replaced
    while (i > 0 && it.hasNext) {
      it.next()
      i -= 1
    }
    while (it.hasNext) b += it.next()
    b.result()
  }

  def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    if (index < 0) throw new IndexOutOfBoundsException(index.toString)
    val b = bf(repr)
    var i = 0
    val it = this.iterator
    while (i < index && it.hasNext) {
      b += it.next()
      i += 1
    }
    if (!it.hasNext) throw new IndexOutOfBoundsException(index.toString)
    b += elem
    it.next()
    while (it.hasNext) b += it.next()
    b.result()
  }

  def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    b += elem
    b ++= thisCollection
    b.result()
  }

  def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    b ++= thisCollection
    b += elem
    b.result()
  }

  def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    val L = length
    b.sizeHint(math.max(L, len))
    var diff = len - L
    b ++= thisCollection
    while (diff > 0) {
      b += elem
      diff -= 1
    }
    b.result()
  }

  def corresponds[B](that: GenSeq[B])(p: (A,B) => Boolean): Boolean = {
    val i = this.iterator
    val j = that.iterator
    while (i.hasNext && j.hasNext)
      if (!p(i.next(), j.next()))
        return false

    !i.hasNext && !j.hasNext
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
  def sortWith(lt: (A, A) => Boolean): Repr = sorted(Ordering fromLessThan lt)

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
  def sortBy[B](f: A => B)(implicit ord: Ordering[B]): Repr = sorted(ord on f)

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
  def sorted[B >: A](implicit ord: Ordering[B]): Repr = {
    val len = this.length
    val b = newBuilder
    if (len == 1) b ++= this
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

  /** Converts this $coll to a sequence.
   *  $willNotTerminateInf
   *
   *  A new collection will not be built; in particular, lazy sequences will stay lazy.
   */
  override def toSeq: Seq[A] = thisCollection

  /** Produces the range of all indices of this sequence.
   *
   *  @return  a `Range` value from `0` to one less than the length of this $coll.
   */
  def indices: Range = 0 until length

  override def view = new SeqView[A, Repr] {
    protected lazy val underlying = self.repr
    override def iterator = self.iterator
    override def length = self.length
    override def apply(idx: Int) = self.apply(idx)
  }

  override def view(from: Int, until: Int) = view.slice(from, until)

  /* Need to override string, so that it's not the Function1's string that gets mixed in.
   */
  override def toString = super[IterableLike].toString
}

/** The companion object for trait `SeqLike`.
 */
object SeqLike {
  // KMP search utilities

  /** Make sure a target sequence has fast, correctly-ordered indexing for KMP.
   *
   *  @author Rex Kerr
   *  @since  2.10
   *  @param  W    The target sequence
   *  @param  n0   The first element in the target sequence that we should use
   *  @param  n1   The far end of the target sequence that we should use (exclusive)
   *  @return Target packed in an IndexedSeq (taken from iterator unless W already is an IndexedSeq)
   */
  private def kmpOptimizeWord[B](W: Seq[B], n0: Int, n1: Int, forward: Boolean) = W match {
    case iso: IndexedSeq[_] =>
      // Already optimized for indexing--use original (or custom view of original)
      if (forward && n0==0 && n1==W.length) iso.asInstanceOf[IndexedSeq[B]]
      else if (forward) new AbstractSeq[B] with IndexedSeq[B] {
        val length = n1 - n0
        def apply(x: Int) = iso(n0 + x).asInstanceOf[B]
      }
      else new AbstractSeq[B] with IndexedSeq[B] {
        def length = n1 - n0
        def apply(x: Int) = iso(n1 - 1 - x).asInstanceOf[B]
      }
    case _ =>
      // W is probably bad at indexing.  Pack in array (in correct orientation)
      // Would be marginally faster to special-case each direction
      new AbstractSeq[B] with IndexedSeq[B] {
        private[this] val Warr = new Array[AnyRef](n1-n0)
        private[this] val delta = if (forward) 1 else -1
        private[this] val done = if (forward) n1-n0 else -1
        val wit = W.iterator.drop(n0)
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
   *  @param  Wopt The target sequence, as at least an IndexedSeq
   *  @param  wlen Just in case we're only IndexedSeq and not IndexedSeqOptimized
   *  @return KMP jump table for target sequence
   */
 private def kmpJumpTable[B](Wopt: IndexedSeq[B], wlen: Int) = {
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
        val iter = S.iterator.drop(m0)
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
          if (Wopt(i) == cache((i+m)%(n1-n0))) {
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

  /** Finds a particular index at which one sequence occurs in another sequence.
   *  Both the source sequence and the target sequence are expressed in terms
   *  other sequences S' and T' with offset and length parameters.  This
   *  function is designed to wrap the KMP machinery in a sufficiently general
   *  way that all library sequence searches can use it.  It is unlikely you
   *  have cause to call it directly: prefer functions such as StringBuilder#indexOf
   *  and Seq#lastIndexOf.
   *
   *  @param  source        the sequence to search in
   *  @param  sourceOffset  the starting offset in source
   *  @param  sourceCount   the length beyond sourceOffset to search
   *  @param  target        the sequence being searched for
   *  @param  targetOffset  the starting offset in target
   *  @param  targetCount   the length beyond targetOffset which makes up the target string
   *  @param  fromIndex     the smallest index at which the target sequence may start
   *
   *  @return the applicable index in source where target exists, or -1 if not found
   */
  def indexOf[B](
    source: Seq[B], sourceOffset: Int, sourceCount: Int,
    target: Seq[B], targetOffset: Int, targetCount: Int,
    fromIndex: Int
  ): Int = {
    // Fiddle with variables to match previous behavior and use kmpSearch
    // Doing LOTS of max/min, both clearer and faster to use math._
    val slen        = source.length
    val clippedFrom = math.max(0, fromIndex)
    val s0          = math.min(slen, sourceOffset + clippedFrom)
    val s1          = math.min(slen, s0 + sourceCount)
    val tlen        = target.length
    val t0          = math.min(tlen, targetOffset)
    val t1          = math.min(tlen, t0 + targetCount)

    // Error checking
    if (clippedFrom > slen-sourceOffset) -1   // Cannot return an index in range
    else if (t1 - t0 < 1) s0                  // Empty, matches first available position
    else if (s1 - s0 < t1 - t0) -1            // Source is too short to find target
    else {
      // Nontrivial search
      val ans = kmpSearch(source, s0, s1, target, t0, t1, forward = true)
      if (ans < 0) ans else ans - math.min(slen, sourceOffset)
    }
  }

  /** Finds a particular index at which one sequence occurs in another sequence.
   *  Like `indexOf`, but finds the latest occurrence rather than earliest.
   *
   *  @see  [[scala.collection.SeqLike]], method `indexOf`
   */
  def lastIndexOf[B](
    source: Seq[B], sourceOffset: Int, sourceCount: Int,
    target: Seq[B], targetOffset: Int, targetCount: Int,
    fromIndex: Int
  ): Int = {
    // Fiddle with variables to match previous behavior and use kmpSearch
    // Doing LOTS of max/min, both clearer and faster to use math._
    val slen        = source.length
    val tlen        = target.length
    val s0          = math.min(slen, sourceOffset)
    val s1          = math.min(slen, s0 + sourceCount)
    val clippedFrom = math.min(s1 - s0, fromIndex)
    val t0          = math.min(tlen, targetOffset)
    val t1          = math.min(tlen, t0 + targetCount)
    val fixed_s1    = math.min(s1, s0 + clippedFrom + (t1 - t0) - 1)

    // Error checking
    if (clippedFrom < 0) -1                   // Cannot return an index in range
    else if (t1 - t0 < 1) s0+clippedFrom      // Empty, matches last available position
    else if (fixed_s1 - s0 < t1 - t0) -1      // Source is too short to find target
    else {
      // Nontrivial search
      val ans = kmpSearch(source, s0, fixed_s1, target, t0, t1, forward = false)
      if (ans < 0) ans else ans - s0
    }
  }
}
