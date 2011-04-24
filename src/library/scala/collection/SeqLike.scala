/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection

import mutable.{ListBuffer, HashMap, ArraySeq}
import immutable.{List, Range}
import generic._
import parallel.ParSeq
import annotation.bridge

/** A template trait for sequences of type `Seq[A]`
 *  $seqInfo
 *
 *  @define seqInfo
 *  Sequences are special cases of iterable collections of class `Iterable`.
 *  Unlike iterables, sequences always have a defined order of elements.
 *  Sequences provide a method `apply` for indexing. Indices range from `0` up the the `length` of
 *  a sequence. Sequences support a number to find occurrences of elements or subsequences, including
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
 *  @define Coll Seq
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
trait SeqLike[+A, +Repr] extends IterableLike[A, Repr] with GenSeqLike[A, Repr] with Parallelizable[A, ParSeq[A]] { self =>

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
    var i = 0
    val it = iterator
    while (it.hasNext && i <= len) {
      it.next()
      i += 1
    }
    i - len
  }

  /** The size of this $coll, equivalent to `length`.
   *
   *  $willNotTerminateInf
   */
  override def size = length

  def segmentLength(p: A => Boolean, from: Int): Int = {
    var i = 0
    var it = iterator.drop(from)
    while (it.hasNext && p(it.next()))
      i += 1
    i
  }

  def indexWhere(p: A => Boolean, from: Int): Int = {
    var i = from
    var it = iterator.drop(from)
    while (it.hasNext) {
      if (p(it.next())) return i
      else i += 1
    }

    -1
  }

  /** Returns index of the first element satisfying a predicate, or `-1`.
   */
  @deprecated("Use indexWhere(p) instead.", "2.8.0")
  def findIndexOf(p: A => Boolean): Int = indexWhere(p)

  def lastIndexWhere(p: A => Boolean, end: Int): Int = {
    var i = length - 1
    val it = reverseIterator
    while (it.hasNext && { val elem = it.next; (i > end || !p(elem)) }) i -= 1
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

  /** Iterates over combinations.
   *
   *  @return   An Iterator which traverses the possible n-element combinations of this $coll.
   *  @example  `"abbbc".combinations(2) = Iterator(ab, ac, bb, bc)`
   */
  def combinations(n: Int): Iterator[Repr] =
    if (n < 0 || n > size) Iterator.empty
    else new CombinationsItr(n)

  private class PermutationsItr extends Iterator[Repr] {
    private[this] val (elms, idxs) = init()
    private var _hasNext = true

    def hasNext = _hasNext
    def next: Repr = {
      if (!hasNext)
        Iterator.empty.next

      val result = (self.newBuilder ++= elms).result
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
      var tmpI = idxs(i)
      idxs(i) = idxs(j)
      idxs(j) = tmpI
      var tmpE = elms(i)
      elms(i) = elms(j)
      elms(j) = tmpE
    }

    private[this] def init() = {
      val m = mutable.HashMap[A, Int]()
      val (es, is) = thisCollection map (e => (e, m.getOrElseUpdate(e, m.size))) sortBy (_._2) unzip

      (es.toBuffer, is.toArray)
    }
  }

  private class CombinationsItr(n: Int) extends Iterator[Repr] {
    // generating all nums such that:
    // (1) nums(0) + .. + nums(length-1) = n
    // (2) 0 <= nums(i) <= cnts(i), where 0 <= i <= cnts.length-1
    private val (elms, cnts, nums) = init()
    private val offs = cnts.scanLeft(0)(_ + _)
    private var _hasNext = true

    def hasNext = _hasNext
    def next: Repr = {
      if (!hasNext)
        Iterator.empty.next

      /** Calculate this result. */
      val buf = self.newBuilder
      for(k <- 0 until nums.length; j <- 0 until nums(k))
        buf += elms(offs(k)+j)
      val res = buf.result

      /** Prepare for the next call to next. */
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
      val (es, is) = thisCollection map (e => (e, m.getOrElseUpdate(e, m.size))) sortBy (_._2) unzip
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
    b.result
  }

  def reverseMap[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    var xs: List[A] = List()
    for (x <- this.seq)
      xs = x :: xs
    val b = bf(repr)
    for (x <- xs)
      b += f(x)

    b.result
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

  @deprecated("use `reverseIterator' instead", "2.8.0")
  def reversedElements = reverseIterator

  def startsWith[B](that: GenSeq[B], offset: Int): Boolean = {
    val i = this.iterator drop offset
    val j = that.iterator
    while (j.hasNext && i.hasNext)
      if (i.next != j.next)
        return false

    !j.hasNext
  }

  @bridge
  def startsWith[B](that: Seq[B], offset: Int): Boolean = startsWith(that: GenSeq[B], offset)

  def endsWith[B](that: GenSeq[B]): Boolean = {
    val i = this.iterator.drop(length - that.length)
    val j = that.iterator
    while (i.hasNext && j.hasNext)
      if (i.next != j.next)
        return false

    !j.hasNext
  }

  @bridge
  def endsWith[B](that: Seq[B]): Boolean = endsWith(that: GenSeq[B])


  /** Finds first index where this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @return  the first index such that the elements of this $coll starting at this index
   *           match the elements of sequence `that`, or `-1` of no such subsequence exists.
   */
  def indexOfSlice[B >: A](that: GenSeq[B]): Int = indexOfSlice(that, 0)

  @bridge
  def indexOfSlice[B >: A](that: Seq[B]): Int = indexOfSlice(that: GenSeq[B])

  /** Finds first index after or at a start index where this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @param  from    the start index
   *  @return  the first index `>= from` such that the elements of this $coll starting at this index
   *           match the elements of sequence `that`, or `-1` of no such subsequence exists.
   */
  def indexOfSlice[B >: A](that: GenSeq[B], from: Int): Int =
    if (this.hasDefiniteSize && that.hasDefiniteSize)
      SeqLike.indexOf(thisCollection, 0, length, that.seq, 0, that.length, from)
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

  @bridge
  def indexOfSlice[B >: A](that: Seq[B], from: Int): Int = indexOfSlice(that: GenSeq[B], from)

  /** Finds last index where this $coll contains a given sequence as a slice.
   *  $willNotTerminateInf
   *  @param  that    the sequence to test
   *  @return  the last index such that the elements of this $coll starting a this index
   *           match the elements of sequence `that`, or `-1` of no such subsequence exists.
   */
  def lastIndexOfSlice[B >: A](that: GenSeq[B]): Int = lastIndexOfSlice(that, length)

  @bridge
  def lastIndexOfSlice[B >: A](that: Seq[B]): Int = lastIndexOfSlice(that: GenSeq[B])

  /** Finds last index before or at a given end index where this $coll contains a given sequence as a slice.
   *  @param  that    the sequence to test
   *  @param  end     the end index
   *  @return  the last index `<= end` such that the elements of this $coll starting at this index
   *           match the elements of sequence `that`, or `-1` of no such subsequence exists.
   */
  def lastIndexOfSlice[B >: A](that: GenSeq[B], end: Int): Int =
    SeqLike.lastIndexOf(thisCollection, 0, length, that.seq, 0, that.length, end)

  @bridge
  def lastIndexOfSlice[B >: A](that: Seq[B], end: Int): Int = lastIndexOfSlice(that: GenSeq[B], end)

  /** Tests whether this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @return  `true` if this $coll contains a slice with the same elements
   *           as `that`, otherwise `false`.
   */
  def containsSlice[B](that: GenSeq[B]): Boolean = indexOfSlice(that) != -1

  @bridge
  def containsSlice[B](that: Seq[B]): Boolean = containsSlice(that: GenSeq[B])

  /** Tests whether this $coll contains a given value as an element.
   *  $mayNotTerminateInf
   *
   *  @param elem  the element to test.
   *  @return     `true` if this $coll has an element that is
   *               is equal (wrt `==`) to `elem`, `false` otherwise.
   */
  def contains(elem: Any): Boolean = exists (_ == elem)

  /** Produces a new sequence which contains all elements of this $coll and also all elements of
   *  a given sequence. `xs union ys`  is equivalent to `xs ++ ys`.
   *  $willNotTerminateInf
   *
   *  Another way to express this
   *  is that `xs union ys` computes the order-presevring multi-set union of `xs` and `ys`.
   *  `union` is hence a counter-part of `diff` and `intersect` which also work on multi-sets.
   *
   *  $willNotTerminateInf
   *
   *  @param that   the sequence to add.
   *  @tparam B     the element type of the returned $coll.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` which contains all elements of this $coll
   *                followed by all elements of `that`.
   *  @usecase def union(that: Seq[A]): $Coll[A]
   *  @return       a new $coll which contains all elements of this $coll
   *                followed by all elements of `that`.
   */
  override def union[B >: A, That](that: GenSeq[B])(implicit bf: CanBuildFrom[Repr, B, That]): That =
    this ++ that

  /** Computes the multiset difference between this $coll and another sequence.
   *  $willNotTerminateInf
   *
   *  @param that   the sequence of elements to remove
   *  @tparam B     the element type of the returned $coll.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` which contains all elements of this $coll
   *                except some of occurrences of elements that also appear in `that`.
   *                If an element value `x` appears
   *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
   *                part of the result, but any following occurrences will.
   *  @usecase def diff(that: Seq[A]): $Coll[A]
   *  @return       a new $coll which contains all elements of this $coll
   *                except some of occurrences of elements that also appear in `that`.
   *                If an element value `x` appears
   *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
   *                part of the result, but any following occurrences will.
   */
  def diff[B >: A](that: GenSeq[B]): Repr = {
    val occ = occCounts(that.seq)
    val b = newBuilder
    for (x <- this)
      if (occ(x) == 0) b += x
      else occ(x) -= 1
    b.result
  }

  @bridge
  def diff[B >: A](that: Seq[B]): Repr = diff(that: GenSeq[B])

  /** Computes the multiset intersection between this $coll and another sequence.
   *  $mayNotTerminateInf
   *
   *  @param that   the sequence of elements to intersect with.
   *  @tparam B     the element type of the returned $coll.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` which contains all elements of this $coll
   *                which also appear in `that`.
   *                If an element value `x` appears
   *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
   *                in the result, but any following occurrences will be omitted.
   *  @usecase def intersect(that: Seq[A]): $Coll[A]
   *  @return       a new $coll which contains all elements of this $coll
   *                which also appear in `that`.
   *                If an element value `x` appears
   *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
   *                in the result, but any following occurrences will be omitted.
   */
  def intersect[B >: A](that: GenSeq[B]): Repr = {
    val occ = occCounts(that.seq)
    val b = newBuilder
    for (x <- this)
      if (occ(x) > 0) {
        b += x
        occ(x) -= 1
      }
    b.result
  }

  @bridge
  def intersect[B >: A](that: Seq[B]): Repr = intersect(that: GenSeq[B])

  private def occCounts[B](sq: Seq[B]): mutable.Map[B, Int] = {
    val occ = new mutable.HashMap[B, Int] { override def default(k: B) = 0 }
    for (y <- sq.seq) occ(y) += 1
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
    b.result
  }

  def patch[B >: A, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    val (prefix, rest) = this.splitAt(from)
    b ++= toCollection(prefix)
    b ++= patch.seq
    b ++= toCollection(rest).view drop replaced
    b.result
  }

  @bridge
  def patch[B >: A, That](from: Int, patch: Seq[B], replaced: Int)(implicit bf: CanBuildFrom[Repr, B, That]): That =
    this.patch(from, patch: GenSeq[B], replaced)(bf)

  def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    val (prefix, rest) = this.splitAt(index)
    b ++= toCollection(prefix)
    b += elem
    b ++= toCollection(rest).view.tail
    b.result
  }

  def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    b += elem
    b ++= thisCollection
    b.result
  }

  def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    b ++= thisCollection
    b += elem
    b.result
  }

  def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    b.sizeHint(length max len)
    var diff = len - length
    b ++= thisCollection
    while (diff > 0) {
      b += elem
      diff -= 1
    }
    b.result
  }

  def corresponds[B](that: GenSeq[B])(p: (A,B) => Boolean): Boolean = {
    val i = this.iterator
    val j = that.iterator
    while (i.hasNext && j.hasNext)
      if (!p(i.next, j.next))
        return false

    !i.hasNext && !j.hasNext
  }

  @bridge
  def corresponds[B](that: Seq[B])(p: (A,B) => Boolean): Boolean =
    corresponds(that: GenSeq[B])(p)

  /** Sorts this $coll according to a comparison function.
   *  $willNotTerminateInf
   *
   *  The sort is stable. That is, elements that are equal wrt `lt` appear in the
   *  same order in the sorted sequence as in the original.
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
   *  @see scala.math.Ordering
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
   *  The sort is stable. That is, elements that are equal wrt `lt` appear in the
   *  same order in the sorted sequence as in the original.
   *
   *  @see scala.math.Ordering
   *
   *  @param  ord the ordering to be used to compare elements.
   *  @return     a $coll consisting of the elements of this $coll
   *              sorted according to the ordering `ord`.
   */
  def sorted[B >: A](implicit ord: Ordering[B]): Repr = {
    val len = this.length
    val arr = new ArraySeq[A](len)
    var i = 0
    for (x <- this.seq) {
      arr(i) = x
      i += 1
    }
    java.util.Arrays.sort(arr.array, ord.asInstanceOf[Ordering[Object]])
    val b = newBuilder
    b.sizeHint(len)
    for (x <- arr) b += x
    b.result
  }

  /** Converts this $coll to a sequence.
   *  $willNotTerminateInf
   *
   *  Overridden for efficiency.
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

  /** Returns index of the last element satisfying a predicate, or -1.
   */
  @deprecated("use `lastIndexWhere` instead", "2.8.0")
  def findLastIndexOf(p: A => Boolean): Int = lastIndexWhere(p)

  /** Tests whether every element of this $coll relates to the
   *  corresponding element of another sequence by satisfying a test predicate.
   *
   *  @param   that  the other sequence
   *  @param   p     the test predicate, which relates elements from both sequences
   *  @tparam  B     the type of the elements of `that`
   *  @return  `true` if both sequences have the same length and
   *                  `p(x, y)` is `true` for all corresponding elements `x` of this $coll
   *                  and `y` of `that`, otherwise `false`.
   */
  @deprecated("use `corresponds` instead", "2.8.0")
  def equalsWith[B](that: Seq[B])(f: (A,B) => Boolean): Boolean = corresponds(that)(f)

 /**
   * returns a projection that can be used to call non-strict <code>filter</code>,
   * <code>map</code>, and <code>flatMap</code> methods that build projections
   * of the collection.
   */
  @deprecated("use `view' instead", "2.8.0")
  override def projection = view
}

/** The companion object for trait `SeqLike`.
 */
object SeqLike {
  /**  A KMP implementation, based on the undoubtedly reliable wikipedia entry.
   *
   *  @author paulp
   *  @since  2.8
   */
  private def KMP[B](S: Seq[B], W: Seq[B]): Option[Int] = {
    // trivial cases
    if (W.isEmpty) return Some(0)
    else if (W drop 1 isEmpty) return (S indexOf W(0)) match {
      case -1 => None
      case x  => Some(x)
    }

    val T: Array[Int] = {
      val arr = new Array[Int](W.length)
      var pos = 2
      var cnd = 0
      arr(0) = -1
      arr(1) = 0
      while (pos < W.length) {
        if (W(pos - 1) == W(cnd)) {
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

    var m, i = 0
    def mi = m + i

    while (mi < S.length) {
      if (W(i) == S(mi)) {
        i += 1
        if (i == W.length)
          return Some(m)
      }
      else {
        m = mi - T(i)
        if (i > 0)
          i = T(i)
      }
    }
    None
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
    fromIndex: Int): Int = {
      val toDrop = fromIndex max 0
      val src = source.slice(sourceOffset, sourceCount) drop toDrop
      val tgt = target.slice(targetOffset, targetCount)

      KMP(src, tgt) match {
        case None    => -1
        case Some(x) => x + toDrop
      }
  }

  /** Finds a particular index at which one sequence occurs in another sequence.
   *  Like indexOf, but finds the latest occurrence rather than earliest.
   *
   *  @see  SeqLike#indexOf
   */
  def lastIndexOf[B](
    source: Seq[B], sourceOffset: Int, sourceCount: Int,
    target: Seq[B], targetOffset: Int, targetCount: Int,
    fromIndex: Int): Int = {
      if (fromIndex < 0) return -1
      val toTake = (fromIndex + targetCount) min sourceCount
      // Given seq 1234567 looking for abc, we need to take an extra
      // abc.length chars to examine beyond what is dictated by fromIndex.
      val src = source.slice(sourceOffset, sourceCount) take toTake reverse
      val tgt = target.slice(targetOffset, targetCount).reverse

      // then we reverse the adjustment here on success.
      KMP(src, tgt) match {
        case None    => -1
        case Some(x) => src.length - x - targetCount
      }
    }
}
