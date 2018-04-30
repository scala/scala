package scala.collection

import java.lang.{Object, String}

import scala.collection.immutable.Range

import scala.annotation.unchecked.uncheckedVariance
import scala.language.higherKinds
import scala.util.hashing.MurmurHash3
import Searching.{SearchResult, Found, InsertionPoint}

/** Base trait for sequence collections
  *
  * @tparam A the element type of the collection
  */
trait Seq[+A]
  extends Iterable[A]
    with PartialFunction[Int, A]
    with SeqOps[A, Seq, Seq[A]]
    with Equals {

  override def iterableFactory: SeqFactory[IterableCC] = Seq

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

  override def hashCode(): Int = MurmurHash3.seqHash(toIterable)

  override def toString(): String = super[Iterable].toString()

}

/**
  * $factoryInfo
  * @define coll sequence
  * @define Coll `Seq`
  */
object Seq extends SeqFactory.Delegate[Seq](immutable.Seq)

/** Base trait for Seq operations
  *
  * @tparam A the element type of the collection
  * @tparam CC type constructor of the collection (e.g. `List`, `Set`). Operations returning a collection
  *             with a different type of element `B` (e.g. `map`) return a `CC[B]`.
  * @tparam C  type of the collection (e.g. `List[Int]`, `String`, `BitSet`). Operations returning a collection
  *             with the same type of element (e.g. `drop`, `filter`) return a `C`.
  * @define orderDependent
  * @define orderDependentFold
  * @define mayNotTerminateInf
  *
  *    Note: may not terminate for infinite-sized collections.
  *
  * @define willNotTerminateInf
  *
  *    Note: will not terminate for infinite-sized collections.
  *
  * @define coll sequence
  * @define Coll `Seq`
  */
trait SeqOps[+A, +CC[_], +C] extends Any
  with IterableOps[A, CC, C] { self =>

  override def view: SeqView[A] = new SeqView.Id[A](this)

  /** Get the element at the specified index. This operation is provided for convenience in `Seq`. It should
    * not be assumed to be efficient unless you have an `IndexedSeq`. */
  @throws[IndexOutOfBoundsException]
  def apply(i: Int): A

  /** The length (number of elements) of the $coll. `size` is an alias for `length` in `Seq` collections. */
  def length: Int

  /**
    * @return This collection as a `Seq[A]`. This is equivalent to `to(Seq)` but might be faster.
    */
  def toSeq: immutable.Seq[A]

  /** A copy of the $coll with an element prepended.
    *
    * Also, the original $coll is not modified, so you will want to capture the result.
    *
    *    Example:
    *    {{{
    *      scala> val x = List(1)
    *      x: List[Int] = List(1)
    *
    *      scala> val y = 2 +: x
    *      y: List[Int] = List(2, 1)
    *
    *      scala> println(x)
    *      List(1)
    *    }}}
    *
    *  @param  elem   the prepended element
    *  @tparam B      the element type of the returned $coll.
    *
    *    @return a new $coll consisting of `value` followed
    *            by all elements of this $coll.
    */
  def prepended[B >: A](elem: B): CC[B] = fromIterable(new View.Prepended(elem, this))

  /** Alias for `prepended`.
    *
    * Note that :-ending operators are right associative (see example).
    * A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
    */
  @`inline` final def +: [B >: A](elem: B): CC[B] = prepended(elem)

  /** A copy of this $coll with an element appended.
    *
    * $willNotTerminateInf
    *
    * Example:
    * {{{
    *    scala> val a = List(1)
    *    a: List[Int] = List(1)
    *
    *    scala> val b = a :+ 2
    *    b: List[Int] = List(1, 2)
    *
    *    scala> println(a)
    *    List(1)
    * }}}
    *
    * @param  elem   the appended element
    * @tparam B      the element type of the returned $coll.
    * @return a new $coll consisting of
    *         all elements of this $coll followed by `value`.
    */
  def appended[B >: A](elem: B): CC[B] = fromIterable(new View.Appended(this, elem))

  /** Alias for `appended`
    *
    * Note that :-ending operators are right associative (see example).
    * A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
    */
  @`inline` final def :+ [B >: A](elem: B): CC[B] = appended(elem)

  /** As with `:++`, returns a new collection containing the elements from the left operand followed by the
    *  elements from the right operand.
    *
    *  It differs from `:++` in that the right operand determines the type of
    *  the resulting collection rather than the left one.
    *  Mnemonic: the COLon is on the side of the new COLlection type.
    *
    *  @param prefix   the iterable to prepend.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new $coll which contains all elements of `prefix` followed
    *                  by all the elements of this $coll.
    */
  def prependedAll[B >: A](prefix: Iterable[B]): CC[B] = fromIterable(new View.Concat(prefix, this))

  /** Alias for `prependedAll` */
  @`inline` final def ++: [B >: A](prefix: Iterable[B]): CC[B] = prependedAll(prefix)

  /** Returns a new $coll containing the elements from the left hand operand followed by the elements from the
    *  right hand operand. The element type of the $coll is the most specific superclass encompassing
    *  the element types of the two operands.
    *
    *  @param suffix the iterable to append.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new collection of type `CC[B]` which contains all elements
    *                of this $coll followed by all elements of `suffix`.
    */
  def appendedAll[B >: A](suffix: Iterable[B]): CC[B] = super.concat(suffix)

  /** Alias for `appendedAll` */
  @`inline` final def :++ [B >: A](suffix: Iterable[B]): CC[B] = appendedAll(suffix)

  // Make `concat` an alias for `appendedAll` so that it benefits from performance
  // overrides of this method
  @`inline` final override def concat[B >: A](suffix: Iterable[B]): CC[B] = appendedAll(suffix)

 /** Produces a new sequence which contains all elements of this $coll and also all elements of
   *  a given sequence. `xs union ys`  is equivalent to `xs ++ ys`.
   *
   *  @param that   the sequence to add.
   *  @tparam B     the element type of the returned $coll.
   *  @return       a new collection which contains all elements of this $coll
   *                followed by all elements of `that`.
   */
  @deprecated("Use `concat` instead", "2.13.0")
  @inline final def union[B >: A, That](that: Seq[B]): CC[B] = concat(that)

  final override def size: Int = length

  /** Selects all the elements of this $coll ignoring the duplicates.
    *
    * @return a new $coll consisting of all the elements of this $coll without duplicates.
    */
  def distinct: C = distinctBy(identity)

  /** Selects all the elements of this $coll ignoring the duplicates as determined by `==` after applying
    * the transforming function `f`.
    *
    * @param f The transforming function whose result is used to determine the uniqueness of each element
    * @tparam B the type of the elements after being transformed by `f`
    * @return a new $coll consisting of all the elements of this $coll without duplicates.
    */
  def distinctBy[B](f: A => B): C = fromSpecificIterable(new View.DistinctBy(this, f))

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
  def startsWith[B >: A](that: IterableOnce[B], offset: Int = 0): Boolean = {
    val i = iterator() drop offset
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
  def endsWith[B >: A](that: Iterable[B]): Boolean = {
    val i = iterator().drop(length - that.size)
    val j = that.iterator()
    while (i.hasNext && j.hasNext)
      if (i.next() != j.next())
        return false

    !j.hasNext
  }

  /** Tests whether this $coll contains given index.
    *
    *  The implementations of methods `apply` and `isDefinedAt` turn a `Seq[A]` into
    *  a `PartialFunction[Int, A]`.
    *
    * @param    idx     the index to test
    * @return   `true` if this $coll contains an element at position `idx`, `false` otherwise.
    */
  def isDefinedAt(idx: Int): Boolean = (idx >= 0) && (idx < length)

  /** A copy of this $coll with an element value appended until a given target length is reached.
   *
   *  @param   len   the target length
   *  @param   elem  the padding value
   *  @tparam B      the element type of the returned $coll.
   *  @return a new $coll consisting of
   *          all elements of this $coll followed by the minimal number of occurrences of `elem` so
   *          that the resulting collection has a length of at least `len`.
   */
  def padTo[B >: A](len: Int, elem: B): CC[B] = fromIterable(new View.PadTo(this, len, elem))

  /** Computes length of longest segment whose elements all satisfy some predicate.
    *
    *  $mayNotTerminateInf
    *
    *  @param   p     the predicate used to test elements.
    *  @param   from  the index where the search starts.
    *  @return  the length of the longest segment of this $coll starting from index `from`
    *           such that every element of the segment satisfies the predicate `p`.
    */
  def segmentLength(p: A => Boolean, from: Int = 0): Int = {
    var i = 0
    val it = iterator().drop(from)
    while (it.hasNext && p(it.next()))
      i += 1
    i
  }

  /** Returns the length of the longest prefix whose elements all satisfy some predicate.
    *
    *  $mayNotTerminateInf
    *
    *  @param   p     the predicate used to test elements.
    *  @return  the length of the longest prefix of this $coll
    *           such that every element of the segment satisfies the predicate `p`.
    */
  @deprecated("Use segmentLength instead of prefixLength", "2.13.0")
  @`inline` final def prefixLength(p: A => Boolean): Int = segmentLength(p, 0)

  /** Finds index of the first element satisfying some predicate after or at some start index.
    *
    *  $mayNotTerminateInf
    *
    *  @param   p     the predicate used to test elements.
    *  @param   from   the start index
    *  @return  the index `>= from` of the first element of this $coll that satisfies the predicate `p`,
    *           or `-1`, if none exists.
    */
  def indexWhere(p: A => Boolean, from: Int = 0): Int = iterator().indexWhere(p, from)

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
    if (that.isEmpty && from == 0) 0
    else {
      val l = knownSize
      val tl = that.knownSize
      if (l >= 0 && tl >= 0) {
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

  @deprecated("Use .reverseIterator().map(f).to(...) instead of .reverseMap(f)", "2.13.0")
  def reverseMap[B](f: A => B): CC[B] = fromIterable(new View.Map(View.fromIteratorProvider(() => reverseIterator()), f))

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
      val (es, is) = (self.toSeq map (e => (e, m.getOrElseUpdate(e, m.size))) sortBy (_._2)).unzip

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
      val (es, is) = (self.toSeq map (e => (e, m.getOrElseUpdate(e, m.size))) sortBy (_._2)).unzip
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
    *  `ord.compare`) appear in the same order in the sorted sequence as in the original.
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

  /** Sorts this $coll according to the Ordering which results from transforming
    * an implicitly given Ordering with a transformation function.
    * $willNotTerminateInf
    *
    * The sort is stable. That is, elements that are equal (as determined by
    * `ord.compare`) appear in the same order in the sorted sequence as in the original.
    *
    *  @see [[scala.math.Ordering]]
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
      val it = iterator()
      while (it.hasNext) {
        if (i == len) return if (it.hasNext) 1 else 0
        it.next()
        i += 1
      }
      i - len
    }
  }

  override def isEmpty: Boolean = lengthCompare(0) == 0

  /** Are the elements of this collection the same (and in the same order)
    * as those of `that`?
    */
  def sameElements[B >: A](that: IterableOnce[B]): Boolean =
    iterator().sameElements(that)

  /** Tests whether every element of this $coll relates to the
    * corresponding element of another sequence by satisfying a test predicate.
    *
    *  @param   that  the other sequence
    *  @param   p     the test predicate, which relates elements from both sequences
    *  @tparam  B     the type of the elements of `that`
    *  @return  `true` if both sequences have the same length and
    *                  `p(x, y)` is `true` for all corresponding elements `x` of this $coll
    *                  and `y` of `that`, otherwise `false`.
    */
  def corresponds[B](that: Seq[B])(p: (A, B) => Boolean): Boolean = {
    val i = iterator()
    val j = that.iterator()
    while (i.hasNext && j.hasNext)
      if (!p(i.next(), j.next()))
        return false
    !i.hasNext && !j.hasNext
  }

  /** Computes the multiset difference between this $coll and another sequence.
    *
    *  @param that   the sequence of elements to remove
    *  @return       a new $coll which contains all elements of this $coll
    *                except some of occurrences of elements that also appear in `that`.
    *                If an element value `x` appears
    *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
    *                part of the result, but any following occurrences will.
    *  $willNotTerminateInf
    */
  def diff(that: Seq[_ >: A]): C = {
    val occ = occCounts(that)
    //TODO diff and intersect could have efficient lazy implementations if fromSpecificIterable accepted an IterableOnce, i.e. it guaranteed doing only a single traversal
    val b = newSpecificBuilder()
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
    *  @return       a new $coll which contains all elements of this $coll
    *                which also appear in `that`.
    *                If an element value `x` appears
    *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
    *                in the result, but any following occurrences will be omitted.
    *  $mayNotTerminateInf
    */
  def intersect(that: Seq[_ >: A]): C = {
    val occ = occCounts(that)
    val b = newSpecificBuilder()
    for (x <- this) {
      val ox = occ(x)  // Avoid multiple map lookups
      if (ox > 0) {
        b += x
        occ(x) = ox - 1
      }
    }
    b.result()
  }

  /** Produces a new $coll where a slice of elements in this $coll is replaced by another sequence.
    *
    * Patching at negative indices is the same as patching starting at 0.
    * Patching at indices at or larger than the length of the original $coll appends the patch to the end.
    * If more values are replaced than actually exist, the excess is ignored.
    *
    *  @param  from     the index of the first replaced element
    *  @param  other    the replacement sequence
    *  @param  replaced the number of elements to drop in the original $coll
    *  @tparam B        the element type of the returned $coll.
    *  @return          a new $coll consisting of all elements of this $coll
    *                   except that `replaced` elements starting from `from` are replaced
    *                   by all the elements of `other`.
    */
  def patch[B >: A](from: Int, other: IterableOnce[B], replaced: Int): CC[B] =
    fromIterable(new View.Patched(this, from, other, replaced))

  private[this] def occCounts[B](sq: Seq[B]): mutable.Map[B, Int] = {
    val occ = new mutable.HashMap[B, Int] { override def default(k: B) = 0 }
    for (y <- sq) occ(y) += 1
    occ
  }

  /** Search this sorted sequence for a specific element. If the sequence is an
    * `IndexedSeq`, a binary search is used. Otherwise, a linear search is used.
    *
    * The sequence should be sorted with the same `Ordering` before calling; otherwise,
    * the results are undefined.
    *
    * @see [[scala.collection.IndexedSeq]]
    * @see [[scala.math.Ordering]]
    * @see [[scala.collection.SeqOps]], method `sorted`
    *
    * @param elem the element to find.
    * @param ord  the ordering to be used to compare elements.
    *
    * @return a `Found` value containing the index corresponding to the element in the
    *         sequence, or the `InsertionPoint` where the element would be inserted if
    *         the element is not in the sequence.
    */
  def search[B >: A](elem: B)(implicit ord: Ordering[B]): SearchResult =
    linearSearch(view, elem, 0)(ord)

  /** Search within an interval in this sorted sequence for a specific element. If this
    * sequence is an `IndexedSeq`, a binary search is used. Otherwise, a linear search
    * is used.
    *
    * The sequence should be sorted with the same `Ordering` before calling; otherwise,
    * the results are undefined.
    *
    * @see [[scala.collection.IndexedSeq]]
    * @see [[scala.math.Ordering]]
    * @see [[scala.collection.SeqOps]], method `sorted`
    *
    * @param elem the element to find.
    * @param from the index where the search starts.
    * @param to   the index following where the search ends.
    * @param ord  the ordering to be used to compare elements.
    *
    * @return a `Found` value containing the index corresponding to the element in the
    *         sequence, or the `InsertionPoint` where the element would be inserted if
    *         the element is not in the sequence.
    */
  def search[B >: A](elem: B, from: Int, to: Int) (implicit ord: Ordering[B]): SearchResult =
    linearSearch(view.slice(from, to), elem, from)(ord)

  private[this] def linearSearch[B >: A](c: View[A], elem: B, offset: Int)
                                        (implicit ord: Ordering[B]): SearchResult = {
    var idx = offset
    val it = c.iterator()
    while (it.hasNext) {
      val cur = it.next()
      if (ord.equiv(elem, cur)) return Found(idx)
      else if (ord.lt(elem, cur)) return InsertionPoint(idx)
      idx += 1
    }
    InsertionPoint(idx)
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
      if (S.iterator().slice(m0, m1).sameElements(W.iterator().slice(n0, n1))) m0
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
      else if (forward) new AbstractIndexedView[B] {
        val length = n1 - n0
        def apply(x: Int) = iso(n0 + x)
      }
      else new AbstractIndexedView[B] {
        def length = n1 - n0
        def apply(x: Int) = iso(n1 - 1 - x)
      }
    case _ =>
      // W is probably bad at indexing.  Pack in array (in correct orientation)
      // Would be marginally faster to special-case each direction
      new AbstractIndexedView[B] {
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

/** Explicit instantiation of the `Seq` trait to reduce class file size in subclasses. */
abstract class AbstractSeq[+A] extends AbstractIterable[A] with Seq[A]
