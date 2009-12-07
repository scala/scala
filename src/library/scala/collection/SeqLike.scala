/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import mutable.{ListBuffer, HashMap, GenericArray}
import immutable.{List, Range}
import generic._

/** The companion object for trait `SeqLike`.
 */
object SeqLike {

  /**  A KMP implementation, based on the undoubtedly reliable wikipedia entry.
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

  /** Waiting for a doc comment from Paul
   */
  def indexOf[B](
    source: Seq[B], sourceOffset: Int, sourceCount: Int,
    target: Seq[B], targetOffset: Int, targetCount: Int,
    fromIndex: Int): Int =
      KMP(source.slice(sourceOffset, sourceCount) drop fromIndex, target.slice(targetOffset, targetCount)) match {
        case None    => -1
        case Some(x) => x + fromIndex
      }

  /** Waiting for a doc comment from Paul
   */
  def lastIndexOf[B](
    source: Seq[B], sourceOffset: Int, sourceCount: Int,
    target: Seq[B], targetOffset: Int, targetCount: Int,
    fromIndex: Int): Int = {
      val src = (source.slice(sourceOffset, sourceCount) take fromIndex).reverse
      val tgt = target.slice(targetOffset, targetCount).reverse

      KMP(src, tgt) match {
        case None    => -1
        case Some(x) => (src.length - tgt.length - x) + sourceOffset
      }
    }
}

/** A template trait for sequences of type `Seq[A]`, representing
 *  sequences of elements of type <code>A</code>.
 *
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
 *  Sequences have two principle subtraits, `IndexedSeq` and `LinearSeq`, which give different guarantees for performance.
 *  An `IndexedSeq` provides fast random-access of elements and a fast `length` operation.
 *  A `LinearSeq` provides fast access only to the first element via `head`, but also
 *  has a fast `tail` operation.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 *  @since   2.8
 *
 *  @tparam A    the element type of the collection
 *  @tparam Repr the type of the actual collection containing the elements.
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
 *  @define mayNotTerminateInf
 *
 *    Note: may not terminate for infinite-sized collections.
 *  @define willNotTerminateInf
 *
 *    Note: will not terminate for infinite-sized collections.
 */
trait SeqLike[+A, +Repr] extends IterableLike[A, Repr] { self =>

  override protected[this] def thisCollection: Seq[A] = this.asInstanceOf[Seq[A]]
  override protected[this] def toCollection(repr: Repr): Seq[A] = repr.asInstanceOf[Seq[A]]

  /** The length of the $coll.
   *
   *  $willNotTerminateInf
   *
   *  Note: `xs.length` and `xs.size` yield the same result.
   *
   *  @return     the number of elements in this $coll.
   */
  def length: Int

  /** Selects an element by its index in the $coll
   *
   *  @param  idx  The index to select
   *  @return the element of tyh
   */
  def apply(idx: Int): A

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
   *
   *  @return    the number of elements in this $coll.
   */
  override def size = length

  /** Tests whether this $coll contains given index.
   *
   *  The implementations of methods `apply` and `isDefinedAt` turn a `Seq[A]` into
   *  a `PartialFunction[Int, A]`.
   *
   * @param    idx     the index to test
   * @return   `true` if this $coll contains an element at position `idx`, `false` otherwise.
   */
  def isDefinedAt(idx: Int): Boolean = (idx >= 0) && (idx < length)

  /** Computes length of longest segment whose elements all satisfy some preficate.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @param   from  the index where the search starts.
   *  @return  the length of the longest segment of this $coll starting from index `from`
   *           such that every element of the segment satisfies the predicate `p`.
   */
  def segmentLength(p: A => Boolean, from: Int): Int = {
    var i = 0
    var it = iterator.drop(from)
    while (it.hasNext && p(it.next()))
      i += 1
    i
  }

  /** Returns the length of the longest prefix whose elements all satisfy some preficate.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return  the length of the longest prefix of this $coll
   *           such that every element of the segment satisfies the predicate `p`.
   */
  def prefixLength(p: A => Boolean) = segmentLength(p, 0)

  /** Finds index of first element satisfying some predicate.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return  the index of the first element of this $coll that satisfies the predicate `p`,
   *           or `-1`, if none exists.
   */
  def indexWhere(p: A => Boolean): Int = indexWhere(p, 0)

  /** Finds index of the first element satisfying some predicate after or at some start index.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @param   from   the start index
   *  @return  the index `>= from` of the first element of this $coll that satisfies the predicate `p`,
   *           or `-1`, if none exists.
   */
  def indexWhere(p: A => Boolean, from: Int): Int = {
    var i = from
    var it = iterator.drop(from)
    while (it.hasNext && !p(it.next()))
      i += 1
    if (it.hasNext) i else -1
  }

  /** Returns index of the first element satisying a predicate, or `-1`.
   *  @deprecated "Use `indexWhere` instead"
   */
  def findIndexOf(p: A => Boolean): Int = indexWhere(p)

  /** Finds index of first occurrence of some value in this $coll.
   *
   *  $mayNotTerminateInf
   *
   *  @param   elem   the element value to search for.
   *  @tparam  B      the type of the element `elem`.
   *  @return  the index of the first element of this $coll that is equal (wrt `==`)
   *           tp `elem`, or `-1`, if none exists.
   *
   *  @usecase def indexOf(elem: A): Int
   *
   *  @param   elem   the element value to search for.
   *  @return  the index of the first element of this $coll that is equal (wrt `==`)
   *           to `elem`, or `-1`, if none exists.
   */
  def indexOf[B >: A](elem: B): Int = indexOf(elem, 0)

  /** Finds index of first occurrence of some value in this $coll after or at some start index.
   *
   *  $mayNotTerminateInf
   *
   *  @param   elem   the element value to search for.
   *  @tparam  B      the type of the element `elem`.
   *  @param   from   the start index
   *  @return  the index `>= from` of the first element of this $coll that is equal (wrt `==`)
   *           tp `elem`, or `-1`, if none exists.
   *
   *  @usecase def indexOf(elem: A): Int
   *
   *  @param   elem   the element value to search for.
   *  @param   from   the start index
   *  @return  the index `>= from` of the first element of this $coll that is equal (wrt `==`)
   *           to `elem`, or `-1`, if none exists.
   */
  def indexOf[B >: A](elem: B, from: Int): Int = indexWhere(elem ==, from)

  /** Finds index of last occurrence of some value in this $coll.
   *
   *  $willNotTerminateInf
   *
   *  @param   elem   the element value to search for.
   *  @tparam  B      the type of the element `elem`.
   *  @return  the index of the last element of this $coll that is equal (wrt `==`)
   *           to `elem`, or `-1`, if none exists.
   *
   *  @usecase def lastIndexOf(elem: A): Int
   *
   *  @param   elem   the element value to search for.
   *  @return  the index of the last element of this $coll that is equal (wrt `==`)
   *           to `elem`, or `-1`, if none exists.
   */
  def lastIndexOf[B >: A](elem: B): Int = lastIndexWhere(elem ==)

  /** Finds index of last occurrence of some value in this $coll before or at a given end index.
   *
   *  @param   elem   the element value to search for.
   *  @param   end    the end index.
   *  @tparam  B      the type of the element `elem`.
   *  @return  the index `<= end` of the last element of this $coll that is equal (wrt `==`)
   *           to `elem`, or `-1`, if none exists.
   *
   *  @usecase def lastIndexOf(elem: A): Int
   *
   *  @param   elem   the element value to search for.
   *  @return  the index `<= end` of the last element of this $coll that is equal (wrt `==`)
   *           to `elem`, or `-1`, if none exists.
   */
  def lastIndexOf[B >: A](elem: B, end: Int): Int = lastIndexWhere(elem ==, end)

  /** Finds index of last element satisfying some predicate.
   *
   *  $willNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return  the index of the last element of this $coll that satisfies the predicate `p`,
   *           or `-1`, if none exists.
   */
  def lastIndexWhere(p: A => Boolean): Int = lastIndexWhere(p, length - 1)

  /** Finds index of last element satisfying some predicate before or at given end index.
   *
   *  @param   p     the predicate used to test elements.
   *  @return  the index `<= end` of the last element of this $coll that satisfies the predicate `p`,
   *           or `-1`, if none exists.
   */
  def lastIndexWhere(p: A => Boolean, end: Int): Int = {
    var i = length - 1
    val it = reverseIterator
    while (it.hasNext && { val elem = it.next; (i > end || !p(elem)) }) i -= 1
    i
  }

  /** Returns new $coll wih elements in reversed order.
   *
   *  $willNotTerminateInf
   *
   *  @return A new $coll with all elements of this $coll in reversed order.
   */
  def reverse: Repr = {
    var xs: List[A] = List()
    for (x <- this)
      xs = x :: xs
    val b = newBuilder
    for (x <- xs)
      b += x
    b.result
  }

  /**
   *  Builds a new collection by applying a function to all elements of this $coll and
   *  collecting the results in reversed order.
   *
   *  $willNotTerminateInf
   *
   *  Note: `xs.reverseMap(f)` is the same as `xs.reverse.map(f)` but might be more efficient.
   *
   *  @param f      the function to apply to each element.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` resulting from applying the given function
   *                `f` to each element of this $coll and collecting the results in reversed order.
   *
   *  @usecase def reverseMap[B](f: A => B): $Coll[B]
   *
   *  Note: `xs.reverseMap(f)` is the same as `xs.reverse.map(f)` but might be more efficient.
   *
   *  @param f      the function to apply to each element.
   *  @tparam B     the element type of the returned collection.
   *  @return       a new $coll resulting from applying the given function
   *                `f` to each element of this $coll and collecting the results in reversed order.
   */
  def reverseMap[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    var xs: List[A] = List()
    for (x <- this)
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

  /** @deprecated use `reverseIterator` instead */
  @deprecated("use `reverseIterator' instead")
  def reversedElements = reverseIterator

  /** Checks whether this $coll contains the given sequence at a given index.
   *
   * If the both the receiver object, <code>this</code> and
   * the argument, <code>that</code> are infinite sequences
   * this method may not terminate.
   *
   * @param  that    the candidate sequence
   * @param  offset  the index where the sequence is searched.
   * @return `true` if the sequence `that` is contained in this $coll at index `offset`,
   *         otherwise `false`.
   */
  def startsWith[B](that: Seq[B], offset: Int): Boolean = {
    val i = this.iterator drop offset
    val j = that.iterator
    while (j.hasNext && i.hasNext)
      if (i.next != j.next)
        return false

    !j.hasNext
  }

  /** Checks whether this $coll starts with the given sequence.
   *
   * @param  that    the candidate sequence
   * @return `true` if this collection has `that` as a prefix, `false` otherwise.
   * otherwise false
   */
  def startsWith[B](that: Seq[B]): Boolean = startsWith(that, 0)

  /** @return true if this sequence end with that sequence
   *  @see String.endsWith
   */
  def endsWith[B](that: Seq[B]): Boolean = {
    val i = this.iterator.drop(length - that.length)
    val j = that.iterator
    while (i.hasNext && j.hasNext)
      if (i.next != j.next)
        return false

    !j.hasNext
  }

  /** @return -1 if <code>that</code> not contained in this, otherwise the
   *  first index where <code>that</code> is contained.
   */
  def indexOfSlice[B >: A](that: Seq[B]): Int = indexOfSlice(that, 0)

  def indexOfSlice[B >: A](that: Seq[B], fromIndex: Int): Int =
    if (this.hasDefiniteSize && that.hasDefiniteSize)
      SeqLike.indexOf(thisCollection, 0, length, that, 0, that.length, fromIndex)
    else {
      var i = fromIndex
      var s: Seq[A] = thisCollection drop i
      while (!s.isEmpty) {
        if (s startsWith that)
          return i

        i += 1
        s = s.tail
      }
      -1
    }

  /** @return -1 if <code>that</code> not contained in this, otherwise the
  *  last index where <code>that</code> is contained.
  *  @note may not terminate for infinite-sized collections.
  */
  def lastIndexOfSlice[B >: A](that: Seq[B]): Int = lastIndexOfSlice(that, that.length)

  // since there's no way to find the last index in an infinite sequence,
  // we just document it may not terminate and assume it will.
  def lastIndexOfSlice[B >: A](that: Seq[B], fromIndex: Int): Int =
    SeqLike.lastIndexOf(thisCollection, 0, length, that, 0, that.length, fromIndex)

  /** Tests if the given value <code>elem</code> is a member of this
   *  sequence.
   *
   *  @param elem element whose membership has to be tested.
   *  @return     <code>true</code> iff there is an element of this sequence
   *              which is equal (w.r.t. <code>==</code>) to <code>elem</code>.
   */
  def contains(elem: Any): Boolean = exists (_ == elem)

  /** <p>
   *    Computes the multiset union of this sequence and the given sequence
   *    <code>that</code>. For example:
   *  </p><pre>
   *    <b>val</b> xs = List(1, 1, 2)
   *    <b>val</b> ys = List(1, 2, 2, 3)
   *    println(xs union ys)  // prints "List(1, 1, 2, 1, 2, 2, 3)"
   *    println(ys union xs)  // prints "List(1, 2, 2, 3, 1, 1, 2)"
   *  </pre>
   *
   *  @param that the sequence of elements to add to the sequence.
   *  @return     a sequence containing the elements of this
   *              sequence and those of the given sequence <code>that</code>.
   */
  def union[B >: A, That](that: Seq[B])(implicit bf: CanBuildFrom[Repr, B, That]): That =
    this ++ that

  /** <p>
   *    Computes the multiset difference between this sequence and the
   *    given sequence <code>that</code>. If an element appears more
   *    than once in both sequences, the difference contains <i>m</i> copies
   *    of that element, where <i>m</i> is the difference between the
   *    number of times the element appears in this sequence and the number
   *    of times it appears in <code>that</code>. For example:
   *  </p><pre>
   *    <b>val</b> xs = List(1, 1, 2)
   *    <b>val</b> ys = List(1, 2, 2, 3)
   *    println(xs diff ys)  // prints "List(1)"
   *    println(xs -- ys)    // prints "List()"
   *  </pre>
   *
   *  @param that the sequence of elements to remove from this sequence.
   *  @return     the sequence of elements contained only in this sequence plus
   *              <i>m</i> copies of each element present in both sequences,
   *              where <i>m</i> is defined as above.
   */
  def diff[B >: A, That](that: Seq[B]): Repr = {
    val occ = occCounts(that)
    val b = newBuilder
    for (x <- this)
      if (occ(x) == 0) b += x
      else occ(x) -= 1
    b.result
  }

  /** <p>
   *    Computes the multiset intersection between this sequence and the
   *    given sequence <code>that</code>; the intersection contains <i>m</i>
   *    copies of an element contained in both sequences, where <i>m</i> is
   *    the smaller of the number of times the element appears in this
   *    sequence or in <code>that</code>. For example:
   *  </p><pre>
   *    <b>val</b> xs = List(1, 1, 2)
   *    <b>val</b> ys = List(3, 2, 2, 1)
   *    println(xs intersect ys)  // prints "List(1, 2)"
   *    println(ys intersect xs)  // prints "List(2, 1)"
   *  </pre>
   *
   *  @param that the sequence to intersect.
   *  @return     the sequence of elements contained both in this sequence and
   *              in the given sequence <code>that</code>.
   */
  def intersect[B >: A, That](that: Seq[B]): Repr = {
    val occ = occCounts(that)
    val b = newBuilder
    for (x <- this)
      if (occ(x) > 0) {
        b += x
        occ(x) -= 1
      }
    b.result
  }

  private def occCounts[B](seq: Seq[B]): mutable.Map[B, Int] = {
    val occ = new mutable.HashMap[B, Int] { override def default(k: B) = 0 }
    for (y <- seq) occ(y) += 1
    occ
  }

  /** Builds a new sequence from this sequence in which any duplicates (wrt to ==) removed.
   *  Among duplicate elements, only the first one is retained in the result sequence
   */
  def removeDuplicates: Repr = {
    val b = newBuilder
    var seen = Set[A]() //TR: should use mutable.HashSet?
    for (x <- this) {
      if (!(seen contains x)) {
        b += x
        seen = (seen + x)
      }
    }
    b.result
  }

  /** A new sequence, consisting of all elements of current sequence
   *  except that `replaced` elements starting from `from` are replaced
   *  by `patch`.
   */
  def patch[B >: A, That](from: Int, patch: Seq[B], replaced: Int)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    val (prefix, rest) = this.splitAt(from)
    b ++= toCollection(prefix)
    b ++= patch
    b ++= toCollection(rest).view drop replaced
    b.result
  }

  /** Returns a copy of this sequence with the element at position `index` replaced by `elem`.
   */
  def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    val (prefix, rest) = this.splitAt(index)
    b ++= toCollection(prefix)
    b += elem
    b ++= toCollection(rest).view.tail
    b.result
  }

  /** Returns a new sequence consisting of `elem` followed by the elements of this sequence.
   */
  def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    b += elem
    b ++= thisCollection
    b.result
  }

  /** Returns a new sequence consisting of the elements of this sequence followed by `elem`.
   */
  def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    b ++= thisCollection
    b += elem
    b.result
  }




  /** Returns a new sequence of given length containing the elements of this sequence followed by zero
   *  or more occurrences of given elements.
   */
  def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    b.sizeHint(length max len)
    var diff = len - length
    b ++= thisCollection
    while (diff > 0) {
      b += elem
      diff -=1
    }
    b.result
  }

  /** Sort the sequence according to the comparison function
   *  <code>lt(e1: a, e2: a) =&gt; Boolean</code>,
   *  which should be true iff <code>e1</code> precedes
   *  <code>e2</code> in the desired ordering.
   *  The sort is stable. That is elements that are equal wrt `lt` appear in the
   *  same order in the sorted sequence as in the original.
   *
   *  @param lt the comparison function
   *  @return   a sequence sorted according to the comparison function
   *            <code>lt(e1: a, e2: a) =&gt; Boolean</code>.
   *  @ex <pre>
   *    List("Steve", "Tom", "John", "Bob")
   *      .sortWith((e1, e2) => (e1 compareTo e2) &lt; 0) =
   *    List("Bob", "John", "Steve", "Tom")</pre>
   */
  def sortWith(lt: (A, A) => Boolean): Repr = sortWith(Ordering fromLessThan lt)

  def sortWith[B >: A](ord: Ordering[B]): Repr = {
    val arr = new GenericArray[A](this.length)
    var i = 0
    for (x <- this) {
      arr(i) = x
      i += 1
    }
    java.util.Arrays.sort(
      arr.array, ord.asInstanceOf[Ordering[Object]])
    val b = newBuilder
    for (x <- arr) b += x
    b.result
  }

  /** Sort the sequence according to the Ordering which results from transforming
   *  the implicitly given Ordering[B] to an Ordering[A].  For example:
   *
   *  <code>
   *    val words = "The quick brown fox jumped over the lazy dog".split(' ')
   *    // this works because scala.Ordering will implicitly provide an Ordering[Tuple2[Int, Char]]
   *    words.sortBy(x => (x.length, x.head))
   *    res0: Array[String] = Array(The, dog, fox, the, lazy, over, brown, quick, jumped)
   *  </code>
   *
   *  @param    f   the transformation function A => B
   *  @param    ord the Ordering[B]
   *  @return       the sorted representation
   */
  def sortBy[B](f: A => B)(implicit ord: Ordering[B]): Repr = sortWith(ord on f)

  /**
   *  Overridden for efficiency.
   *
   *  @return  the sequence itself
   */
  override def toSeq: Seq[A] = thisCollection

  /** The range of all indices of this sequence.
   */
  def indices: Range = 0 until length

  override def view = new SeqView[A, Repr] {
    protected lazy val underlying = self.repr
    override def iterator = self.iterator
    override def length = self.length
    override def apply(idx: Int) = self.apply(idx)
  }

  override def view(from: Int, until: Int) = view.slice(from, until)

  override def hashCode() = (Seq.hashSeed /: this)(_ * 41 + _.hashCode)

  override def equals(that: Any): Boolean = that match {
    case that: Seq[_]  => (that canEqual this) && (this sameElements that)
    case _                  => false
  }

  /** Need to override string, so that it's not the Function1's string that gets mixed in.
   */
  override def toString = super[IterableLike].toString

  /** Returns index of the last element satisying a predicate, or -1. */
  @deprecated("use `lastIndexWhere' instead")
  def findLastIndexOf(p: A => Boolean): Int = lastIndexWhere(p)

  @deprecated("Should be replaced by <code>(s1, s2) forall { case (x, y) => f(x, y) }</code>")
  def equalsWith[B](that: Seq[B])(f: (A,B) => Boolean): Boolean = {
    val i = this.iterator
    val j = that.iterator
    while (i.hasNext && j.hasNext)
      if (!f(i.next, j.next))
        return false

    !i.hasNext && !j.hasNext
  }

  /** Is <code>that</code> a slice in this? */
  def containsSlice[B](that: Seq[B]): Boolean = indexOfSlice(that) != -1

 /**
   * returns a projection that can be used to call non-strict <code>filter</code>,
   * <code>map</code>, and <code>flatMap</code> methods that build projections
   * of the collection.
   */
  @deprecated("use `view' instead")
  override def projection = view
}

