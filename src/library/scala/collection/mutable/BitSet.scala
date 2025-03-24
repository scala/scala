/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package mutable

import scala.collection.immutable.Range
import BitSetOps.{LogWL, MaxSize}
import scala.annotation.implicitNotFound

/**
  * A class for mutable bitsets.
  *
  * $bitsetinfo
  *
  * @see [[https://docs.scala-lang.org/overviews/collections-2.13/concrete-mutable-collection-classes.html#mutable-bitsets "Scala's Collection Library overview"]]
  * section on `Mutable Bitsets` for more information.
  *
  * @define Coll `BitSet`
  * @define coll bitset
  * @define orderDependent
  * @define orderDependentFold
  * @define mayNotTerminateInf
  * @define willNotTerminateInf
  */
class BitSet(protected[collection] final var elems: Array[Long])
  extends AbstractSet[Int]
    with SortedSet[Int]
    with SortedSetOps[Int, SortedSet, BitSet]
    with StrictOptimizedIterableOps[Int, Set, BitSet]
    with StrictOptimizedSortedSetOps[Int, SortedSet, BitSet]
    with collection.BitSet
    with collection.BitSetOps[BitSet]
    with Serializable {

  def this(initSize: Int) = this(new Array[Long](math.max((initSize + 63) >> 6, 1)))

  def this() = this(0)

  override protected def fromSpecific(coll: IterableOnce[Int]): BitSet = bitSetFactory.fromSpecific(coll)
  override protected def newSpecificBuilder: Builder[Int, BitSet] = bitSetFactory.newBuilder
  override def empty: BitSet = bitSetFactory.empty

  def bitSetFactory: BitSet.type = BitSet

  override def unsorted: Set[Int] = this

  protected[collection] final def nwords: Int = elems.length

  protected[collection] final def word(idx: Int): Long =
    if (idx < nwords) elems(idx) else 0L

  protected[collection] def fromBitMaskNoCopy(elems: Array[Long]): BitSet =
    if (elems.length == 0) empty
    else new BitSet(elems)

  def addOne(elem: Int): this.type = {
    require(elem >= 0)
    if (!contains(elem)) {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) | (1L << elem))
    }
    this
  }

  def subtractOne(elem: Int): this.type = {
    require(elem >= 0)
    if (contains(elem)) {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) & ~(1L << elem))
    }
    this
  }

  def clear(): Unit = {
    elems = new Array[Long](elems.length)
  }

  protected final def updateWord(idx: Int, w: Long): Unit = {
    ensureCapacity(idx)
    elems(idx) = w
  }

  protected final def ensureCapacity(idx: Int): Unit = {
    require(idx < MaxSize)
    if (idx >= nwords) {
      var newlen = nwords
      while (idx >= newlen) newlen = math.min(newlen * 2, MaxSize)
      val elems1 = new Array[Long](newlen)
      Array.copy(elems, 0, elems1, 0, nwords)
      elems = elems1
    }
  }

  def unconstrained: collection.Set[Int] = this

  /** Updates this bitset to the union with another bitset by performing a bitwise "or".
    *
    *  @param   other  the bitset to form the union with.
    *  @return  the bitset itself.
    */
  def |= (other: collection.BitSet): this.type = {
    ensureCapacity(other.nwords - 1)
    var i = 0
    val othernwords = other.nwords
    while (i < othernwords) {
      elems(i) = elems(i) | other.word(i)
      i += 1
    }
    this
  }
  /** Updates this bitset to the intersection with another bitset by performing a bitwise "and".
    *
    *  @param   other  the bitset to form the intersection with.
    *  @return  the bitset itself.
    */
  def &= (other: collection.BitSet): this.type = {
    // Different from other operations: no need to ensure capacity because
    // anything beyond the capacity is 0.  Since we use other.word which is 0
    // off the end, we also don't need to make sure we stay in bounds there.
    var i = 0
    val thisnwords = nwords
    while (i < thisnwords) {
      elems(i) = elems(i) & other.word(i)
      i += 1
    }
    this
  }
  /** Updates this bitset to the symmetric difference with another bitset by performing a bitwise "xor".
    *
    *  @param   other  the bitset to form the symmetric difference with.
    *  @return  the bitset itself.
    */
  def ^= (other: collection.BitSet): this.type = {
    ensureCapacity(other.nwords - 1)
    var i = 0
    val othernwords = other.nwords
    while (i < othernwords) {

      elems(i) = elems(i) ^ other.word(i)
      i += 1
    }
    this
  }
  /** Updates this bitset to the difference with another bitset by performing a bitwise "and-not".
    *
    *  @param   other  the bitset to form the difference with.
    *  @return  the bitset itself.
    */
  def &~= (other: collection.BitSet): this.type = {
    var i = 0
    val max = Math.min(nwords, other.nwords)
    while (i < max) {
      elems(i) = elems(i) & ~other.word(i)
      i += 1
    }
    this
  }

  override def clone(): BitSet = new BitSet(java.util.Arrays.copyOf(elems, elems.length))

  def toImmutable: immutable.BitSet = immutable.BitSet.fromBitMask(elems)

  override def map(f: Int => Int): BitSet = strictOptimizedMap(newSpecificBuilder, f)
  override def map[B](f: Int => B)(implicit @implicitNotFound(collection.BitSet.ordMsg) ev: Ordering[B]): SortedSet[B] =
    super[StrictOptimizedSortedSetOps].map(f)

  override def flatMap(f: Int => IterableOnce[Int]): BitSet = strictOptimizedFlatMap(newSpecificBuilder, f)
  override def flatMap[B](f: Int => IterableOnce[B])(implicit @implicitNotFound(collection.BitSet.ordMsg) ev: Ordering[B]): SortedSet[B] =
    super[StrictOptimizedSortedSetOps].flatMap(f)

  override def collect(pf: PartialFunction[Int, Int]): BitSet = strictOptimizedCollect(newSpecificBuilder, pf)
  override def collect[B](pf: scala.PartialFunction[Int, B])(implicit @implicitNotFound(collection.BitSet.ordMsg) ev: Ordering[B]): SortedSet[B] =
    super[StrictOptimizedSortedSetOps].collect(pf)

  // necessary for disambiguation
  override def zip[B](that: IterableOnce[B])(implicit @implicitNotFound(collection.BitSet.zipOrdMsg) ev: Ordering[(Int, B)]): SortedSet[(Int, B)] =
    super.zip(that)

  override def addAll(xs: IterableOnce[Int]): this.type = xs match {
    case bs: collection.BitSet =>
      this |= bs
    case range: Range =>
      if (range.nonEmpty) {
        val start = range.min
        if (start >= 0) {
          val end = range.max
          val endIdx = end >> LogWL
          ensureCapacity(endIdx)

          if (range.step == 1 || range.step == -1) {
            val startIdx = start >> LogWL
            val wordStart = startIdx * BitSetOps.WordLength
            val wordMask = -1L << (start - wordStart)

            if (endIdx > startIdx) {
              elems(startIdx) |= wordMask
              java.util.Arrays.fill(elems, startIdx + 1, endIdx, -1L)
              elems(endIdx) |= -1L >>> (BitSetOps.WordLength - (end - endIdx * BitSetOps.WordLength) - 1)
            } else elems(endIdx) |= (wordMask & (-1L >>> (BitSetOps.WordLength - (end - wordStart) - 1)))
          } else super.addAll(range)
        } else super.addAll(range)
      }
      this

    case sorted: collection.SortedSet[Int] =>
      // if `sorted` is using the regular Int ordering, ensure capacity for the largest
      // element up front to avoid multiple resizing allocations
      if (sorted.nonEmpty) {
        val ord = sorted.ordering
        if (ord eq Ordering.Int) {
          ensureCapacity(sorted.lastKey >> LogWL)
        } else if (ord eq Ordering.Int.reverse) {
          ensureCapacity(sorted.firstKey >> LogWL)
        }
        val iter = sorted.iterator
        while (iter.hasNext) {
          addOne(iter.next())
        }
      }

      this

    case other =>
      super.addAll(other)
  }

  override def subsetOf(that: collection.Set[Int]): Boolean = that match {
    case bs: collection.BitSet =>
      val thisnwords = this.nwords
      val bsnwords = bs.nwords
      val minWords = Math.min(thisnwords, bsnwords)

      // if any bits are set to `1` in words out of range of `bs`, then this is not a subset. Start there
      var i = bsnwords
      while (i < thisnwords) {
        if (word(i) != 0L) return false
        i += 1
      }

      // the higher range of `this` is all `0`s, fall back to lower range
      var j = 0
      while (j < minWords) {
        if ((word(j) & ~bs.word(j)) != 0L) return false
        j += 1
      }

      true
    case other =>
      super.subsetOf(other)
  }

  override def subtractAll(xs: IterableOnce[Int]): this.type = xs match {
    case bs: collection.BitSet => this &~= bs
    case other => super.subtractAll(other)
  }

  protected[this] def writeReplace(): AnyRef = new BitSet.SerializationProxy(this)

  override def diff(that: collection.Set[Int]): BitSet = that match {
    case bs: collection.BitSet =>
      /*
        * Algorithm:
        *
        * We iterate, word-by-word, backwards from the shortest of the two bitsets (this, or bs) i.e. the one with
        * the fewer words.
        *
        * Array Shrinking:
        * If `this` is not longer than `bs`, then since we must iterate through the full array of words,
        * we can track the new highest index word which is non-zero, at little additional cost. At the end, the new
        * Array[Long] allocated for the returned BitSet will only be of size `maxNonZeroIndex + 1`
        */

      val bsnwords = bs.nwords
      val thisnwords = nwords
      if (bsnwords >= thisnwords) {
        // here, we may have opportunity to shrink the size of the array
        // so, track the highest index which is non-zero. That ( + 1 ) will be our new array length
        var i = thisnwords - 1
        var currentWord = 0L

        while (i >= 0 && currentWord == 0L) {
          val oldWord = word(i)
          currentWord = oldWord & ~bs.word(i)
          i -= 1
        }

        if (i < 0) {
          fromBitMaskNoCopy(Array(currentWord))
        } else {
          val minimumNonZeroIndex: Int = i + 1
          val newArray = elems.take(minimumNonZeroIndex + 1)
          newArray(i + 1) = currentWord
          while (i >= 0) {
            newArray(i) = word(i) & ~bs.word(i)
            i -= 1
          }
          fromBitMaskNoCopy(newArray)
        }
      } else {
        // here, there is no opportunity to shrink the array size, no use in tracking highest non-zero index
        val newElems = elems.clone()
        var i = bsnwords - 1
        while (i >= 0) {
          newElems(i) = word(i) & ~bs.word(i)
          i -= 1
        }
        fromBitMaskNoCopy(newElems)
      }
    case _ => super.diff(that)
  }

  override def filterImpl(pred: Int => Boolean, isFlipped: Boolean): BitSet = {
    // We filter the BitSet from highest to lowest, so we can determine exactly the highest non-zero word
    // index which lets us avoid:
    // * over-allocating -- the resulting array will be exactly the right size
    // * multiple resizing allocations -- the array is allocated one time, not log(n) times.
    var i = nwords - 1
    var newArray: Array[Long] = null
    while (i >= 0) {
      val w = BitSetOps.computeWordForFilter(pred, isFlipped, word(i), i)
      if (w != 0L) {
        if (newArray eq null) {
          newArray = new Array(i + 1)
        }
        newArray(i) = w
      }
      i -= 1
    }
    if (newArray eq null) {
      empty
    } else {
      fromBitMaskNoCopy(newArray)
    }
  }

  override def filterInPlace(p: Int => Boolean): this.type = {
    val thisnwords = nwords
    var i = 0
    while (i < thisnwords) {
      elems(i) = BitSetOps.computeWordForFilter(p, isFlipped = false, elems(i), i)
      i += 1
    }
    this
  }

  override def toBitMask: Array[Long] = elems.clone()
}

@SerialVersionUID(3L)
object BitSet extends SpecificIterableFactory[Int, BitSet] {

  def fromSpecific(it: scala.collection.IterableOnce[Int]): BitSet = Growable.from(empty, it)

  def empty: BitSet = new BitSet()

  def newBuilder: Builder[Int, BitSet] = new GrowableBuilder(empty)

  /** A bitset containing all the bits in an array */
  def fromBitMask(elems: Array[Long]): BitSet = {
    val len = elems.length
    if (len == 0) empty
    else {
      val a = java.util.Arrays.copyOf(elems, len)
      new BitSet(a)
    }
  }

  /** A bitset containing all the bits in an array, wrapping the existing
    *  array without copying.
    */
  def fromBitMaskNoCopy(elems: Array[Long]): BitSet = {
    val len = elems.length
    if (len == 0) empty
    else new BitSet(elems)
  }

  @SerialVersionUID(3L)
  private final class SerializationProxy(coll: BitSet) extends scala.collection.BitSet.SerializationProxy(coll) {
    protected[this] def readResolve(): Any = BitSet.fromBitMaskNoCopy(elems)
  }
}
