/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package immutable

import generic._
import BitSetLike.{LogWL, updateArray}
import mutable.Builder

/** A class for immutable bitsets.
 * $bitsetinfo
 *
 * @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#immutable-bitsets"Scala's Collection Library overview"]]
 *      section on `Immutable BitSets` for more information.
 * @define Coll `immutable.BitSet`
 * @define coll immutable bitset
 */
@SerialVersionUID(1611436763290191562L)
abstract class BitSet extends scala.collection.AbstractSet[Int]
  with SortedSet[Int]
  with scala.collection.BitSet
  with BitSetLike[BitSet]
  with Serializable {
  override def empty = BitSet.empty

  override protected[this] def copyOrSelf: BitSet = this

  protected def fromBitMaskNoCopy(elems: Array[Long]): BitSet = BitSet.fromBitMaskNoCopy(elems)

  /** Update word at index `idx`; enlarge set if `idx` outside range of set.
   */
  protected def updateWord(idx: Int, w: Long): BitSet

  /** Adds element to bitset, returning a new set.
   */
  def +(elem: Int): BitSet = {
    require(elem >= 0, "bitset element must be >= 0")
    val idx      = elem >> LogWL
    val bit      = 1L << elem
    val thisWord = word(idx)
    if ((thisWord & bit) != 0L) this
    else updateWord(idx, thisWord | bit)
  }

  /** Removes element from bitset, returning a new set
   */
  def -(elem: Int): BitSet = {
    if (elem < 0) this
    else {
      val idx      = elem >> LogWL
      val bit      = 1L << elem
      val thisWord = word(idx)
      if ((thisWord & bit) == 0L) this
      else updateWord(idx, thisWord ^ bit)
    }
  }
}

/** $factoryInfo
 *
 * @define Coll `immutable.BitSet`
 * @define coll immutable bitset
 */
object BitSet extends BitSetFactory[BitSet] {
  /** The empty bitset */
  def empty: BitSet = _empty

  private object _empty extends BitSet {
    override protected def word(idx: Int): Long = 0
    override def nwords: Int = 0
    override private[collection] def lastWordSet = -1
    override def |(other: collection.BitSet): BitSet =
      if (other.lastWordSet > 1) super.|(other)
      else createSmall(other._word(0), other._word(1))
    override def &(other: collection.BitSet): BitSet = this
    override def &~(other: collection.BitSet): BitSet = this
    override def --(elems: GenTraversableOnce[Int]): BitSet = this
    override def contains(elem: Int): Boolean = false
    override def subsetOf(other: collection.BitSet): Boolean = true
    /** Update word at index `idx`; enlarge set if `idx` outside range of set.
     */
    override protected def updateWord(idx: Int, w: Long): BitSet = {
      if (idx == 0) createSmall(w, 0L)
      else if (idx == 1) createSmall(0L, w)
      else {
        val array = new Array[Long](idx + 1)
        array(idx) = w
        fromBitMaskNoCopy(array)
      }
    }
  }

  private def createSmall(a: Long, b: Long): BitSet =
    if (b != 0L) new BitSet2(a, b)
    else if (a != 0) new BitSet1(a)
    else empty

  /** A builder that takes advantage of mutable BitSets. */
  def newBuilder: Builder[Int, BitSet] = new Builder[Int, BitSet] {
    private[this] val b = new mutable.BitSet
    override def ++=(xs: TraversableOnce[Int]): this.type = {
      b ++= xs; this
    }
    def +=(x: Int) = {
      b += x; this
    }
    def clear() = b.clear()
    def result() = b.toImmutable
  }

  /** $bitsetCanBuildFrom */
  implicit val canBuildFrom: CanBuildFrom[BitSet, Int, BitSet] = bitsetCanBuildFrom

  /** A bitset containing all the bits in an array
   * but trimming zeros from the RHS
   */
  def fromBitMask(elems: Array[Long]): BitSet =
    fromBitMaskArray(elems, false)

  /** A bitset containing all the bits in an array,
   * trimming zeros from the RHS.
   * sharing the underlyingArray if there are no zores on the RHS.
   */
  def fromBitMaskNoCopy(elems: Array[Long]): BitSet =
    fromBitMaskArray(elems, true)

  private def fromBitMaskArray(elems: Array[Long], canShareArray: Boolean): BitSet = {
    var len = elems.length
    while (len > 0 && (elems(len - 1) == 0L)) len -= 1
    if (len == 0) empty
    else if (len == 1) new BitSet1(elems(0))
    else if (len == 2) createSmall(elems(0), elems(1))
    else if (canShareArray && len == elems.length) new BitSetN(elems)
    else new BitSetN(java.util.Arrays.copyOf(elems, len))
  }

  @SerialVersionUID(2260107458435649300L)
  class BitSet1(val elems: Long) extends BitSet {
    protected def nwords = 1
    protected def word(idx: Int) = if (idx == 0) elems else 0L
    protected def updateWord(idx: Int, w: Long): BitSet =
      if (idx == 0)
        if (w == 0L) empty
        else new BitSet1(w)
      else if (idx == 1) createSmall(elems, w)
      else {
        val array = new Array[Long](idx + 1)
        array(0) = elems
        array(idx) = w
        fromBitMaskNoCopy(array)
      }
    override def head: Int =
      if (elems == 0L) throw new NoSuchElementException("Empty BitSet")
      else java.lang.Long.numberOfTrailingZeros(elems)
    override def tail: BitSet =
      if (elems == 0L) throw new NoSuchElementException("Empty BitSet")
      else createSmall(elems ^ java.lang.Long.lowestOneBit(elems), 0L)

    override def |(other: collection.BitSet): BitSet = {
      val lws = other.lastWordSet
      if (lws > 1) super.|(other)
      else {
        val w0 = elems | other._word(0)
        val w1 = other._word(1)
        if (w0 == elems && w1 == 0L) this
        else createSmall(w0, w1)
      }
    }
    override def &(other: collection.BitSet): BitSet = {
      val w0 = elems & other._word(0)
      if (w0 == elems) this
      else createSmall(w0, 0L)
    }
    override def &~(other: collection.BitSet): BitSet = {
      val w0 = elems & ~ other._word(0)
      if (w0 == elems) this
      else createSmall(w0, 0L)
    }
  }

  class BitSet2(val elems0: Long, elems1: Long) extends BitSet {
    protected def nwords = 2
    protected def word(idx: Int) = if (idx == 0) elems0 else if (idx == 1) elems1 else 0L
    protected def updateWord(idx: Int, w: Long): BitSet =
      if (idx == 0) createSmall(w, elems1)
      else if (idx == 1) createSmall(elems0, w)
      else {
        val array = new Array[Long](idx + 1)
        array(0) = elems0
        array(1) = elems1
        array(idx) = w
        fromBitMaskNoCopy(array)
      }
    override def head: Int =
      if (elems0 == 0L) {
        if (elems1 == 0) throw new NoSuchElementException("Empty BitSet")
        64 + java.lang.Long.numberOfTrailingZeros(elems1)
      }
      else java.lang.Long.numberOfTrailingZeros(elems0)
    override def tail: BitSet =
      if (elems0 == 0L) {
        if (elems1 == 0L) throw new NoSuchElementException("Empty BitSet")
        createSmall(elems0, elems1 ^ java.lang.Long.lowestOneBit(elems1))
      }
      else createSmall(elems0 ^ java.lang.Long.lowestOneBit(elems0), elems1)

    override def |(other: collection.BitSet): BitSet = {
      val lws = other.lastWordSet
      if (lws > 1) super.|(other)
      else {
        val w0 = elems0 | other._word(0)
        val w1 = elems1 | other._word(1)
        if (w0 == elems0 && w1 == elems1) this
        else createSmall(w0, w1)
      }
    }
    override def &(other: collection.BitSet): BitSet = {
      val w0 = elems0 & other._word(0)
      val w1 = elems1 & other._word(1)
      if (w0 == elems0 && w1 == elems1) this
      else createSmall(w0, w1)
    }
    override def &~(other: collection.BitSet): BitSet = {
      val w0 = elems0 & ~other._word(0)
      val w1 = elems1 & ~other._word(1)
      if (w0 == elems0 && w1 == elems1) this
      else createSmall(w0, w1)
    }
  }

  /** The implementing class for bit sets with elements >= 128 (exceeding
   * the capacity of two long values). The constructor wraps an existing
   * bit mask without copying, thus exposing a mutable part of the internal
   *  implementation. Care needs to be taken not to modify the exposed
   * array.
   */
  class BitSetN(val elems: Array[Long]) extends BitSet {
    protected def nwords = elems.length
    protected def word(idx: Int) = if (idx < nwords) elems(idx) else 0L
    protected def updateWord(idx: Int, w: Long): BitSet = {
      if (idx >= 2 && w == 0) {
        // optimisation - if we are going to create a small bitset then avoid the
        // array copy
        var i = idx - 1
        while (i >= 2 && elems(i) == 0L) i -= 1
        if (i == 1) return createSmall(elems(0), elems(1))
      }
      fromBitMaskNoCopy(updateArray(elems, idx, w))
    }
    override def tail: BitSet = {
      val n = nwords
      var i = 0
      while (i < n) {
        val wi = word(i)
        if (wi != 0L) return updateWord(i, wi ^ java.lang.Long.lowestOneBit(wi))
        i += 1
      }
      throw new NoSuchElementException("Empty BitSet")
    }
    override def |(other: collection.BitSet): BitSet = if (other eq this) this else {
      var idx       = other.lastWordSet
      var canBeThis = idx < elems.length
      while (idx >= 0 && canBeThis) {
        canBeThis = (other._word(idx) | elems(idx)) == elems(idx)
        idx -= 1
      }
      if (canBeThis) this
      else super.|(other)
    }
    override def &(other: collection.BitSet): BitSet = if (other eq this) this else {
      var idx       = lastWordSet
      var canBeThis = true
      while (idx >= 0 && canBeThis) {
        canBeThis = (other._word(idx) & elems(idx)) == elems(idx)
        idx -= 1
      }
      if (canBeThis) this
      else super.&(other)
    }
    override def &~(other: collection.BitSet): BitSet = if (other eq this) this else {
      var idx       = lastWordSet
      var canBeThis = true
      while (idx >= 0 && canBeThis) {
        canBeThis = (other._word(idx) & ~elems(idx)) == elems(idx)
        idx -= 1
      }
      if (canBeThis) this
      else super.&~(other)
    }
  }

}
