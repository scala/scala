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
package mutable

import generic._
import BitSetLike.{LogWL, MaxSize}

/** A class for mutable bitsets.
 *
 *  $bitsetinfo
 *
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#mutable-bitsets "Scala's Collection Library overview"]]
 *  section on `Mutable Bitsets` for more information.
 *
 *  @define Coll `BitSet`
 *  @define coll bitset
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is always `BitSet[B]` because an implicit of type `CanBuildFrom[BitSet, B, BitSet]`
 *    is defined in object `BitSet`.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`. This is usually the `canBuildFrom` value
 *    defined in object `BitSet`.
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(8483111450368547763L)
class BitSet(protected final var elems: Array[Long]) extends AbstractSet[Int]
                                                  with SortedSet[Int]
                                                  with scala.collection.BitSet
                                                  with BitSetLike[BitSet]
                                                  with SetLike[Int, BitSet]
                                                  with Serializable {

  override def empty = BitSet.empty

  /** Creates the bitset of a certain initial size.
   *
   *  @param initSize    initial size of the bitset.
   */
  def this(initSize: Int) = this(new Array[Long]((initSize + 63) >> 6 max 1))

  def this() = this(0)

  @deprecatedOverriding("Internal implementation does not admit sensible overriding of this method.", "2.11.0")
  protected def nwords = elems.length

  @deprecatedOverriding("Internal implementation does not admit sensible overriding of this method.", "2.11.0")
  protected def word(idx: Int): Long =
    if (idx < nwords) elems(idx) else 0L

  protected final def updateWord(idx: Int, w: Long) {
    ensureCapacity(idx)
    elems(idx) = w
  }

  protected final def ensureCapacity(idx: Int) {
    require(idx < MaxSize)
    if (idx >= nwords) {
      var newlen = nwords
      while (idx >= newlen) newlen = (newlen * 2) min MaxSize
      elems = java.util.Arrays.copyOf(elems, newlen)
    }
  }

  protected def fromBitMaskNoCopy(words: Array[Long]): BitSet = {
    if (words.length == 0) {
      empty
    } else {
      new BitSet(words)
    }
  }

  override def add(elem: Int): Boolean = {
    require(elem >= 0)
    val idx = elem >> LogWL
    val bit = (1L << elem)
    ensureCapacity(idx)
    val currentWord = elems(idx)
    val updated = currentWord | bit
    if (currentWord == updated) false
    else {
      elems(idx) = updated
      true
    }
  }

  override def remove(elem: Int): Boolean = {
    // we dont need to check for < 0 as it cant be present
    if (elem < 0) false
    else {
      val idx = elem >> LogWL
      val bitMask = ~(1L << elem)
      if (idx >= nwords) false
      else {
        val currentWord = elems(idx)
        val updated = currentWord & bitMask
        if (currentWord == updated) false
        else {
          elems(idx) = updated
          true
        }
      }
    }
  }

  override def ++=(xs: TraversableOnce[Int]): this.type = xs match {
    case bs: collection.BitSet => this unionWith bs
    case _ => super.++=(xs)
  }

  override def --=(xs: TraversableOnce[Int]): this.type = xs match {
    case bs: collection.BitSet => this diffWith bs
    case _ => super.--=(xs)
  }
  @deprecatedOverriding("Override add to prevent += and add from exhibiting different behavior.", "2.11.0")
  def += (elem: Int): this.type = { add(elem); this }

  @deprecatedOverriding("Override add to prevent += and add from exhibiting different behavior.", "2.11.0")
  def -= (elem: Int): this.type = { remove(elem); this }

  /** Updates this bitset to the union with another bitset by performing a bitwise "or".
   *
   *  @param   other  the bitset to form the union with.
   *  @return  the bitset itself.
   */
  def |= (other: BitSet): this.type = unionWith(other)

  private def unionWith (other: collection.BitSetLike[_]): this.type = {
    val len = other._nwords
    ensureCapacity(len - 1)
    var idx = 0
    while (idx < len) {
      elems(idx) = elems(idx) | other._word(idx)
      idx += 1
    }
    this
  }
  /** Updates this bitset to the intersection with another bitset by performing a bitwise "and".
   *
   *  @param   other  the bitset to form the intersection with.
   *  @return  the bitset itself.
   */
  def &= (other: BitSet): this.type = {
    // Different from other operations: no need to ensure capacity because
    // anything beyond the capacity is 0.  Since we use other.word which is 0
    // off the end, we also don't need to make sure we stay in bounds there.
    val len = Math.min(nwords, other._nwords)
    var idx = 0
    while (idx < len) {
      elems(idx) = elems(idx) & other._word(idx)
      idx += 1
    }
    java.util.Arrays.fill(elems, idx, nwords, 0)
    this
  }
  /** Updates this bitset to the symmetric difference with another bitset by performing a bitwise "xor".
   *
   *  @param   other  the bitset to form the symmetric difference with.
   *  @return  the bitset itself.
   */
  def ^= (other: BitSet): this.type = {
    val len = Math.min(nwords, other.nwords)
    var idx = 0
    while (idx < len) {
      elems(idx) = elems(idx) ^ other.word(idx)
      idx += 1
    }
    this
  }
  /** Updates this bitset to the difference with another bitset by performing a bitwise "and-not".
   *
   *  @param   other  the bitset to form the difference with.
   *  @return  the bitset itself.
   */
  def &~= (other: BitSet): this.type = diffWith(other)

  private def diffWith (other: collection.BitSet): this.type = {
    val len = other._nwords
    ensureCapacity(len - 1)
    var idx = 0
    while (idx < len) {
      elems(idx) = elems(idx) & ~other._word(idx)
      idx += 1
    }
    this
  }

  override def clear() {
    java.util.Arrays.fill(elems, 0L)
  }

  /** Wraps this bitset as an immutable bitset backed by the array of bits
   *  of this bitset.
   *
   *  @note Subsequent changes in this bitset will be reflected in the returned immutable bitset.
   *
   *  @return an immutable set containing all the elements of this set.
   */
  @deprecated("If this BitSet contains a value that is 128 or greater, the result of this method is an 'immutable' " +
    "BitSet that shares state with this mutable BitSet. Thus, if the mutable BitSet is modified, it will violate the " +
    "immutability of the result.", "2.12.0")
  def toImmutable = immutable.BitSet.fromBitMaskNoCopy(elems)

  override def clone(): BitSet = {
    val elems1 = new Array[Long](elems.length)
    Array.copy(elems, 0, elems1, 0, elems.length)
    new BitSet(elems1)
  }
}

/** $factoryInfo
 *  @define coll bitset
 *  @define Coll `BitSet`
 */
object BitSet extends BitSetFactory[BitSet] {
  def empty: BitSet = new BitSet

  /** A growing builder for mutable Sets. */
  def newBuilder: Builder[Int, BitSet] = new GrowingBuilder[Int, BitSet](empty) {
    override def ++=(xs: TraversableOnce[Int]): this.type = {
      elems ++= xs
      this
    }
  }

  /** $bitsetCanBuildFrom */
  implicit val canBuildFrom: CanBuildFrom[BitSet, Int, BitSet] = bitsetCanBuildFrom

  /** A bitset containing all the bits in an array */
  def fromBitMask(elems: Array[Long]): BitSet = {
    if (elems.length == 0) empty
    else new BitSet(elems.clone)
  }

  /** A bitset containing all the bits in an array, wrapping the existing
   *  array without copying.
   */
  def fromBitMaskNoCopy(elems: Array[Long]): BitSet = {
    if (elems.length == 0) empty
    else new BitSet(elems)
  }
}
