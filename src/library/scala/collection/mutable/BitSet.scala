/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable

import generic._
import BitSetLike.{LogWL, MaxSize}

/** A class for mutable bitsets.
 *
 *  $bitsetinfo
 *
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#mutable_bitsets "Scala's Collection Library overview"]]
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
      val elems1 = new Array[Long](newlen)
      Array.copy(elems, 0, elems1, 0, nwords)
      elems = elems1
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
    if (contains(elem)) false
    else {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) | (1L << elem))
      true
    }
  }

  override def remove(elem: Int): Boolean = {
    require(elem >= 0)
    if (contains(elem)) {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) & ~(1L << elem))
      true
    } else false
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
  def |= (other: BitSet): this.type = {
    ensureCapacity(other.nwords - 1)
    for (i <- 0 until other.nwords)
      elems(i) = elems(i) | other.word(i)
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
    for (i <- 0 until nwords)
      elems(i) = elems(i) & other.word(i)
    this
  }
  /** Updates this bitset to the symmetric difference with another bitset by performing a bitwise "xor".
   *
   *  @param   other  the bitset to form the symmetric difference with.
   *  @return  the bitset itself.
   */
  def ^= (other: BitSet): this.type = {
    ensureCapacity(other.nwords - 1)
    for (i <- 0 until other.nwords)
      elems(i) = elems(i) ^ other.word(i)
    this
  }
  /** Updates this bitset to the difference with another bitset by performing a bitwise "and-not".
   *
   *  @param   other  the bitset to form the difference with.
   *  @return  the bitset itself.
   */
  def &~= (other: BitSet): this.type = {
    ensureCapacity(other.nwords - 1)
    for (i <- 0 until other.nwords)
      elems(i) = elems(i) & ~other.word(i)
    this
  }

  override def clear() {
    elems = new Array[Long](elems.length)
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
  def newBuilder: Builder[Int, BitSet] = new GrowingBuilder[Int, BitSet](empty)

  /** $bitsetCanBuildFrom */
  implicit def canBuildFrom: CanBuildFrom[BitSet, Int, BitSet] = bitsetCanBuildFrom

  /** A bitset containing all the bits in an array */
  def fromBitMask(elems: Array[Long]): BitSet = {
    val len = elems.length
    if (len == 0) {
      empty
    } else {
      val a = new Array[Long](len)
      Array.copy(elems, 0, a, 0, len)
      new BitSet(a)
    }
  }

  /** A bitset containing all the bits in an array, wrapping the existing
   *  array without copying.
   */
  def fromBitMaskNoCopy(elems: Array[Long]): BitSet = {
    if (elems.length == 0) {
      empty
    } else {
      new BitSet(elems)
    }
  }
}
