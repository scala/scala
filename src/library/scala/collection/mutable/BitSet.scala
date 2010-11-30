/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package mutable

import generic._
import BitSetLike.{LogWL, updateArray}

/** A class for mutable bitsets.
 *
 *  $bitsetinfo
 *
 *  @define Coll BitSet
 *  @define coll bitset
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is always `BitSet[B]` because an implicit of type `CanBuildFrom[BitSet, B, BitSet]`
 *    is defined in object `BitSet`.
 *  @define $bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`. This is usually the `canBuildFrom` value
 *    defined in object `BitSet`.
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(8483111450368547763L)
class BitSet(protected var elems: Array[Long]) extends Set[Int]
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

  protected def nwords = elems.length
  protected def word(idx: Int): Long =
    if (idx < nwords) elems(idx) else 0L

  private def updateWord(idx: Int, w: Long) {
    if (idx >= nwords) {
      var newlen = nwords
      while (idx >= newlen) newlen = newlen * 2
      val elems1 = new Array[Long](newlen)
      Array.copy(elems, 0, elems1, 0, nwords)
      elems = elems1
    }
    elems(idx) = w
  }

  protected def fromArray(words: Array[Long]): BitSet = new BitSet(words)

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

  def += (elem: Int): this.type = { add(elem); this }
  def -= (elem: Int): this.type = { remove(elem); this }

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
  def toImmutable = immutable.BitSet.fromArray(elems)

  override def clone(): BitSet = {
    val elems1 = new Array[Long](elems.length)
    Array.copy(elems, 0, elems1, 0, elems.length)
    new BitSet(elems1)
  }
}

/** $factoryInfo
 *  @define coll bitset
 *  @define Coll BitSet
 */
object BitSet extends BitSetFactory[BitSet] {
  def empty: BitSet = new BitSet

  /** A growing builder for mutable Sets. */
  def newBuilder: Builder[Int, BitSet] = new GrowingBuilder[Int, BitSet](empty)

  /** $bitsetCanBuildFrom */
  implicit def canBuildFrom: CanBuildFrom[BitSet, Int, BitSet] = bitsetCanBuildFrom
}
