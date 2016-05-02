/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package immutable

import generic._
import BitSetLike.{LogWL, updateArray}
import mutable.Builder

/** A class for immutable bitsets.
 *  $bitsetinfo
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#immutable_bitsets "Scala's Collection Library overview"]]
 *  section on `Immutable BitSets` for more information.
 *
 *  @define Coll `immutable.BitSet`
 *  @define coll immutable bitset
 */
@SerialVersionUID(1611436763290191562L)
abstract class BitSet extends scala.collection.AbstractSet[Int]
                         with SortedSet[Int]
                         with scala.collection.BitSet
                         with BitSetLike[BitSet]
                         with Serializable {
  override def empty = BitSet.empty

  protected def fromBitMaskNoCopy(elems: Array[Long]): BitSet = BitSet.fromBitMaskNoCopy(elems)

  /** Update word at index `idx`; enlarge set if `idx` outside range of set.
   */
  protected def updateWord(idx: Int, w: Long): BitSet

  /** Adds element to bitset, returning a new set.
   */
  def + (elem: Int): BitSet = {
    require(elem >= 0, "bitset element must be >= 0")
    if (contains(elem)) this
    else {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) | (1L << elem))
    }
  }

  /** Removes element from bitset, returning a new set
   */
  def - (elem: Int): BitSet = {
    require(elem >= 0, "bitset element must be >= 0")
    if (contains(elem)) {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) & ~(1L << elem))
    } else this
  }
}

/** $factoryInfo
 *  @define Coll `immutable.BitSet`
 *  @define coll immutable bitset
 */
object BitSet extends BitSetFactory[BitSet] {
  /** The empty bitset */
  val empty: BitSet = new BitSet1(0L)

  private def createSmall(a: Long, b: Long): BitSet = if (b == 0L) new BitSet1(a) else new BitSet2(a, b)

  /** A builder that takes advantage of mutable BitSets. */
  def newBuilder: Builder[Int, BitSet] = new Builder[Int, BitSet] {
    private[this] val b = new mutable.BitSet
    def += (x: Int) = { b += x; this }
    def clear() = b.clear()
    def result() = b.toImmutable
  }

  /** $bitsetCanBuildFrom */
  implicit def canBuildFrom: CanBuildFrom[BitSet, Int, BitSet] = bitsetCanBuildFrom

  /** A bitset containing all the bits in an array */
  def fromBitMask(elems: Array[Long]): BitSet = {
    val len = elems.length
    if (len == 0) empty
    else if (len == 1) new BitSet1(elems(0))
    else if (len == 2) createSmall(elems(0), elems(1))
    else {
      val a = new Array[Long](len)
      Array.copy(elems, 0, a, 0, len)
      new BitSetN(a)
    }
  }

  /** A bitset containing all the bits in an array, wrapping the existing
   *  array without copying.
   */
  def fromBitMaskNoCopy(elems: Array[Long]): BitSet = {
    val len = elems.length
    if (len == 0) empty
    else if (len == 1) new BitSet1(elems(0))
    else if (len == 2) createSmall(elems(0), elems(1))
    else new BitSetN(elems)
  }

  @SerialVersionUID(2260107458435649300L)
  class BitSet1(val elems: Long) extends BitSet {
    protected def nwords = 1
    protected def word(idx: Int) = if (idx == 0) elems else 0L
    protected def updateWord(idx: Int, w: Long): BitSet =
      if (idx == 0) new BitSet1(w)
      else if (idx == 1) createSmall(elems, w)
      else fromBitMaskNoCopy(updateArray(Array(elems), idx, w))
    override def head: Int =
      if (elems == 0L) throw new NoSuchElementException("Empty BitSet")
      else java.lang.Long.numberOfTrailingZeros(elems)
    override def tail: BitSet =
      if (elems == 0L) throw new NoSuchElementException("Empty BitSet")
      else new BitSet1(elems - java.lang.Long.lowestOneBit(elems))
  }

  class BitSet2(val elems0: Long, elems1: Long) extends BitSet {
    protected def nwords = 2
    protected def word(idx: Int) = if (idx == 0) elems0 else if (idx == 1) elems1 else 0L
    protected def updateWord(idx: Int, w: Long): BitSet =
      if (idx == 0) new BitSet2(w, elems1)
      else if (idx == 1) createSmall(elems0, w)
      else fromBitMaskNoCopy(updateArray(Array(elems0, elems1), idx, w))
    override def head: Int =
      if (elems0 == 0L) {
        if (elems1 == 0) throw new NoSuchElementException("Empty BitSet")
        64 + java.lang.Long.numberOfTrailingZeros(elems1)
      }
      else java.lang.Long.numberOfTrailingZeros(elems0)
    override def tail: BitSet =
      if (elems0 == 0L) {
        if (elems1 == 0L) throw new NoSuchElementException("Empty BitSet")
        createSmall(elems0, elems1 - java.lang.Long.lowestOneBit(elems1))
      }
      else new BitSet2(elems0 - java.lang.Long.lowestOneBit(elems0), elems1)
  }

  /** The implementing class for bit sets with elements >= 128 (exceeding
   *  the capacity of two long values). The constructor wraps an existing
   *  bit mask without copying, thus exposing a mutable part of the internal
   *  implementation. Care needs to be taken not to modify the exposed
   *  array.
   */
  class BitSetN(val elems: Array[Long]) extends BitSet {
    protected def nwords = elems.length
    protected def word(idx: Int) = if (idx < nwords) elems(idx) else 0L
    protected def updateWord(idx: Int, w: Long): BitSet = fromBitMaskNoCopy(updateArray(elems, idx, w))
    override def tail: BitSet = {
      val n = nwords
      var i = 0
      while (i < n) {
        val wi = word(i)
        if (wi != 0L) return fromBitMaskNoCopy(updateArray(elems, i, wi - java.lang.Long.lowestOneBit(wi)))
        i += 1
      }
      throw new NoSuchElementException("Empty BitSet")
    }
  }
}
