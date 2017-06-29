package strawman
package collection
package immutable

import BitSetOps.{LogWL, updateArray}
import mutable.{Builder, GrowableBuilder}

import scala.{Array, Boolean, Int, Long, Ordering, SerialVersionUID, Serializable, Unit}
import scala.Predef.require

/** A class for immutable bitsets.
  *  $bitsetinfo
  *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#immutable-bitsets "Scala's Collection Library overview"]]
  *  section on `Immutable BitSets` for more information.
  *
  *  @define Coll `immutable.BitSet`
  *  @define coll immutable bitset
  */
@SerialVersionUID(1611436763290191562L)
sealed abstract class BitSet
  extends SortedSet[Int]
    with collection.BitSet
    with SortedSetOps[Int, SortedSet, BitSet]
    with collection.BitSetOps[BitSet]
    with StrictOptimizedIterableOps[Int, BitSet]
    with Serializable {

  def empty: BitSet = BitSet.empty

  def iterableFactory = Set
  def sortedIterableFactory = SortedSet

  protected[this] def fromSpecificIterable(coll: collection.Iterable[Int]): BitSet = BitSet.fromSpecificIterable(coll)
  protected[this] def sortedFromIterable[B : Ordering](it: collection.Iterable[B]): SortedSet[B] = SortedSet.sortedFromIterable(it)
  protected[this] def newSpecificBuilder(): Builder[Int, BitSet] = BitSet.newBuilder()

  protected[collection] def fromBitMaskNoCopy(elems: Array[Long]): BitSet = BitSet.fromBitMaskNoCopy(elems)

  def incl(elem: Int): BitSet = {
    require(elem >= 0, "bitset element must be >= 0")
    if (contains(elem)) this
    else {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) | (1L << elem))
    }
  }

  def excl(elem: Int): BitSet = {
    require(elem >= 0, "bitset element must be >= 0")
    if (contains(elem)) {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) & ~(1L << elem))
    } else this
  }

  def unordered: Set[Int] = this

  /** Update word at index `idx`; enlarge set if `idx` outside range of set.
    */
  protected def updateWord(idx: Int, w: Long): BitSet
}

object BitSet extends SpecificIterableFactory[Int, BitSet] {

  def fromSpecificIterable(it: strawman.collection.Iterable[Int]): BitSet =
    it match {
      case bs: BitSet => bs
      case _          => empty.++(it)
    }

  def empty: BitSet = new BitSet1(0L)

  def newBuilder(): Builder[Int, BitSet] =
    mutable.BitSet.newBuilder().mapResult(bs => fromBitMaskNoCopy(bs.elems))

  private def createSmall(a: Long, b: Long): BitSet = if (b == 0L) new BitSet1(a) else new BitSet2(a, b)

  /** A bitset containing all the bits in an array */
  def fromBitMask(elems: Array[Long]): BitSet = {
    val len = elems.length
    if (len == 0) empty
    else if (len == 1) new BitSet1(elems(0))
    else if (len == 2) createSmall(elems(0), elems(1))
    else {
      val a = java.util.Arrays.copyOf(elems, len)
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
    protected[collection] def nwords = 1
    protected[collection] def word(idx: Int) = if (idx == 0) elems else 0L
    protected[collection] def updateWord(idx: Int, w: Long): BitSet =
      if (idx == 0) new BitSet1(w)
      else if (idx == 1) createSmall(elems, w)
      else fromBitMaskNoCopy(updateArray(Array(elems), idx, w))
  }

  class BitSet2(val elems0: Long, elems1: Long) extends BitSet {
    protected[collection] def nwords = 2
    protected[collection] def word(idx: Int) = if (idx == 0) elems0 else if (idx == 1) elems1 else 0L
    protected[collection] def updateWord(idx: Int, w: Long): BitSet =
      if (idx == 0) new BitSet2(w, elems1)
      else if (idx == 1) createSmall(elems0, w)
      else fromBitMaskNoCopy(updateArray(Array(elems0, elems1), idx, w))
  }

  class BitSetN(val elems: Array[Long]) extends BitSet {
    protected[collection] def nwords = elems.length
    protected[collection] def word(idx: Int) = if (idx < nwords) elems(idx) else 0L
    protected[collection] def updateWord(idx: Int, w: Long): BitSet = fromBitMaskNoCopy(updateArray(elems, idx, w))
  }

}
