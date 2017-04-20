package strawman
package collection.immutable

import strawman.collection.BoundedIterableFactory
import strawman.collection.BitSetLike.{LogWL, updateArray}
import strawman.collection.mutable.Builder

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
  extends collection.BitSet
    with collection.BitSetLike[BitSet]
    with SortedSet[Int]
    with SetMonoTransforms[Int, BitSet] // Override mono transforms ops to return a BitSet rather than a SortedSet[Int]
    with Serializable {

  def empty: BitSet = BitSet.empty

  // From IterableMonoTransforms
  protected def fromIterableWithSameElemType(coll: collection.Iterable[Int]): BitSet = BitSet.fromIterable(coll)
  // From ConstrainedIterablePolyTransforms
  protected def constrainedFromIterable[B : Ordering](it: collection.Iterable[B]): SortedSet[B] = SortedSet.constrainedFromIterable(it)
  // From IterablePolyTransforms
  def fromIterable[B](coll: collection.Iterable[B]): Set[B] = Set.fromIterable(coll)

  protected[collection] def fromBitMaskNoCopy(elems: Array[Long]): BitSet = BitSet.fromBitMaskNoCopy(elems)

  def add(elem: Int): BitSet = {
    require(elem >= 0, "bitset element must be >= 0")
    if (contains(elem)) this
    else {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) | (1L << elem))
    }
  }

  def remove(elem: Int): BitSet = {
    require(elem >= 0, "bitset element must be >= 0")
    if (contains(elem)) {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) & ~(1L << elem))
    } else this
  }

  def unconstrained: Set[Int] = this

  /** Update word at index `idx`; enlarge set if `idx` outside range of set.
    */
  protected def updateWord(idx: Int, w: Long): BitSet
}

object BitSet extends BoundedIterableFactory[Int] {
  type To[_] = BitSet

  def fromIterable[E <: Int](it: strawman.collection.Iterable[E]): BitSet =
    it match {
      case bs: BitSet => bs
      case _          => newBuilder[E].++=(it).result
    }

  def newBuilder[E <: Int]: Builder[E, BitSet] = new Builder[Int, BitSet] {
    private[this] val b = collection.mutable.BitSet.empty
    def addInPlace(x: Int): this.type = { b += x; this }
    def clear(): Unit = b.clear()
    def result: BitSet = fromBitMaskNoCopy(b.elems)
  }

  private def createSmall(a: Long, b: Long): BitSet = if (b == 0L) new BitSet1(a) else new BitSet2(a, b)

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

  def empty[A <: Int]: BitSet = new BitSet1(0L)

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
