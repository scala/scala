package strawman
package collection
package mutable

import BitSetOps.{LogWL, MaxSize}

import scala.{Array, Int, Long, math, None, Option, Ordering, SerialVersionUID, Serializable, Some, Unit}
import scala.Predef.{require}

/**
  * A class for mutable bitsets.
  *
  * $bitsetinfo
  *
  * @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#mutable_bitsets "Scala's Collection Library overview"]]
  * section on `Mutable Bitsets` for more information.
  *
  * @define Coll `BitSet`
  * @define coll bitset
  * @define orderDependent
  * @define orderDependentFold
  * @define mayNotTerminateInf
  * @define willNotTerminateInf
  */
@SerialVersionUID(8483111450368547763L)
class BitSet(protected[collection] final var elems: Array[Long])
  extends SortedSet[Int]
    with collection.BitSet
    with SortedSetOps[Int, SortedSet, BitSet]
    with collection.BitSetOps[BitSet]
    with Serializable {

  def this(initSize: Int) = this(new Array[Long](math.max((initSize + 63) >> 6, 1)))

  def this() = this(0)

  def fromIterable[B](coll: collection.Iterable[B]): collection.mutable.Set[B] =
    collection.mutable.Set.fromIterable(coll)

  protected[this] def orderedFromIterable[B : Ordering](it: collection.Iterable[B]): collection.mutable.SortedSet[B] =
    collection.mutable.SortedSet.orderedFromIterable(it)

  protected[this] def fromSpecificIterable(coll: collection.Iterable[Int]): BitSet =
    BitSet.fromSpecificIterable(coll)

  protected[collection] final def nwords: Int = elems.length

  protected[collection] final def word(idx: Int): Long =
    if (idx < nwords) elems(idx) else 0L

  protected[collection] def fromBitMaskNoCopy(elems: Array[Long]): BitSet = new BitSet(elems)

  def add(elem: Int): this.type = {
    require(elem >= 0)
    if (!contains(elem)) {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) | (1L << elem))
    }
    this
  }

  def subtract(elem: Int): this.type = {
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

  def get(elem: Int): Option[Int] = if (contains(elem)) Some(elem) else None

  def unconstrained: collection.Set[Int] = this

  def empty: BitSet = BitSet.empty

  override def clone(): BitSet =
    new BitSet(java.util.Arrays.copyOf(elems, elems.length))

}

object BitSet extends SpecificIterableFactory[Int, BitSet] {

  def fromSpecificIterable(it: strawman.collection.Iterable[Int]): BitSet = Growable.fromIterable(empty, it)

  def empty: BitSet = new BitSet()

}
