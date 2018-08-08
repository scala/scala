package scala
package collection
package mutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.collection.immutable.Range
import BitSetOps.{LogWL, MaxSize}
import scala.annotation.implicitNotFound


/**
  * A class for mutable bitsets.
  *
  * $bitsetinfo
  *
  * @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#mutable-bitsets "Scala's Collection Library overview"]]
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
    with collection.BitSet
    with SortedSetOps[Int, SortedSet, BitSet]
    with collection.BitSetOps[BitSet]
    with StrictOptimizedIterableOps[Int, Set, BitSet]
    with StrictOptimizedSortedSetOps[Int, SortedSet, BitSet] {

  def this(initSize: Int) = this(new Array[Long](math.max((initSize + 63) >> 6, 1)))

  def this() = this(0)

  def bitSetFactory = BitSet

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
  def |= (other: BitSet): this.type = {
    ensureCapacity(other.nwords - 1)
    for (i <- Range(0, other.nwords))
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
    for (i <- Range(0, nwords))
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
    for (i <- Range(0, other.nwords))
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
    for (i <- Range(0, other.nwords))
      elems(i) = elems(i) & ~other.word(i)
    this
  }

  override def clone(): BitSet =
    new BitSet(java.util.Arrays.copyOf(elems, elems.length))

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

  override protected[this] def writeReplace(): AnyRef = new BitSet.SerializationProxy(this)
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

  // scalac generates a `readReplace` method to discard the deserialized state (see https://github.com/scala/bug/issues/10412).
  // This prevents it from serializing it in the first place:
  private[this] def writeObject(out: ObjectOutputStream): Unit = ()
  private[this] def readObject(in: ObjectInputStream): Unit = ()
}
