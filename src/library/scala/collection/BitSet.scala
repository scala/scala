package scala
package collection

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.Builder


/** Base type of bitsets.
  *
  * This trait provides most of the operations of a `BitSet` independently of its representation.
  * It is inherited by all concrete implementations of bitsets.
  *
  * @define bitsetinfo
  *  Bitsets are sets of non-negative integers which are represented as
  *  variable-size arrays of bits packed into 64-bit words. The memory footprint of a bitset is
  *  determined by the largest number stored in it.
  * @define coll bitset
  * @define Coll `BitSet`
  */
trait BitSet extends SortedSet[Int] with BitSetOps[BitSet] {
  override protected def fromSpecific(coll: IterableOnce[Int]): BitSetC = bitSetFactory.fromSpecific(coll)
  override protected def newSpecificBuilder: Builder[Int, BitSetC] = bitSetFactory.newBuilder
  override def empty: BitSetC = bitSetFactory.empty
  override protected[this] def stringPrefix = "BitSet"
  override def unsorted: Set[Int] = this
}

@SerialVersionUID(3L)
object BitSet extends SpecificIterableFactory[Int, BitSet] {
  private[collection] final val ordMsg = "No implicit Ordering[${B}] found to build a SortedSet[${B}]. You may want to upcast to a Set[Int] first by calling `unsorted`."
  private[collection] final val zipOrdMsg = "No implicit Ordering[${B}] found to build a SortedSet[(Int, ${B})]. You may want to upcast to a Set[Int] first by calling `unsorted`."

  def empty: BitSet = immutable.BitSet.empty
  def newBuilder: Builder[Int, BitSet] = immutable.BitSet.newBuilder
  def fromSpecific(it: IterableOnce[Int]): BitSet = immutable.BitSet.fromSpecific(it)

  @SerialVersionUID(3L)
  private[collection] abstract class SerializationProxy(@transient protected val coll: BitSet) extends Serializable {

    @transient protected var elems: Array[Long] = _

    private[this] def writeObject(out: ObjectOutputStream): Unit = {
      out.defaultWriteObject()
      val nwords = coll.nwords
      out.writeInt(nwords)
      var i = 0
      while(i < nwords) {
        out.writeLong(coll.word(i))
        i += 1
      }
    }

    private[this] def readObject(in: ObjectInputStream): Unit = {
      in.defaultReadObject()
      val nwords = in.readInt()
      elems = new Array[Long](nwords)
      var i = 0
      while(i < nwords) {
        elems(i) = in.readLong()
        i += 1
      }
    }

    protected[this] def readResolve(): Any
  }
}

/** Base implementation type of bitsets */
trait BitSetOps[+C <: BitSet with BitSetOps[C]]
  extends SortedSetOps[Int, SortedSet, C] { self =>
  import BitSetOps._

  def bitSetFactory: SpecificIterableFactory[Int, BitSetC]

  def unsorted: Set[Int]

  /**
    * Type alias to `C`. It is used to provide a default implementation of the `fromSpecific`
    * and `newSpecificBuilder` operations.
    *
    * Due to the `@uncheckedVariance` annotation, usage of this type member can be unsound and is
    * therefore not recommended.
    */
  protected type BitSetC = C @uncheckedVariance

  final def ordering: Ordering[Int] = Ordering.Int

  /** The number of words (each with 64 bits) making up the set */
  protected[collection] def nwords: Int

  /** The words at index `idx`, or 0L if outside the range of the set
    *  '''Note:''' requires `idx >= 0`
    */
  protected[collection] def word(idx: Int): Long

  /** Creates a new set of this kind from an array of longs
    */
  protected[collection] def fromBitMaskNoCopy(elems: Array[Long]): C

  def contains(elem: Int): Boolean =
    0 <= elem && (word(elem >> LogWL) & (1L << elem)) != 0L

  def iterator: Iterator[Int] = iteratorFrom(0)

  def iteratorFrom(start: Int): Iterator[Int] = new AbstractIterator[Int] {
    private[this] var current = start
    private[this] val end = nwords * WordLength
    def hasNext: Boolean = {
      while (current != end && !self.contains(current)) current += 1
      current != end
    }
    def next(): Int =
      if (hasNext) { val r = current; current += 1; r }
      else Iterator.empty.next()
  }

  override def size: Int = {
    var s = 0
    var i = nwords
    while (i > 0) {
      i -= 1
      s += java.lang.Long.bitCount(word(i))
    }
    s
  }

  override def isEmpty: Boolean = 0 until nwords forall (i => word(i) == 0)

  override def foreach[U](f: Int => U): Unit = {
    /* NOTE: while loops are significantly faster as of 2.11 and
       one major use case of bitsets is performance. Also, there
       is nothing to do when all bits are clear, so use that as
       the inner loop condition. */
    var i = 0
    while (i < nwords) {
      var w = word(i)
      var j = i * WordLength
      while (w != 0L) {
        if ((w&1L) == 1L) f(j)
        w = w >>> 1
        j += 1
      }
      i += 1
    }
  }

  /** Creates a bit mask for this set as a new array of longs
    */
  def toBitMask: Array[Long] = {
    val a = new Array[Long](nwords)
    var i = a.length
    while(i > 0) {
      i -= 1
      a(i) = word(i)
    }
    a
  }

  def rangeImpl(from: Option[Int], until: Option[Int]): C = {
    val a = coll.toBitMask
    val len = a.length
    if (from.isDefined) {
      var f = from.get
      var pos = 0
      while (f >= 64 && pos < len) {
        f -= 64
        a(pos) = 0
        pos += 1
      }
      if (f > 0 && pos < len) a(pos) &= ~((1L << f)-1)
    }
    if (until.isDefined) {
      val u = until.get
      val w = u / 64
      val b = u % 64
      var clearw = w+1
      while (clearw < len) {
        a(clearw) = 0
        clearw += 1
      }
      if (w < len) a(w) &= (1L << b)-1
    }
    coll.fromBitMaskNoCopy(a)
  }

  override def concat(other: collection.IterableOnce[Int]): C = other match {
    case otherBitset: BitSet =>
      val len = coll.nwords max otherBitset.nwords
      val words = new Array[Long](len)
      for (idx <- 0 until len)
        words(idx) = this.word(idx) | otherBitset.word(idx)
      fromBitMaskNoCopy(words)
    case _ => super.concat(other)
  }

  override def intersect(other: Set[Int]): C = other match {
    case otherBitset: BitSet =>
      val len = coll.nwords min otherBitset.nwords
      val words = new Array[Long](len)
      for (idx <- 0 until len)
        words(idx) = this.word(idx) & otherBitset.word(idx)
      fromBitMaskNoCopy(words)
    case _ => super.intersect(other)
  }

  abstract override def diff(other: Set[Int]): C = other match {
    case otherBitset: BitSet =>
      val len = coll.nwords
      val words = new Array[Long](len)
      for (idx <- 0 until len)
        words(idx) = this.word(idx) & ~otherBitset.word(idx)
      fromBitMaskNoCopy(words)
    case _ => super.diff(other)
  }

  /** Computes the symmetric difference of this bitset and another bitset by performing
    *  a bitwise "exclusive-or".
    *
    *  @param other the other bitset to take part in the symmetric difference.
    *  @return     a bitset containing those bits of this
    *              bitset or the other bitset that are not contained in both bitsets.
    */
  def xor(other: BitSet): C = {
    val len = coll.nwords max other.nwords
    val words = new Array[Long](len)
    for (idx <- 0 until len)
      words(idx) = coll.word(idx) ^ other.word(idx)
    coll.fromBitMaskNoCopy(words)
  }

  @`inline` final def ^ (other: BitSet): C = xor(other)

  /**
    * Builds a new bitset by applying a function to all elements of this bitset
    * @param f the function to apply to each element.
    * @return a new bitset resulting from applying the given function ''f'' to
    *         each element of this bitset and collecting the results
    */
  def map(f: Int => Int): C = fromSpecific(new View.Map(toIterable, f))

  def flatMap(f: Int => IterableOnce[Int]): C = fromSpecific(new View.FlatMap(toIterable, f))

  def collect(pf: PartialFunction[Int, Int]): C = fromSpecific(super[SortedSetOps].collect(pf).toIterable)
}

object BitSetOps {

  /* Final vals can sometimes be inlined as constants (faster) */
  private[collection] final val LogWL = 6
  private[collection] final val WordLength = 64
  private[collection] final val MaxSize = (Int.MaxValue >> LogWL) + 1

  private[collection] def updateArray(elems: Array[Long], idx: Int, w: Long): Array[Long] = {
    var len = elems.length
    while (len > 0 && (elems(len - 1) == 0L || w == 0L && idx == len - 1)) len -= 1
    var newlen = len
    if (idx >= newlen && w != 0L) newlen = idx + 1
    val newelems = new Array[Long](newlen)
    Array.copy(elems, 0, newelems, 0, len)
    if (idx < newlen) newelems(idx) = w
    else assert(w == 0L)
    newelems
  }
}
