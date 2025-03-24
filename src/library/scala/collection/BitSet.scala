/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.annotation.nowarn
import scala.collection.Stepper.EfficientSplit
import scala.collection.mutable.Builder


/** Base type of bitsets.
  *
  * This trait provides most of the operations of a `BitSet` independently of its representation.
  * It is inherited by all concrete implementations of bitsets.
  *
  * @define bitsetinfo
  *  Bitsets are sets of non-negative integers which are represented as
  *  variable-size arrays of bits packed into 64-bit words. The lower bound of memory footprint of a bitset is
  *  determined by the largest number stored in it.
  * @define coll bitset
  * @define Coll `BitSet`
  */
trait BitSet extends SortedSet[Int] with BitSetOps[BitSet] {
  override protected def fromSpecific(coll: IterableOnce[Int]): BitSet = bitSetFactory.fromSpecific(coll)
  override protected def newSpecificBuilder: Builder[Int, BitSet] = bitSetFactory.newBuilder
  override def empty: BitSet = bitSetFactory.empty
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
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

  def bitSetFactory: SpecificIterableFactory[Int, C]

  def unsorted: Set[Int]

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
    private[this] var currentPos = if (start > 0) start >> LogWL else 0
    private[this] var currentWord = if (start > 0) word(currentPos) & (-1L << (start & (WordLength - 1))) else word(0)
    final override def hasNext: Boolean = {
      while (currentWord == 0) {
        if (currentPos + 1 >= nwords) return false
        currentPos += 1
        currentWord = word(currentPos)
      }
      true
    }
    final override def next(): Int = {
      if (hasNext) {
        val bitPos = java.lang.Long.numberOfTrailingZeros(currentWord)
        currentWord &= currentWord - 1
        (currentPos << LogWL) + bitPos
      } else Iterator.empty.next()
    }
  }

  override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Int, S]): S with EfficientSplit = {
    val st = scala.collection.convert.impl.BitSetStepper.from(this)
    val r =
      if (shape.shape == StepperShape.IntShape) st
      else {
        assert(shape.shape == StepperShape.ReferenceShape, s"unexpected StepperShape: $shape")
        AnyStepper.ofParIntStepper(st)
      }
    r.asInstanceOf[S with EfficientSplit]
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

  @inline private[this] def smallestInt: Int = {
    val thisnwords = nwords
    var i = 0
    while(i < thisnwords) {
      val currentWord = word(i)
      if (currentWord != 0L) {
        return java.lang.Long.numberOfTrailingZeros(currentWord) + (i * WordLength)
      }
      i += 1
    }
    throw new UnsupportedOperationException("empty.smallestInt")
  }

  @inline private[this] def largestInt: Int = {
    var i = nwords - 1
    while(i >= 0) {
      val currentWord = word(i)
      if (currentWord != 0L) {
        return ((i + 1) * WordLength) - java.lang.Long.numberOfLeadingZeros(currentWord) - 1
      }
      i -= 1
    }
    throw new UnsupportedOperationException("empty.largestInt")
  }

  override def max[B >: Int](implicit ord: Ordering[B]): Int =
    if (Ordering.Int eq ord) largestInt
    else if (Ordering.Int isReverseOf ord) smallestInt
    else super.max(ord)


  override def min[B >: Int](implicit ord: Ordering[B]): Int =
    if (Ordering.Int eq ord) smallestInt
    else if (Ordering.Int isReverseOf ord) largestInt
    else super.min(ord)

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
      val f = from.get
      val w = f >> LogWL
      val b = f & (WordLength - 1)
      if (w >= 0) {
        java.util.Arrays.fill(a, 0, math.min(w, len), 0)
        if (b > 0 && w < len) a(w) &= ~((1L << b) - 1)
      }
    }
    if (until.isDefined) {
      val u = until.get
      val w = u >> LogWL
      val b = u & (WordLength - 1)
      if (w < len) {
        java.util.Arrays.fill(a, math.max(w + 1, 0), len, 0)
        if (w >= 0) a(w) &= (1L << b) - 1
      }
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
  def map(f: Int => Int): C = fromSpecific(new View.Map(this, f))

  def flatMap(f: Int => IterableOnce[Int]): C = fromSpecific(new View.FlatMap(this, f))

  def collect(pf: PartialFunction[Int, Int]): C = fromSpecific(super[SortedSetOps].collect(pf))

  override def partition(p: Int => Boolean): (C, C) = {
    val left = filter(p)
    (left, this &~ left)
  }
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

  private[collection] def computeWordForFilter(pred: Int => Boolean, isFlipped: Boolean, oldWord: Long, wordIndex: Int): Long =
    if (oldWord == 0L) 0L else {
      var w = oldWord
      val trailingZeroes = java.lang.Long.numberOfTrailingZeros(w)
      var jmask = 1L << trailingZeroes
      var j = wordIndex * BitSetOps.WordLength + trailingZeroes
      val maxJ = (wordIndex + 1) * BitSetOps.WordLength - java.lang.Long.numberOfLeadingZeros(w)
      while (j != maxJ) {
        if ((w & jmask) != 0L) {
          if (pred(j) == isFlipped) {
            // j did not pass the filter here
            w = w & ~jmask
          }
        }
        jmask = jmask << 1
        j += 1
      }
      w
    }
}
