/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection

import BitSetLike._
import mutable.StringBuilder

/** A template trait for bitsets.
 *  $bitsetinfo
 *
 * This trait provides most of the operations of a `BitSet` independently of its representation.
 * It is inherited by all concrete implementations of bitsets.
 *
 *  @tparam  This the type of the bitset itself.
 *
 *  @define bitsetinfo
 *  Bitsets are sets of non-negative integers which are represented as
 *  variable-size arrays of bits packed into 64-bit words. The memory footprint of a bitset is
 *  determined by the largest number stored in it.
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since 2.8
 *  @define coll bitset
 *  @define Coll `BitSet`
 */
trait BitSetLike[+This <: BitSetLike[This] with SortedSet[Int]] extends SortedSetLike[Int, This] { self =>

  def empty: This

  /** The number of words (each with 64 bits) making up the set */
  protected def nwords: Int

  /** The words at index `idx`, or 0L if outside the range of the set
   *  '''Note:''' requires `idx >= 0`
   */
  protected def word(idx: Int): Long

  /** Creates a new set of this kind from an array of longs
   */
  protected def fromBitMaskNoCopy(elems: Array[Long]): This

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

  implicit def ordering: Ordering[Int] = Ordering.Int

  def rangeImpl(from: Option[Int], until: Option[Int]): This = {
    val a = toBitMask
    val len = a.length
    if(from.isDefined) {
      var f = from.get
      var pos = 0
      while(f >= 64 && pos < len) {
        f -= 64
        a(pos) = 0
        pos += 1
      }
      if(f > 0 && pos < len) a(pos) &= ~((1L << f)-1)
    }
    if(until.isDefined) {
      val u = until.get
      val w = u / 64
      val b = u % 64
      var clearw = w+1
      while(clearw < len) {
        a(clearw) = 0
        clearw += 1
      }
      if(w < len) a(w) &= (1L << b)-1
    }
    fromBitMaskNoCopy(a)
  }

  def iterator: Iterator[Int] = iteratorFrom(0)

  override def keysIteratorFrom(start: Int) = new AbstractIterator[Int] {
    private var current = start
    private val end = nwords * WordLength
    def hasNext: Boolean = {
      while (current != end && !self.contains(current)) current += 1
      current != end
    }
    def next(): Int =
      if (hasNext) { val r = current; current += 1; r }
      else Iterator.empty.next()
  }

  override def foreach[U](f: Int => U) {
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

  /** Computes the union between this bitset and another bitset by performing
   *  a bitwise "or".
   *
   *  @param   other  the bitset to form the union with.
   *  @return  a new bitset consisting of all bits that are in this
   *           bitset or in the given bitset `other`.
   */
  def | (other: BitSet): This = {
    val len = this.nwords max other.nwords
    val words = new Array[Long](len)
    for (idx <- 0 until len)
      words(idx) = this.word(idx) | other.word(idx)
    fromBitMaskNoCopy(words)
  }

  /** Computes the intersection between this bitset and another bitset by performing
   *  a bitwise "and".
   *  @param   other  the bitset to intersect with.
   *  @return  a new bitset consisting of all elements that are both in this
   *  bitset and in the given bitset `other`.
   */
  def & (other: BitSet): This = {
    val len = this.nwords min other.nwords
    val words = new Array[Long](len)
    for (idx <- 0 until len)
      words(idx) = this.word(idx) & other.word(idx)
    fromBitMaskNoCopy(words)
  }

  /** Computes the difference of this bitset and another bitset by performing
   *  a bitwise "and-not".
   *
   *  @param other the set of bits to exclude.
   *  @return     a bitset containing those bits of this
   *              bitset that are not also contained in the given bitset `other`.
   */
  def &~ (other: BitSet): This = {
    val len = this.nwords
    val words = new Array[Long](len)
    for (idx <- 0 until len)
      words(idx) = this.word(idx) & ~other.word(idx)
    fromBitMaskNoCopy(words)
  }

  /** Computes the symmetric difference of this bitset and another bitset by performing
   *  a bitwise "exclusive-or".
   *
   *  @param other the other bitset to take part in the symmetric difference.
   *  @return     a bitset containing those bits of this
   *              bitset or the other bitset that are not contained in both bitsets.
   */
  def ^ (other: BitSet): This = {
    val len = this.nwords max other.nwords
    val words = new Array[Long](len)
    for (idx <- 0 until len)
      words(idx) = this.word(idx) ^ other.word(idx)
    fromBitMaskNoCopy(words)
  }

  def contains(elem: Int): Boolean =
    0 <= elem && (word(elem >> LogWL) & (1L << elem)) != 0L

  /** Tests whether this bitset is a subset of another bitset.
   *
   *  @param other  the bitset to test.
   *  @return     `true` if this bitset is a subset of `other`, i.e. if
   *              every bit of this set is also an element in `other`.
   */
  def subsetOf(other: BitSet): Boolean =
    (0 until nwords) forall (idx => (this.word(idx) & ~ other.word(idx)) == 0L)

  override def head: Int = {
    val n = nwords
    var i = 0
    while (i < n) {
      val wi = word(i)
      if (wi != 0L) return WordLength*i + java.lang.Long.numberOfTrailingZeros(wi)
      i += 1
    }
    throw new NoSuchElementException("Empty BitSet")
  }

  override def last: Int = {
    var i = nwords - 1
    while (i >= 0) {
      val wi = word(i)
      if (wi != 0L) return WordLength*i + 63 - java.lang.Long.numberOfLeadingZeros(wi)
      i += 1
    }
    throw new NoSuchElementException("Empty BitSet")
  }

  override def addString(sb: StringBuilder, start: String, sep: String, end: String) = {
    sb append start
    var pre = ""
    val max = nwords * WordLength
    var i = 0
    while(i != max) {
      if (contains(i)) {
        sb append pre append i
        pre = sep
      }
      i += 1
    }
    sb append end
  }

  override def stringPrefix = "BitSet"
}

/** Companion object for BitSets. Contains private data only */
object BitSetLike {
  /* Final vals can sometimes be inlined as constants (faster) */
  private[collection] final val LogWL = 6
  private final val WordLength = 64
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
