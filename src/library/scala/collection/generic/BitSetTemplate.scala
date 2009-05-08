package scala.collection.generic

import BitSetTemplate._
import generic._

/** common base class for mutable and immutable bit sets
 */
trait BitSetTemplate[+This <: BitSetTemplate[This] with Set[Int]] extends SetTemplate[Int, This] { self =>

  /** The number of words (each with 64 bits) making up the set */
  protected def nwords: Int

  /** The words at index `idx', or 0L if outside the range of the set
   *  @pre idx >= 0
   */
  protected def word(idx: Int): Long

  /** Update word at index `idx`; enlarge set if `idx` outside range of set
   */
  protected def updateWord(idx: Int, w: Long): This

  /** Create a new set of this kind from an array of longs
   */
  protected def fromArray(elems: Array[Long]): This

  /** Adds element to bitset, reusing set if its mutable.
   */
  override def + (elem: Int): This = {
    require(elem >= 0)
    if (contains(elem)) thisCollection
    else {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) | (1L << elem))
    }
  }

  /** Removes element to bitset, reusing set if its mutable.
   */
  override def - (elem: Int): This = {
    require(elem >= 0)
    if (contains(elem)) {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) & ~(1L << elem))
    } else thisCollection
  }

  /** The number of elements in the bitset.
   */
  override def size: Int = {
    var s = 0
    var i = nwords
    while (i > 0) {
      i -= 1
      s += popCount(word(i))
    }
    s
  }

  def elements = new Iterator[Int] {
    private var current = 0
    private val end = nwords * WordLength
    def hasNext: Boolean = {
      while (current < end && !self.contains(current)) current += 1
      current < end
    }
    def next(): Int =
      if (hasNext) { val r = current; current += 1; r }
      else Iterator.empty.next
  }

  override def foreach(f: Int => Unit) {
    for (i <- 0 until nwords) {
      val w = word(i)
      for (j <- i * WordLength until (i + 1) * WordLength) {
        if ((w & (1L << j)) != 0) f(j)
      }
    }
  }

  /** A new bitset which is the logical or of this set and the given argument set.
   */
  def | (other: BitSet): This = {
    val len = this.nwords max other.nwords
    val words = new Array[Long](len)
    for (idx <- 0 until len)
      words(idx) = this.word(idx) | other.word(idx)
    fromArray(words)
  }

  /** A new bitset which is the logical and of this set and the given argument set.
   */
  def & (other: BitSet): This = {
    val len = this.nwords min other.nwords
    val words = new Array[Long](len)
    for (idx <- 0 until len)
      words(idx) = this.word(idx) & other.word(idx)
    fromArray(words)
  }

  /** A new bitset which is the logical and-not of this set and the given argument set.
   */
  def &~ (other: BitSet): This = {
    val len = this.nwords
    val words = new Array[Long](len)
    for (idx <- 0 until len)
      words(idx) = this.word(idx) & ~other.word(idx)
    fromArray(words)
  }

  /** A new bitset which is the logical exclusive or of this set and the given argument set.
   */
  def ^ (other: BitSet): This = {
    val len = this.nwords max other.nwords
    val words = new Array[Long](len)
    for (idx <- 0 until len)
      words(idx) = this.word(idx) ^ other.word(idx)
    fromArray(words)
  }

  /** Does the set contain the given element?
   */
  def contains(elem: Int): Boolean =
    0 <= elem && (word(elem >> LogWL) & (1L << elem)) != 0

  /** Is the set a subset of the given bitset
   */
  def subSet(other: BitSet): Boolean =
    (0 until nwords) forall (idx => (this.word(idx) & ~ other.word(idx)) == 0L)

  /** Add bitset elements as numbers to string buffer
   */
  override def addString(sb: StringBuilder, start: String, sep: String, end: String) = {
    sb append start
    var pre = ""
    for (i <- 0 until nwords * WordLength)
      if (contains(i)) {
        sb append pre append i
        pre = sep
      }
    sb append end
  }
}

object BitSetTemplate {
  private val LogWL = 6
  private val WordLength = 64

  private val pc1: Array[Int] = {
    def countBits(x: Int): Int = if (x == 0) 0 else x % 2 + countBits(x >>> 1)
    Array.fromFunction(countBits _)(256)
  }

  private def popCount(w: Long): Int = {
    def pc2(w: Int) = if (w == 0) 0 else pc1(w & 0xff) + pc1(w >>> 8)
    def pc4(w: Int) = if (w == 0) 0 else pc2(w & 0xffff) + pc2(w >>> 16)
    if (w == 0) 0 else pc4(w.toInt) + pc4((w >>> 32).toInt)
  }
}
