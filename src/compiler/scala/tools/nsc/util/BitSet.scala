package scala.tools.nsc
package util

import BitSet._

abstract class BitSet {

  protected def nwords: Int
  protected def word(idx: Int): Long
  protected def updateWord(idx: Int, w: Long): BitSet

  def + (elem: Int): BitSet = {
    require(elem >= 0)
    if (contains(elem)) this
    else {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) | (1L << elem))
    }
  }

  def - (elem: Int): BitSet = {
    require(elem >= 0)
    if (contains(elem)) {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) & ~(1L << elem))
    } else this
  }

  def | (other: BitSet): BitSet = {
    val len = this.nwords max other.nwords
    val words = new Array[Long](len)
    for (idx <- 0 until len)
      words(idx) = this.word(idx) | other.word(idx)
    fromArray(words)
  }

  def & (other: BitSet): BitSet = {
    val len = this.nwords min other.nwords
    val words = new Array[Long](len)
    for (idx <- 0 until len)
      words(idx) = this.word(idx) & other.word(idx)
    fromArray(words)
  }

  def &~ (other: BitSet): BitSet = {
    val len = this.nwords
    val words = new Array[Long](len)
    for (idx <- 0 until len)
      words(idx) = this.word(idx) & ~other.word(idx)
    fromArray(words)
  }

  def ^ (other: BitSet): BitSet = {
    val len = this.nwords max other.nwords
    val words = new Array[Long](len)
    for (idx <- 0 until len)
      words(idx) = this.word(idx) ^ other.word(idx)
    fromArray(words)
  }

  def contains(elem: Int): Boolean =
    0 <= elem && (word(elem >> LogWL) & (1L << elem)) != 0L

  def subSet(other: BitSet): Boolean =
    (0 until nwords) forall (idx => (this.word(idx) & ~ other.word(idx)) == 0L)

  override def equals(other: Any) = other match {
    case that: BitSet =>
      (0 until (this.nwords max that.nwords)) forall (idx => this.word(idx) == that.word(idx))
    case _ =>
      false
  }

  override def hashCode: Int = {
    var h = hashSeed
    for (idx <- 0 until nwords) {
      val w = word(idx)
      h = (h * 41 + (w >>> 32).toInt) * 41 + w.toInt
    }
    h
  }

  def addString(sb: StringBuilder, start: String, sep: String, end: String) {
    sb append start
    var pre = ""
    for (i <- 0 until nwords * WordLength)
      if (contains(i)) {
        sb append pre append i
        pre = sep
      }
    sb append end
  }

  def mkString(start: String, sep: String, end: String) = {
    val sb = new StringBuilder
    addString(sb, start, sep, end)
    sb.toString
  }

  override def toString = mkString("BitSet(", ", ", ")")
}

object BitSet {

  private final val WordLength = 64
  private final val LogWL = 6
  private val hashSeed = "BitSet".hashCode

  val empty: BitSet = new BitSet1(0L)

  def apply(elems: Int*) = (empty /: elems) (_ + _)

  def fromArray(elems: Array[Long]) = {
    val len = elems.length
    if (len == 0) empty
    else if (len == 1) new BitSet1(elems(0))
    else if (len == 2) new BitSet2(elems(0), elems(1))
    else new BitSetN(elems)
  }

  private def updateArray(elems: Array[Long], idx: Int, w: Long): BitSet = {
    var len = elems.length
    while (len > 0 && (elems(len - 1) == 0L || w == 0L && idx == len - 1)) len -= 1
    var newlen = len
    if (idx >= newlen && w != 0L) newlen = idx + 1
    val newelems = new Array[Long](newlen)
    Array.copy(elems, 0, newelems, 0, len)
    if (idx < newlen) newelems(idx) = w
    else assert(w == 0L)
    fromArray(newelems)
  }

  class BitSet1(val elems: Long) extends BitSet {
    protected def nwords = 1
    protected def word(idx: Int) = if (idx == 0) elems else 0L
    protected def updateWord(idx: Int, w: Long): BitSet =
      if (idx == 0) new BitSet1(w)
      else if (idx == 1) new BitSet2(elems, w)
      else updateArray(Array(elems), idx, w)
  }

  class BitSet2(val elems0: Long, elems1: Long) extends BitSet {
    protected def nwords = 2
    protected def word(idx: Int) = if (idx == 0) elems0 else if (idx == 1) elems1 else 0L
    protected def updateWord(idx: Int, w: Long): BitSet =
      if (idx == 0) new BitSet2(w, elems1)
      else if (idx == 1) new BitSet2(elems0, w)
      else updateArray(Array(elems0, elems1), idx, w)
  }

  class BitSetN(val elems: Array[Long]) extends BitSet {
    protected def nwords = elems.length
    protected def word(idx: Int) = if (idx < nwords) elems(idx) else 0L
    protected def updateWord(idx: Int, w: Long): BitSet = updateArray(elems, idx, w)
  }
}
