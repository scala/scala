package scala.collection.mutable

import generic._

/** A class for mutable bitsets */
class BitSet(initSize: Int) extends Set[Int] with collection.BitSet with BitSetTemplate[BitSet] {

  override def empty = BitSet.empty

  def this() = this(0)

  protected var elems: Array[Long] = new Array[Long]((initSize + 63) >> 6 max 1)

  protected def nwords = elems.length
  protected def word(idx: Int): Long =
    if (idx < nwords) elems(idx) else 0L
  protected def updateWord(idx: Int, w: Long): BitSet = {
    if (idx >= nwords) {
      var newlen = nwords
      while (idx >= newlen) newlen = newlen * 2
      val elems1 = new Array[Long](newlen)
      Array.copy(elems, 0, elems1, 0, nwords)
      elems = elems1
    }
    elems(idx) = w
    this
  }

  protected def fromArray(words: Array[Long]): BitSet = {
    val s = new BitSet; s.elems = words; s
  }

  override def += (elem: Int) { super.+(elem) }

  override def -= (elem: Int) { super.-(elem) }

  override def + (elem: Int): this.type = { +=(elem); this }

  override def - (elem: Int): this.type = { -=(elem); this }

  def toImmutable = immutable.BitSet.fromArray(elems)
}

/** A factory object for mutable bitsets */
object BitSet {
  def empty: BitSet = new BitSet
  def apply(bits: Int*): BitSet = {
    var s = empty; for (b <- bits) s += b; s
  }
}
