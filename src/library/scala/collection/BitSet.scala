/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection


/** <p>
 *    The class <code>BitSet</code> provides the interface for a space-efficient
 *    implementation of dense integer sets represented as bits in array of
 *    integers. Bit indices are between <code>0..(capacity-1)</code> inclusive.
 *  </p>
 *
 *  @author  Burak Emir, Stephane Micheloud, Nikolay Mihaylov
 *  @author  Martin Odersky
 *  @version 2.0  01/01/2007
 */

abstract class BitSet extends Set[Int] {

  import compat.Platform.arraycopy
  import compat.Math.min

  /** number of bits in this bitset */
  def size: Int

  /** @return true if bit i is set */
  def contains(i: Int): Boolean =
    (i < capacity) && ((arr(offset(i)) & mask(i)) != 0)

  def capacity: Int

  protected def arr: Array[Int]

  /** returns an iterator over the truth values of all bits */
   final def elements: Iterator[Int] = new Iterator[Int] {
     var i = 0
     def findNext: Unit = {
       while (!BitSet.this.contains(i) && (i < capacity))
         i = i + 1
     }
     findNext
     def hasNext: Boolean = i < capacity
     def next: Int = { val j = i; i = i + 1; findNext; j }
   }


  /**
   * @return a copy of the array underlying this bitset
   */
  def toArray: Array[Int] = {
    val length = memsize(capacity)
    val newarr = new Array[Int](length)
    arraycopy(newarr, 0, this.arr, 0, length)
    newarr
  }

  /**
   * Checks if two bitsets are structurally identical.
   * Uses accelerated (32 x faster) check if the other set is a BitSet
   *
   * @return true, iff both bitsets contain the same elements.
   */
  override def equals(other: Any): Boolean = other match {
    case that: BitSet =>
      (size == that.size) && {
        var len = memsize(min(this.capacity, that.capacity))
        var i = 0
        while (i < len && arr(i) == that.arr(i)) i = i + 1
        i == len
      }
    case _ =>
      super.equals(other)
  }

  override def hashCode(): Int = {
    val len = memsize(this.capacity)
    var h = 0
    var i = 0
    while (i < len) { h = h * 41 + arr(i); i = i + 1 }
    h
  }

  /**
   * Checks if this set is a subset of set <code>that</code>.
   * Uses accelerated (32 x faster) check if the other set is a BitSet
   *
   * @param  that another set.
   * @return true, iff the other set is a superset of this set.
   */
  override def subsetOf(other: Set[Int]): Boolean = other match {
    case that: BitSet =>
      val thisLen = memsize(this.capacity)
      val thatLen = memsize(that.capacity)
      val minLen = min(thisLen, thatLen)
      var i = 0
      while (i < minLen && that.arr(i) == (that.arr(i) | arr(i))) i = i + 1
      while (i < thisLen && arr(i) == 0) i = i + 1
      i == thisLen
    case _ =>
      super.subsetOf(other)
  }

  /** @return the number of Int cells needed to store <code>n</code> bits */
  protected final def memsize(n: Int): Int = offset(n + 31)

  /** @return the number of bits represented by <code>n</code> words */
  protected final def nbits(n: Int): Int = n << 5

  /** @return the position in the array where the bit resides */
  protected final def offset(n: Int): Int = n >>> 5

  /** @return a mask with 1 at the position of the bit */
  protected final def mask(n: Int): Int = 1 << (n & 0x1F)

}
