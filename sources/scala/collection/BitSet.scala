/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection;

/** The class <code>BitSet</code> ...
 *
 *  @author  Burak Emir, Stephane Micheloud
 *  @version 1.0
 */
abstract class BitSet with Function1[Int,Boolean] {

  /** number of bits in this bitset */
  def size: Int;

  /** returns true if bit i is set */
  def apply(i: Int): Boolean;

  /** returns an iterator over the truth values of all bits */
  final def booleanElements: Iterator[Boolean] = new Iterator[Boolean] {
    var i = 0;
    def hasNext: Boolean = i < size;
    def next: Boolean = { i = i + 1; apply(i-1) }
  }

  /**
   * Returns the subset of <code>[0..size]</code> whose elements are
   * indices of bits set to <code>v</code>.
   *
   *  @param v
   */
  final def toSet(v: Boolean) = {
    var res = new immutable.TreeSet[Int]();
    var j = 0;
    while (j < size) {
      if (v == apply(j))
        res = res + j;
      j = j + 1;
    }
    res
  }

  /**
   * Checks if two bitsets are structurally identical.
   *
   *  @return true, iff both bitsets contain the same sequence of elements.
   */
  override def equals(that: Any): Boolean =
    that.isInstanceOf[BitSet] &&
    { val other = that.asInstanceOf[BitSet];
      (size == other.size) &&
      (Iterator.range(0, size) forall { i => apply(i) == other.apply(i)});
    };

  /**
   * Returns a string representation of this bitset in hexadecimal form,
   * e.g. the bitset 001000001100 (12 bits) is represented as "20c" and
   * the bitset 00100000110 (11 bits) as "106".
   *
   * @return the string representation for this bitset
   */
  override def toString() = {
    val sb = new StringBuffer();
    val n = size % 4;
    if (n > 0) {
      val x = (if (apply(0))          4 else 0)
            + (if (n > 1 && apply(1)) 2 else 0)
            + (if (n > 2 && apply(2)) 1 else 0);
      sb.append(Integer.toHexString(x));
    }
    var i = n;
    while (i < size) {
      val x = (if (apply(i))   8 else 0)
            + (if (apply(i+1)) 4 else 0)
            + (if (apply(i+2)) 2 else 0)
            + (if (apply(i+3)) 1 else 0);
      sb.append(Integer.toHexString(x));
      i = i + 4
    }
    sb.toString()
  }

}
