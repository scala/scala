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
 *  @author  Burak Emir
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

  /** returns the subset of <code>[0..size]</code> whose elements are
   *  indices of bits set to <code>v</code>.
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

}
