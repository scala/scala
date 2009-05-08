/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable

import BitSet._
import generic._

/** a base class for immutable bit sets
 */
abstract class BitSet extends Set[Int] with collection.BitSet with BitSetTemplate[BitSet] {
  override def empty = BitSet.empty
  def fromArray(elems: Array[Long]): BitSet = BitSet.fromArray(elems)
}

/** A factory object for bitsets */
object BitSet {

  /** The empty bitset */
  val empty: BitSet = new BitSet1(0L)

  /** A bitset containing given elements */
  def apply(elems: Int*) = (empty /: elems) (_ + _)

  /** A bitset containing all the bits in an array */
  def fromArray(elems: Array[Long]): BitSet = {
    val len = elems.length
    if (len == 0) empty
    else if (len == 1) new BitSet1(elems(0))
    else if (len == 2) new BitSet2(elems(0), elems(1))
    else new BitSetN(elems)
  }

  private val hashSeed = "BitSet".hashCode

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

