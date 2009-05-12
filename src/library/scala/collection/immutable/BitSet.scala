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
import BitSetTemplate.{LogWL, updateArray}

/** a base class for immutable bit sets
 */
abstract class BitSet extends Set[Int] with collection.BitSet with BitSetTemplate[BitSet] {
  override def empty = BitSet.empty
  def fromArray(elems: Array[Long]): BitSet = BitSet.fromArray(elems)

  /** Update word at index `idx`; enlarge set if `idx` outside range of set
   */
  protected def updateWord(idx: Int, w: Long): BitSet

  /** Adds element to bitset, returning a new set.
   */
  def plus (elem: Int): BitSet = {
    require(elem >= 0)
    if (contains(elem)) this
    else {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) | (1L << elem))
    }
  }

  /** Removes element from bitset, returning a new set
   */
  def minus (elem: Int): BitSet = {
    require(elem >= 0)
    if (contains(elem)) {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) & ~(1L << elem))
    } else this
  }
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

  class BitSet1(val elems: Long) extends BitSet {
    protected def nwords = 1
    protected def word(idx: Int) = if (idx == 0) elems else 0L
    protected def updateWord(idx: Int, w: Long): BitSet =
      if (idx == 0) new BitSet1(w)
      else if (idx == 1) new BitSet2(elems, w)
      else fromArray(updateArray(Array(elems), idx, w))
  }

  class BitSet2(val elems0: Long, elems1: Long) extends BitSet {
    protected def nwords = 2
    protected def word(idx: Int) = if (idx == 0) elems0 else if (idx == 1) elems1 else 0L
    protected def updateWord(idx: Int, w: Long): BitSet =
      if (idx == 0) new BitSet2(w, elems1)
      else if (idx == 1) new BitSet2(elems0, w)
      else fromArray(updateArray(Array(elems0, elems1), idx, w))
  }

  class BitSetN(val elems: Array[Long]) extends BitSet {
    protected def nwords = elems.length
    protected def word(idx: Int) = if (idx < nwords) elems(idx) else 0L
    protected def updateWord(idx: Int, w: Long): BitSet = fromArray(updateArray(elems, idx, w))
  }
}

