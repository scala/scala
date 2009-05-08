/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import generic._

/** common base class for mutable and immutable bit sets
 */
trait BitSet extends Set[Int] with BitSetTemplate[BitSet] {
  override def empty = BitSet.empty
}

/** A factory object for bitsets */
object BitSet {

    /** The empty bitset */
  val empty: BitSet = immutable.BitSet.empty

  /** A bitset containing given elements */
  def apply(elems: Int*) = immutable.BitSet.apply(elems: _*)
}

