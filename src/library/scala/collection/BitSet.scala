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
trait BitSet extends Set[Int]
                with BitSetLike[BitSet] {
  override def empty: BitSet = BitSet.empty
}

/** A factory object for bitsets */
object BitSet extends BitSetFactory[BitSet] {
  val empty: BitSet = immutable.BitSet.empty
}

