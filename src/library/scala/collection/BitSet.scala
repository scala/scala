/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import generic._

/** A common base class for mutable and immutable bitsets.
 *  $bitsetinfo
 */
trait BitSet extends Set[Int]
                with BitSetLike[BitSet] {
  override def empty: BitSet = BitSet.empty
}

/** $factoryInfo
 *  @define coll bitset
 */
object BitSet extends BitSetFactory[BitSet] {
  val empty: BitSet = immutable.BitSet.empty
  /** $canBuildFromInfo */
  implicit def canBuildFrom: CanBuildFrom[BitSet, Int, BitSet] = bitsetCanBuildFrom
}

