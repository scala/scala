/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection

import generic._

/** A common base class for mutable and immutable bitsets.
 *  $bitsetinfo
 */
trait BitSet extends SortedSet[Int]
                with BitSetLike[BitSet] {
  override def empty: BitSet = BitSet.empty
}

/** $factoryInfo
 *  @define coll bitset
 *  @define Coll `BitSet`
 */
object BitSet extends BitSetFactory[BitSet] {
  val empty: BitSet = immutable.BitSet.empty
  def newBuilder = immutable.BitSet.newBuilder

  /** $canBuildFromInfo */
  implicit val canBuildFrom: CanBuildFrom[BitSet, Int, BitSet] = bitsetCanBuildFrom
}

