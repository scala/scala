/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package generic

import scala.collection._
import mutable.Builder

/** @define coll collection
 *  @define Coll `Traversable`
 *  @define factoryInfo
 *    This object provides a set of operations to create `$Coll` values.
 *    @author Martin Odersky
 *    @version 2.8
 *  @define canBuildFromInfo
 *    The standard `CanBuildFrom` instance for $Coll objects.
 *    @see CanBuildFrom
 *  @define bitsetCanBuildFrom
 *    The standard `CanBuildFrom` instance for bitsets.
 */
trait BitSetFactory[Coll <: BitSet with BitSetLike[Coll]] {
  def empty: Coll
  def newBuilder: Builder[Int, Coll]
  def apply(elems: Int*): Coll = (empty /: elems) (_ + _)
  def bitsetCanBuildFrom = new CanBuildFrom[Coll, Int, Coll] {
    def apply(from: Coll) = newBuilder
    def apply() = newBuilder
  }
}

