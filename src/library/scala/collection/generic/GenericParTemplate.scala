/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2010-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package generic

import scala.collection.parallel.Combiner
import scala.collection.parallel.ParIterable
import scala.collection.parallel.ParMap

import scala.annotation.unchecked.uncheckedVariance
import scala.language.higherKinds

/** A template trait for collections having a companion.
 *
 *  @tparam A    the element type of the collection
 *  @tparam CC   the type constructor representing the collection class
 *  @author Aleksandar Prokopec
 *  @since 2.8
 */
trait GenericParTemplate[+A, +CC[X] <: ParIterable[X]]
extends GenericTraversableTemplate[A, CC]
   with HasNewCombiner[A, CC[A] @uncheckedVariance]
{
  def companion: GenericCompanion[CC] with GenericParCompanion[CC]

  protected[this] override def newBuilder: scala.collection.mutable.Builder[A, CC[A]] = newCombiner

  protected[this] override def newCombiner: Combiner[A, CC[A]] = {
    val cb = companion.newCombiner[A]
    cb
  }

  override def genericBuilder[B]: Combiner[B, CC[B]] = genericCombiner[B]

  def genericCombiner[B]: Combiner[B, CC[B]] = {
    val cb = companion.newCombiner[B]
    cb
  }

}


trait GenericParMapTemplate[K, +V, +CC[X, Y] <: ParMap[X, Y]] extends GenericParTemplate[(K, V), ParIterable]
{
  protected[this] override def newCombiner: Combiner[(K, V), CC[K, V]] = {
    val cb = mapCompanion.newCombiner[K, V]
    cb
  }

  def mapCompanion: GenericParMapCompanion[CC]

  def genericMapCombiner[P, Q]: Combiner[(P, Q), CC[P, Q]] = {
    val cb = mapCompanion.newCombiner[P, Q]
    cb
  }
}

