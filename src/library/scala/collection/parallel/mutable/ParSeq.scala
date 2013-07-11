/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection.parallel.mutable

import scala.collection.generic.GenericParTemplate
import scala.collection.generic.GenericCompanion
import scala.collection.generic.GenericParCompanion
import scala.collection.generic.CanCombineFrom
import scala.collection.generic.ParFactory
import scala.collection.parallel.ParSeqLike
import scala.collection.parallel.Combiner

/** A mutable variant of `ParSeq`.
 *
 *  @define Coll `mutable.ParSeq`
 *  @define coll mutable parallel sequence
 */
trait ParSeq[T] extends scala.collection/*.mutable*/.GenSeq[T] // was: scala.collection.mutable.Seq[T]
                   with ParIterable[T]
                   with scala.collection.parallel.ParSeq[T]
                   with GenericParTemplate[T, ParSeq]
                   with ParSeqLike[T, ParSeq[T], scala.collection.mutable.Seq[T]] {
self =>
  override def companion: GenericCompanion[ParSeq] with GenericParCompanion[ParSeq] = ParSeq
  //protected[this] override def newBuilder = ParSeq.newBuilder[T]

  def update(i: Int, elem: T): Unit

  def seq: scala.collection.mutable.Seq[T]

  override def toSeq: ParSeq[T] = this
}


/** $factoryInfo
 *  @define Coll `mutable.ParSeq`
 *  @define coll mutable parallel sequence
 */
object ParSeq extends ParFactory[ParSeq] {
  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParSeq[T]] = new GenericCanCombineFrom[T]

  def newBuilder[T]: Combiner[T, ParSeq[T]] = ParArrayCombiner[T]

  def newCombiner[T]: Combiner[T, ParSeq[T]] = ParArrayCombiner[T]
}
