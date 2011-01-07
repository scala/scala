package scala.collection
package parallel.immutable


import scala.collection.generic.GenericParTemplate
import scala.collection.generic.GenericCompanion
import scala.collection.generic.GenericParCompanion
import scala.collection.generic.CanCombineFrom
import scala.collection.generic.ParFactory
import scala.collection.parallel.ParSeqLike
import scala.collection.parallel.Combiner




/** An immutable variant of `ParSeq`.
 *
 *  @define Coll mutable.ParSeq
 *  @define coll mutable parallel sequence
 */
trait ParSeq[+T]
extends collection.immutable.Seq[T]
   with collection.parallel.ParSeq[T]
   with ParIterable[T]
   with GenericParTemplate[T, ParSeq]
   with ParSeqLike[T, ParSeq[T], Seq[T]]
{
  override def companion: GenericCompanion[ParSeq] with GenericParCompanion[ParSeq] = ParSeq
  override def toSeq: ParSeq[T] = this
}


/** $factoryInfo
 *  @define Coll mutable.ParSeq
 *  @define coll mutable parallel sequence
 */
object ParSeq extends ParFactory[ParSeq] {
 implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParSeq[T]] = new GenericCanCombineFrom[T]

 def newBuilder[T]: Combiner[T, ParSeq[T]] = ParVector.newBuilder[T]

 def newCombiner[T]: Combiner[T, ParSeq[T]] = ParVector.newCombiner[T]
}



