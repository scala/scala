package scala.collection.parallel.mutable


import scala.collection.generic.GenericParallelTemplate
import scala.collection.generic.GenericCompanion
import scala.collection.generic.GenericParallelCompanion
import scala.collection.generic.CanCombineFrom
import scala.collection.generic.ParallelFactory
import scala.collection.parallel.ParallelSeqLike
import scala.collection.parallel.Combiner







/** A mutable variant of `ParallelSeq`.
 *
 *  @define Coll mutable.ParallelSeq
 *  @define coll mutable parallel sequence
 */
trait ParallelSeq[T] extends collection.mutable.Seq[T]
                        with ParallelIterable[T]
                        with collection.parallel.ParallelSeq[T]
                        with GenericParallelTemplate[T, ParallelSeq]
                        with ParallelSeqLike[T, ParallelSeq[T], Seq[T]] {
  self =>
  override def companion: GenericCompanion[ParallelSeq] with GenericParallelCompanion[ParallelSeq] = ParallelSeq

  def update(i: Int, elem: T): Unit

}


/** $factoryInfo
 *  @define Coll mutable.ParallelSeq
 *  @define coll mutable parallel sequence
 */
object ParallelSeq extends ParallelFactory[ParallelSeq] {
  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParallelSeq[T]] = new GenericCanCombineFrom[T]

  def newBuilder[T]: Combiner[T, ParallelSeq[T]] = ParallelArrayCombiner[T]

  def newCombiner[T]: Combiner[T, ParallelSeq[T]] = ParallelArrayCombiner[T]
}















