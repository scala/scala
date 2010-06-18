package scala.collection.parallel



import scala.collection.generic.GenericCompanion
import scala.collection.generic.GenericParallelCompanion
import scala.collection.generic.GenericParallelTemplate
import scala.collection.generic.ParallelFactory
import scala.collection.generic.CanCombineFrom
import scala.collection.parallel.mutable.ParallelArrayCombiner
import scala.collection.parallel.mutable.ParallelArray


/** A template trait for parallel sequences.
 *
 *  $parallelseqinfo
 *
 *  $sideeffects
 */
trait ParallelSeq[+T] extends Seq[T]
                         with ParallelIterable[T]
                         with GenericParallelTemplate[T, ParallelSeq]
                         with ParallelSeqLike[T, ParallelSeq[T], Seq[T]] {
  override def companion: GenericCompanion[ParallelSeq] with GenericParallelCompanion[ParallelSeq] = ParallelSeq

  def apply(i: Int): T

  override def toString = super[ParallelIterable].toString
}


object ParallelSeq extends ParallelFactory[ParallelSeq] {
  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParallelSeq[T]] = new GenericCanCombineFrom[T]

  def newBuilder[T]: Combiner[T, ParallelSeq[T]] = ParallelArrayCombiner[T]

  def newCombiner[T]: Combiner[T, ParallelSeq[T]] = ParallelArrayCombiner[T]
}


























