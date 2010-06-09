package scala.collection.generic



import scala.collection.parallel.Combiner
import scala.collection.parallel.ParallelIterable
import scala.collection.parallel.TaskSupport


import annotation.unchecked.uncheckedVariance






/** A template trait for collections having a companion.
 *
 *  @tparam A    the element type of the collection
 *  @tparam CC   the type constructor representing the collection class
 *  @since 2.8
 *  @author prokopec
 */
trait GenericParallelTemplate[+A, +CC[X] <: ParallelIterable[X]]
extends GenericTraversableTemplate[A, CC]
   with HasNewCombiner[A, CC[A] @uncheckedVariance]
   with TaskSupport
{
  def companion: GenericCompanion[CC] with GenericParallelCompanion[CC]

  protected[this] override def newBuilder: collection.mutable.Builder[A, CC[A]] = newCombiner

  protected[this] override def newCombiner: Combiner[A, CC[A]] = {
    val cb = companion.newCombiner[A]
    cb.environment = environment
    cb
  }

  override def genericBuilder[B]: Combiner[B, CC[B]] = genericCombiner[B]

  def genericCombiner[B]: Combiner[B, CC[B]] = {
    val cb = companion.newCombiner[B]
    cb.environment = environment
    cb
  }

}








