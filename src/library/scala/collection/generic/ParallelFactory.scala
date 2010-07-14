package scala.collection.generic


import scala.collection.parallel.ParallelIterable
import scala.collection.parallel.Combiner



/** A template class for companion objects of `ParallelIterable` and subclasses thereof.
 *  This class extends `TraversableFactory` and provides a set of operations to create `$Coll` objects.
 *
 *  @define $coll parallel collection
 *  @define $Coll ParallelIterable
 */
abstract class ParallelFactory[CC[X] <: ParallelIterable[X] with GenericParallelTemplate[X, CC]]
extends TraversableFactory[CC]
   with GenericParallelCompanion[CC] {

  type EPC[T, C] = collection.parallel.EnvironmentPassingCombiner[T, C]

  /**
   * A generic implementation of the `CanCombineFrom` trait, which forwards all calls to
   * `apply(from)` to the `genericParallelBuilder` method of the $coll `from`, and calls to `apply()`
   * to this factory.
   */
  class GenericCanCombineFrom[A] extends GenericCanBuildFrom[A] with CanCombineFrom[CC[_], A, CC[A]] {
    override def apply(from: Coll) = from.genericCombiner
    override def apply() = newBuilder[A]
  }
}













