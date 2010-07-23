package scala.collection.generic


import scala.collection.parallel.ParIterable
import scala.collection.parallel.Combiner



/** A template class for companion objects of `ParIterable` and subclasses thereof.
 *  This class extends `TraversableFactory` and provides a set of operations to create `$Coll` objects.
 *
 *  @define $coll parallel collection
 *  @define $Coll ParIterable
 */
abstract class ParFactory[CC[X] <: ParIterable[X] with GenericParTemplate[X, CC]]
extends TraversableFactory[CC]
   with GenericParCompanion[CC] {

  type EPC[T, C] = collection.parallel.EnvironmentPassingCombiner[T, C]

  /**
   * A generic implementation of the `CanCombineFrom` trait, which forwards all calls to
   * `apply(from)` to the `genericParBuilder` method of the $coll `from`, and calls to `apply()`
   * to this factory.
   */
  class GenericCanCombineFrom[A] extends GenericCanBuildFrom[A] with CanCombineFrom[CC[_], A, CC[A]] {
    override def apply(from: Coll) = from.genericCombiner
    override def apply() = newBuilder[A]
  }
}













