package strawman.collection
package decorators

/**
  * Type class witnessing that a collection type `C` has
  * a conversion to `immutable.MapOps[K, V, _, _]`.
  *
  * @see [[scala.collection.decorators.HasIterableOps]]
  */
trait HasImmutableMapOps[C] extends HasMapOps[C] {

  // Convenient intermediate type definitions to satisfy type bounds
  protected type _CC[X, +Y] <: immutable.MapOps[X, Y, _CC, _]
  protected type _C <: immutable.MapOps[K, V, _CC, _C]

  /** A conversion from the type `C` to `immutable.MapOps[K, V, _, _]` */
  def apply(c: C): immutable.MapOps[K, V, _CC, _C]

}

object HasImmutableMapOps {

  // 1. Map collections
  implicit def mapHasMapOps[CC[X, +Y] <: immutable.MapOps[X, Y, CC, CC[X, Y]], K0, V0]: HasImmutableMapOps[CC[K0, V0]] { type K = K0; type V = V0 } =
    new HasImmutableMapOps[CC[K0, V0]] {
      type K = K0
      type V = V0
      type _CC[X, +Y] = CC[X, Y]
      type _C = CC[K, V]
      def apply(c: CC[K0, V0]): immutable.MapOps[K0, V0, _CC, _C] = c
    }

  // 2. Sorted Map collections
  implicit def sortedMapHasMapOps[CC[X, +Y] <: immutable.Map[X, Y] with immutable.SortedMapOps[X, Y, CC, CC[X, Y]], K0, V0]: HasImmutableMapOps[CC[K0, V0]] { type K = K0; type V = V0 } =
    new HasImmutableMapOps[CC[K0, V0]] {
      type K = K0
      type V = V0
      type _CC[X, +Y] = immutable.Map[X, Y]
      type _C = _CC[K, V]
      def apply(c: CC[K0, V0]): immutable.MapOps[K0, V0, _CC, _C] = c
    }

}