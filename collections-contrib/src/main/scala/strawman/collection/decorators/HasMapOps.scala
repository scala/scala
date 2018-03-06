package strawman.collection
package decorators

/**
  * Type class witnessing that a collection type `C`
  * has keys of type `K`, values of type `V` and has a conversion to `MapOps[K, V, _, _]`.
  *
  * This type enables simple enrichment of `Map`s with extension methods.
  *
  * @tparam C Collection type (e.g. `Map[Int, String]`)
  */
trait HasMapOps[C] extends HasIterableOps[C] {

  /** The type of keys */
  type K

  /** The type of values */
  type V

  type A = (K, V)

  /** A conversion from the type `C` to `MapOps[K, V, _, _]` */
  def apply(c: C): MapOps[K, V, ({ type l[X, +Y] = IterableOps[_, AnyConstr, _] })#l, _]

}

object HasMapOps extends LowPriorityHasMapOps {

  // 1. Map collections
  implicit def mapHasMapOps[CC[X, +Y] <: MapOps[X, Y, ({ type l[X, +Y] = IterableOps[_, AnyConstr, _] })#l, _], K0, V0]: HasMapOps[CC[K0, V0]] { type K = K0; type V = V0 } =
    new HasMapOps[CC[K0, V0]] {
      type K = K0
      type V = V0
      def apply(c: CC[K0, V0]): MapOps[K0, V0, ({ type l[X, +Y] = IterableOps[_, AnyConstr, _] })#l, _] = c
    }

}

trait LowPriorityHasMapOps {

  // Makes `HasImmutableMapOps` instances visible in `HasMapOps` companion
  implicit def hasImmutableMapOpsHasMapOps[C, K0, V0](implicit
    hasImmutableMapOps: HasImmutableMapOps[C] { type K = K0; type V = V0 }
  ): HasMapOps[C] { type K = K0; type V = V0 } = hasImmutableMapOps

}