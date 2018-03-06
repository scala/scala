package strawman.collection
package decorators

/**
  * Type class witnessing that a collection type `C`
  * has elements of type `A` and has a conversion to `IterableOps[A, _, _]`.
  *
  * This type enables simple enrichment of `Iterable`s with extension methods.
  *
  * @tparam C Collection type (e.g. `List[Int]`)
  */
trait HasIterableOps[C] {

  /** The type of elements (e.g. `Int`) */
  type A

  /** A conversion from the type `C` to `IterableOps[A, _, _]` */
  def apply(c: C): IterableOps[A, AnyConstr, _]

}

object HasIterableOps extends LowPriorityHasIterableOps {

  implicit def iterableHasIterableOps[CC[X] <: IterableOps[X, AnyConstr, _], A0]: HasIterableOps[CC[A0]] { type A = A0 } =
    new HasIterableOps[CC[A0]] {
      type A = A0
      def apply(c: CC[A0]): IterableOps[A0, AnyConstr, _] = c
    }

}

trait LowPriorityHasIterableOps {

  // Makes `HasSeqOps` instances visible in `HasIterableOps` companion
  implicit def hasSeqOpsHasIterableOps[C, A0](implicit
    hasSeqOps: HasSeqOps[C] { type A = A0 }
  ): HasIterableOps[C] { type A = A0 } = hasSeqOps

  // Makes `HasMapOps` instances visible in `HasIterableOps` companion
  implicit def hasMapOpsHasIterableOps[C, K0, V0](implicit
    hasMapOps: HasMapOps[C] { type K = K0; type V = V0 }
  ): HasIterableOps[C] { type A = (K0, V0) } = hasMapOps

}