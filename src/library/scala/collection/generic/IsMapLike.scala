package scala.collection
package generic

import IsMapLike.Tupled
import scala.collection.immutable.{IntMap, LongMap}
import scala.language.higherKinds

/**
  * Type class witnessing that a collection type `Repr`
  * has keys of type `K`, values of type `V` and has a conversion to
  * `MapOps[K, V, Iterable, C]`, for some types `K`, `V` and `C`.
  *
  * This type enables simple enrichment of `Map`s with extension methods.
  *
  * @see [[scala.collection.generic.IsIterableLike]]
  * @tparam Repr Collection type (e.g. `Map[Int, String]`)
  */
trait IsMapLike[Repr] extends IsIterableLike[Repr] {

  /** The type of keys */
  type K

  /** The type of values */
  type V

  type A = (K, V)

  /** A conversion from the type `Repr` to `MapOps[K, V, Iterable, C]`
    *
    * @note The third type parameter of the returned `MapOps` value is
    *       still `Iterable` (and not `Map`) because `MapView[K, V]` only
    *       extends `MapOps[K, V, View, View[A]]`.
    */
  override def apply(c: Repr): MapOps[K, V, Tupled[Iterable]#Ap, C]

}

object IsMapLike {

  /** Convenient type level function that takes a unary type constructor `F[_]`
    * and returns a binary type constructor that tuples its parameters and passes
    * them to `F`.
    *
    * `Tupled[F]#Ap` is equivalent to `({ type Ap[X, +Y] = F[(X, Y)] })#Ap`.
    */
  type Tupled[F[+_]] = { type Ap[X, Y] = F[(X, Y)] }

  // Map collections
  implicit def mapOpsIsMapLike[CC0[X, Y] <: MapOps[X, Y, Tupled[Iterable]#Ap, CC0[X, Y]], K0, V0]: IsMapLike[CC0[K0, V0]] { type K = K0; type V = V0; type C = CC0[K, V] } =
    new IsMapLike[CC0[K0, V0]] {
      type K = K0
      type V = V0
      type C = CC0[K0, V0]
      def apply(c: CC0[K0, V0]): MapOps[K0, V0, Tupled[Iterable]#Ap, C] = c
    }

  // MapView
  implicit def mapViewIsMapLike[CC0[X, Y] <: MapView[X, Y], K0, V0]: IsMapLike[CC0[K0, V0]] { type K = K0; type V = V0; type C = View[(K0, V0)] } =
    new IsMapLike[CC0[K0, V0]] {
      type K = K0
      type V = V0
      type C = View[(K, V)]
      def apply(c: CC0[K0, V0]): MapOps[K0, V0, Tupled[Iterable]#Ap, View[(K0, V0)]] = c
    }

  // AnyRefMap has stricter bounds than the ones used by the mapOpsIsMapLike definition
  implicit def anyRefMapIsMapLike[K0 <: AnyRef, V0]: IsMapLike[mutable.AnyRefMap[K0, V0]] { type K = K0; type V = V0; type C = mutable.AnyRefMap[K0, V0] } =
    new IsMapLike[mutable.AnyRefMap[K0, V0]] {
      type K = K0
      type V = V0
      type C = mutable.AnyRefMap[K0, V0]
      def apply(c: mutable.AnyRefMap[K0, V0]): MapOps[K0, V0, Tupled[Iterable]#Ap, mutable.AnyRefMap[K0, V0]] = c
    }

  // IntMap takes one type parameter only whereas mapOpsIsMapLike uses a parameter CC0 with two type parameters
  implicit def intMapIsMapLike[V0]: IsMapLike[IntMap[V0]] { type K = Int; type V = V0; type C = IntMap[V0] } =
    new IsMapLike[IntMap[V0]] {
      type K = Int
      type V = V0
      type C = IntMap[V0]
      def apply(c: IntMap[V0]): MapOps[Int, V0, Tupled[Iterable]#Ap, IntMap[V0]] = c
    }

  // LongMap is in a similar situation as IntMap
  implicit def longMapIsMapLike[V0]: IsMapLike[LongMap[V0]] { type K = Long; type V = V0; type C = LongMap[V0] } =
    new IsMapLike[LongMap[V0]] {
      type K = Long
      type V = V0
      type C = LongMap[V0]
      def apply(c: LongMap[V0]): MapOps[Long, V0, Tupled[Iterable]#Ap, LongMap[V0]] = c
    }

  // mutable.LongMap is in a similar situation as LongMap and IntMap
  implicit def mutableLongMapIsMapLike[V0]: IsMapLike[mutable.LongMap[V0]] { type K = Long; type V = V0; type C = mutable.LongMap[V0] } =
    new IsMapLike[mutable.LongMap[V0]] {
      type K = Long
      type V = V0
      type C = mutable.LongMap[V0]
      def apply(c: mutable.LongMap[V0]): MapOps[Long, V0, Tupled[Iterable]#Ap, mutable.LongMap[V0]] = c
    }


}
