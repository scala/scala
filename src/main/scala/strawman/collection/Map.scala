package strawman.collection

import strawman.collection.mutable.Builder

import scala.Option
import scala.annotation.unchecked.uncheckedVariance
import scala.Predef.???

/** Base Map type */
trait Map[K, +V]
  extends Iterable[(K, V)]
    with MapLike[K, V, Map]

/** Base Map implementation type */
trait MapLike[K, +V, +C[X, Y] <: Map[X, Y]]
  extends IterableLike[(K, V), Iterable]
    with IterableMonoTransforms[(K, V), C[K, V @uncheckedVariance]]
    with MapPolyTransforms[K, V, C] {

  def get(key: K): Option[V]

}

/** Polymorphic transformation methods */
trait MapPolyTransforms[K, +V, +C[X, Y] <: Map[X, Y]] extends IterablePolyTransforms[(K, V), Iterable] {

  def map[K2, V2](f: (K, V) => (K2, V2)): C[K2, V2]

  def flatMap[K2, V2](f: (K, V) => IterableOnce[(K2, V2)]): C[K2, V2]

}

/** Factory methods for collections of kind `* −> * -> *` */
trait MapFactories[C[_, _]] {

  def newBuilder[K, V]: Builder[(K, V), C[K, V]]

  def empty[K, V]: C[K, V] =
    newBuilder[K, V].result

  def apply[K, V](elems: (K, V)*): C[K, V] =
    newBuilder[K, V].++=(elems.toStrawman).result

}