package strawman
package collection

import collection.mutable.Builder

import scala.{Any, Boolean, None, NoSuchElementException, Nothing, Option, PartialFunction, Some}
import scala.annotation.unchecked.uncheckedVariance

/** Base Map type */
trait Map[K, +V]
  extends Iterable[(K, V)]
    with MapLike[K, V, Map]

/** Base Map implementation type */
trait MapLike[K, +V, +C[X, Y] <: Map[X, Y]]
  extends IterableLike[(K, V), Iterable]
    with MapMonoTransforms[K, V, C[K, V @uncheckedVariance]]
    with MapPolyTransforms[K, V, C]
    with PartialFunction[K, V] {

  /** Optionally returns the value associated with a key.
    *
    *  @param  key    the key value
    *  @return an option value containing the value associated with `key` in this map,
    *          or `None` if none exists.
    */
  def get(key: K): Option[V]

  /** Retrieves the value which is associated with the given key. This
    *  method invokes the `default` method of the map if there is no mapping
    *  from the given key to a value. Unless overridden, the `default` method throws a
    *  `NoSuchElementException`.
    *
    *  @param  key the key
    *  @return     the value associated with the given key, or the result of the
    *              map's `default` method, if none exists.
    */
  def apply(key: K): V = get(key) match {
    case None => default(key)
    case Some(value) => value
  }

  /** Defines the default value computation for the map,
    *  returned when a key is not found
    *  The method implemented here throws an exception,
    *  but it might be overridden in subclasses.
    *
    *  @param key the given key value for which a binding is missing.
    *  @throws NoSuchElementException
    */
  def default(key: K): V =
    throw new NoSuchElementException("key not found: " + key)

  /** Tests whether this map contains a binding for a key.
    *
    *  @param key the key
    *  @return    `true` if there is a binding for `key` in this map, `false` otherwise.
    */
  def contains(key: K): Boolean = get(key).isDefined


  /** Tests whether this map contains a binding for a key. This method,
    *  which implements an abstract method of trait `PartialFunction`,
    *  is equivalent to `contains`.
    *
    *  @param key the key
    *  @return    `true` if there is a binding for `key` in this map, `false` otherwise.
    */
  def isDefinedAt(key: K): Boolean = contains(key)

}

trait MapMonoTransforms[K, +V, +Repr]
  extends IterableMonoTransforms[(K, V), Repr]

/** Operations that return a Map collection with different types of keys or values (e.g. `map`) */
trait MapPolyTransforms[K, +V, +C[X, Y]]
  extends IterablePolyTransforms[(K, V), Iterable]
    with MapValuePolyTransforms[K, V, C] {

  /** Similar to fromIterable, but returns a Map collection type */
  protected def mapFromIterable[K2, V2](it: Iterable[(K2, V2)]): C[K2, V2]

  def map[K2, V2](f: (K, V) => (K2, V2)): C[K2, V2] = mapFromIterable(View.Map(coll, f.tupled))

  def flatMap[K2, V2](f: (K, V) => IterableOnce[(K2, V2)]): C[K2, V2] = mapFromIterable(View.FlatMap(coll, f.tupled))

  def concat [V2 >: V](xs: collection.Iterable[(K, V2)]): C[K, V2] = mapFromIterable(View.Concat(coll, xs))

}

/** Operations that return a Map collection with different types values (e.g. `concat`) */
trait MapValuePolyTransforms[K, +V, +C[X, Y]] {

  def concat [V2 >: V](xs: collection.Iterable[(K, V2)]): C[K, V2]

  /** Alias for `concat` */
  final def ++ [V2 >: V](xs: collection.Iterable[(K, V2)]): C[K, V2] = concat(xs)

}

/** Factory methods for collections of kind `* −> * -> *` */
trait MapFactory[+C[_, _]] { self =>
  def newBuilder[K, V]: Builder[(K, V), C[K, V]]

  def fromIterable[K, V](it: Iterable[(K, V)]): C[K, V] =
    newBuilder[K, V].++=(it).result

  def empty[K, V]: C[K, V]

  def apply[K, V](elems: (K, V)*): C[K, V] =
    newBuilder[K, V].++=(elems.toStrawman).result
}

/** Factory methods for collections of kind `* −> * -> *` which require an implicit evidence value for the key type */
trait ConstrainedMapFactory[+C[_, _], Ev[_]] { self =>

  def constrainedNewBuilder[K : Ev, V]: Builder[(K, V), C[K, V]]

  def constrainedFromIterable[K : Ev, V](it: Iterable[(K, V)]): C[K, V] =
    constrainedNewBuilder[K, V].++=(it).result

  def empty[K : Ev, V]: C[K, V]

  def apply[K : Ev, V](elems: (K, V)*): C[K, V] =
    constrainedNewBuilder[K, V].++=(elems.toStrawman).result
}
