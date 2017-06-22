package strawman
package collection

import collection.mutable.Builder

import scala.{Any, Boolean, ClassCastException, Equals, Int, NoSuchElementException, None, Nothing, Option, Ordering, PartialFunction, Some, `inline`}
import scala.annotation.unchecked.uncheckedVariance
import scala.util.hashing.MurmurHash3

/** Base Map type */
trait Map[K, +V] extends Iterable[(K, V)] with MapOps[K, V, Map, Map[K, V]]

/** Base Map implementation type */
trait MapOps[K, +V, +CC[X, Y] <: Map[X, Y], +C <: Map[K, V]]
  extends IterableOps[(K, V), Iterable, C]
    with PartialFunction[K, V]
    with Equals {

  protected[this] def coll: Map[K, V]

  /** Similar to fromIterable, but returns a Map collection type */
  protected[this] def mapFromIterable[K2, V2](it: Iterable[(K2, V2)]): CC[K2, V2]

  /** Optionally returns the value associated with a key.
    *
    *  @param  key    the key value
    *  @return an option value containing the value associated with `key` in this map,
    *          or `None` if none exists.
    */
  def get(key: K): Option[V]

  /**  Returns the value associated with a key, or a default value if the key is not contained in the map.
   *   @param   key      the key.
   *   @param   default  a computation that yields a default value in case no binding for `key` is
   *                     found in the map.
   *   @tparam  V1       the result type of the default computation.
   *   @return  the value associated with `key` if it exists,
   *            otherwise the result of the `default` computation.
   *
   *   @usecase def getOrElse(key: K, default: => V): V
   *     @inheritdoc
   */
  def getOrElse[V1 >: V](key: K, default: => V1): V1 = get(key) match {
    case Some(v) => v
    case None => default
  }

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

  /** The empty map of the same type as this map
    * @return an empty map of type `Repr`.
    */
  def empty: C

  def map[K2, V2](f: ((K, V)) => (K2, V2)): CC[K2, V2] = mapFromIterable(View.Map(coll, f))

  def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)]): CC[K2, V2] = mapFromIterable(View.FlatMap(coll, f))

  def concat [V2 >: V](xs: collection.Iterable[(K, V2)]): CC[K, V2] = mapFromIterable(View.Concat(coll, xs))

  /** Alias for `concat` */
  /*@`inline` final*/ def ++ [V2 >: V](xs: collection.Iterable[(K, V2)]): CC[K, V2] = concat(xs)

  def canEqual(that: Any): Boolean = true

  override def equals(o: Any): Boolean = o match {
    case that: Map[b, _] =>
      (this eq that) ||
      (that canEqual this) &&
      (this.size == that.size) && {
        try {
          this forall {
            case (k, v) => that.get(k.asInstanceOf[b]) match {
              case Some(`v`) =>
                true
              case _ => false
            }
          }
        } catch {
          case _: ClassCastException => false
        }
      }
    case _ =>
      false
  }

  override def hashCode(): Int = Set.unorderedHash(coll, "Map".##)

}

object Map extends MapFactoryWithBuilder.Delegate[Map](immutable.Map)