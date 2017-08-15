package strawman
package collection

import collection.mutable.Builder

import scala.{Any, Boolean, ClassCastException, Equals, Int, NoSuchElementException, None, Nothing, Option, Ordering, PartialFunction, Serializable, Some, `inline`, throws}
import scala.annotation.unchecked.uncheckedVariance
import scala.util.hashing.MurmurHash3

/** Base Map type */
trait Map[K, +V] extends Iterable[(K, V)] with MapOps[K, V, Map, Map[K, V]] {
  final protected[this] def coll: this.type = this
}

/** Base Map implementation type */
trait MapOps[K, +V, +CC[X, Y] <: MapOps[X, Y, CC, _], +C <: MapOps[K, V, CC, C]]
  extends IterableOps[(K, V), Iterable, C]
    with PartialFunction[K, V]
    with Equals {

  /** Similar to fromIterable, but returns a Map collection type */
  protected[this] def mapFromIterable[K2, V2](it: Iterable[(K2, V2)]): CC[K2, V2]

  def mapFactory: MapFactory[CC]

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
  @throws[NoSuchElementException]
  def apply(key: K): V = get(key) match {
    case None => default(key)
    case Some(value) => value
  }

  override /*PartialFunction*/ def applyOrElse[K1 <: K, V1 >: V](x: K1, default: K1 => V1): V1 = getOrElse(x, default(x))

  /** Collects all keys of this map in a set.
    * @return  a set containing all keys of this map.
    */
  def keySet: Set[K] = new DefaultKeySet

  /** The implementation class of the set returned by `keySet`.
    */
  protected class DefaultKeySet extends Set[K] with Serializable {
    def contains(key: K) = MapOps.this.coll.contains(key)
    def iterator() = keysIterator()
    override def size = toIterable.size
    override def foreach[U](f: K => U) = keysIterator() foreach f
    def iterableFactory: IterableFactory[Set] = Set
    def empty: Set[K] = iterableFactory.empty
    def diff(that: Set[K]): Set[K] = fromSpecificIterable(iterableFactory.fromIterable(toIterable).diff(that))
    protected[this] def fromSpecificIterable(coll: Iterable[K]): Set[K] = iterableFactory.fromIterable(coll)
    protected[this] def newSpecificBuilder(): Builder[K, Set[K]] = iterableFactory.newBuilder()
  }

  /** Collects all keys of this map in an iterable collection.
    *
    *  @return the keys of this map as an iterable.
    */
  def keys: Iterable[K] = keySet

  /** Collects all values of this map in an iterable collection.
    *
    *  @return the values of this map as an iterable.
    */
  def values: Iterable[V] = View.fromIterator(valuesIterator())

  /** Creates an iterator for all keys.
    *
    *  @return an iterator over all keys.
    */
  def keysIterator(): Iterator[K] = new Iterator[K] {
    val iter = toIterable.iterator()
    def hasNext = iter.hasNext
    def next() = iter.next()._1
  }

  /** Creates an iterator for all values in this map.
    *
    *  @return an iterator over all values that are associated with some key in this map.
    */
  def valuesIterator(): Iterator[V] = new Iterator[V] {
    val iter = toIterable.iterator()
    def hasNext = iter.hasNext
    def next() = iter.next()._2
  }

  /** Filters this map by retaining only keys satisfying a predicate.
    *  @param  p   the predicate used to test keys
    *  @return an immutable map consisting only of those key value pairs of this map where the key satisfies
    *          the predicate `p`. The resulting map wraps the original map without copying any elements.
    */
  def filterKeys(p: K => Boolean): View[(K, V)] = View.FilterKeys(toIterable, p)

  /** Transforms this map by applying a function to every retrieved value.
    *  @param  f   the function used to transform values of this map.
    *  @return a map view which maps every key of this map
    *          to `f(this(key))`. The resulting map wraps the original map without copying any elements.
    */
  def mapValues[W](f: V => W): View[(K, W)] = View.MapValues(toIterable, f)

  /** Defines the default value computation for the map,
    *  returned when a key is not found
    *  The method implemented here throws an exception,
    *  but it might be overridden in subclasses.
    *
    *  @param key the given key value for which a binding is missing.
    *  @throws NoSuchElementException
    */
  @throws[NoSuchElementException]
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

  override def withFilter(p: ((K, V)) => Boolean): MapWithFilter = new MapWithFilter(p)

  /** Specializes `WithFilter` for Map collection types */
  class MapWithFilter(p: ((K, V)) => Boolean) extends WithFilter(p) {

    def map[K2, V2](f: ((K, V)) => (K2, V2)): CC[K2, V2] = mapFactory.fromIterable(View.Map(filtered, f))

    def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)]): CC[K2, V2] = mapFactory.fromIterable(View.FlatMap(filtered, f))

    override def withFilter(q: ((K, V)) => Boolean): MapWithFilter = new MapWithFilter(kv => p(kv) && q(kv))

  }

  def map[K2, V2](f: ((K, V)) => (K2, V2)): CC[K2, V2] = mapFromIterable(View.Map(toIterable, f))

  def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)]): CC[K2, V2] = mapFromIterable(View.FlatMap(toIterable, f))

  def concat[V2 >: V](xs: collection.Iterable[(K, V2)]): CC[K, V2] = mapFromIterable(View.Concat(toIterable, xs))

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

  override def hashCode(): Int = Set.unorderedHash(toIterable, "Map".##)

}

object Map extends MapFactory.Delegate[Map](immutable.Map)