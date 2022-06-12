/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection

import scala.annotation.nowarn
import scala.collection.generic.DefaultSerializable
import scala.collection.mutable.StringBuilder
import scala.util.hashing.MurmurHash3

/** Base Map type */
trait Map[K, +V]
  extends Iterable[(K, V)]
    with MapOps[K, V, Map, Map[K, V]]
    with MapFactoryDefaults[K, V, Map, Iterable]
    with Equals {

  def mapFactory: scala.collection.MapFactory[Map] = Map

  def canEqual(that: Any): Boolean = true

  /**
   * Equality of maps is implemented using the lookup method [[get]]. This method returns `true` if
   *   - the argument `o` is a `Map`,
   *   - the two maps have the same [[size]], and
   *   - for every `(key, value)` pair in this map, `other.get(key) == Some(value)`.
   *
   * The implementation of `equals` checks the [[canEqual]] method, so subclasses of `Map` can narrow down the equality
   * to specific map types. The `Map` implementations in the standard library can all be compared, their `canEqual`
   * methods return `true`.
   *
   * Note: The `equals` method only respects the equality laws (symmetry, transitivity) if the two maps use the same
   * key equivalence function in their lookup operation. For example, the key equivalence operation in a
   * [[scala.collection.immutable.TreeMap]] is defined by its ordering. Comparing a `TreeMap` with a `HashMap` leads
   * to unexpected results if `ordering.equiv(k1, k2)` (used for lookup in `TreeMap`) is different from `k1 == k2`
   * (used for lookup in `HashMap`).
   *
   * {{{
   *   scala> import scala.collection.immutable._
   *   scala> val ord: Ordering[String] = _ compareToIgnoreCase _
   *
   *   scala> TreeMap("A" -> 1)(ord) == HashMap("a" -> 1)
   *   val res0: Boolean = false
   *
   *   scala> HashMap("a" -> 1) == TreeMap("A" -> 1)(ord)
   *   val res1: Boolean = true
   * }}}
   *
   *
   * @param o The map to which this map is compared
   * @return `true` if the two maps are equal according to the description
   */
  override def equals(o: Any): Boolean =
    (this eq o.asInstanceOf[AnyRef]) || (o match {
      case map: Map[K @unchecked, _] if map.canEqual(this) =>
        (this.size == map.size) && {
          try this.forall(kv => map.getOrElse(kv._1, Map.DefaultSentinelFn()) == kv._2)
          catch { case _: ClassCastException => false } // PR #9565 / scala/bug#12228
        }
      case _ =>
        false
    })

  override def hashCode(): Int = MurmurHash3.mapHash(this)

  // These two methods are not in MapOps so that MapView is not forced to implement them
  @deprecated("Use - or removed on an immutable Map", "2.13.0")
  def - (key: K): Map[K, V]
  @deprecated("Use -- or removedAll on an immutable Map", "2.13.0")
  def - (key1: K, key2: K, keys: K*): Map[K, V]

  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected[this] def stringPrefix: String = "Map"

  override def toString(): String = super[Iterable].toString() // Because `Function1` overrides `toString` too
}

/** Base Map implementation type
  *
  * @tparam K Type of keys
  * @tparam V Type of values
  * @tparam CC type constructor of the map (e.g. `HashMap`). Operations returning a collection
  *            with a different type of entries `(L, W)` (e.g. `map`) return a `CC[L, W]`.
  * @tparam C  type of the map (e.g. `HashMap[Int, String]`). Operations returning a collection
  *            with the same type of element (e.g. `drop`, `filter`) return a `C`.
  * @define coll map
  * @define Coll `Map`
  */
// Note: the upper bound constraint on CC is useful only to
// erase CC to IterableOps instead of Object
trait MapOps[K, +V, +CC[_, _] <: IterableOps[_, AnyConstr, _], +C]
  extends IterableOps[(K, V), Iterable, C]
    with PartialFunction[K, V] {

  override def view: MapView[K, V] = new MapView.Id(this)

  /** Returns a [[Stepper]] for the keys of this map. See method [[stepper]]. */
  def keyStepper[S <: Stepper[_]](implicit shape: StepperShape[K, S]): S = {
    import convert.impl._
    val s = shape.shape match {
      case StepperShape.IntShape    => new IntIteratorStepper   (keysIterator.asInstanceOf[Iterator[Int]])
      case StepperShape.LongShape   => new LongIteratorStepper  (keysIterator.asInstanceOf[Iterator[Long]])
      case StepperShape.DoubleShape => new DoubleIteratorStepper(keysIterator.asInstanceOf[Iterator[Double]])
      case _                        => shape.seqUnbox(new AnyIteratorStepper(keysIterator))
    }
    s.asInstanceOf[S]
  }

  /** Returns a [[Stepper]] for the values of this map. See method [[stepper]]. */
  def valueStepper[S <: Stepper[_]](implicit shape: StepperShape[V, S]): S = {
    import convert.impl._
    val s = shape.shape match {
      case StepperShape.IntShape    => new IntIteratorStepper   (valuesIterator.asInstanceOf[Iterator[Int]])
      case StepperShape.LongShape   => new LongIteratorStepper  (valuesIterator.asInstanceOf[Iterator[Long]])
      case StepperShape.DoubleShape => new DoubleIteratorStepper(valuesIterator.asInstanceOf[Iterator[Double]])
      case _                        => shape.seqUnbox(new AnyIteratorStepper(valuesIterator))
    }
    s.asInstanceOf[S]
  }

  /** Similar to `fromIterable`, but returns a Map collection type.
    * Note that the return type is now `CC[K2, V2]`.
    */
  @`inline` protected final def mapFromIterable[K2, V2](it: Iterable[(K2, V2)]): CC[K2, V2] = mapFactory.from(it)

  /** The companion object of this map, providing various factory methods.
    *
    * @note When implementing a custom collection type and refining `CC` to the new type, this
    *       method needs to be overridden to return a factory for the new type (the compiler will
    *       issue an error otherwise).
    */
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
  def keySet: Set[K] = new KeySet

  /** The implementation class of the set returned by `keySet`.
    */
  protected class KeySet extends AbstractSet[K] with GenKeySet with DefaultSerializable {
    def diff(that: Set[K]): Set[K] = fromSpecific(this.view.filterNot(that))
  }

  /** A generic trait that is reused by keyset implementations */
  protected trait GenKeySet { this: Set[K] =>
    def iterator: Iterator[K] = MapOps.this.keysIterator
    def contains(key: K): Boolean = MapOps.this.contains(key)
    override def size: Int = MapOps.this.size
    override def knownSize: Int = MapOps.this.knownSize
    override def isEmpty: Boolean = MapOps.this.isEmpty
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
  def values: Iterable[V] = new AbstractIterable[V] with DefaultSerializable {
    override def knownSize: Int = MapOps.this.knownSize
    override def iterator: Iterator[V] = valuesIterator
  }

  /** Creates an iterator for all keys.
    *
    *  @return an iterator over all keys.
    */
  def keysIterator: Iterator[K] = new AbstractIterator[K] {
    val iter = MapOps.this.iterator
    def hasNext = iter.hasNext
    def next() = iter.next()._1
  }

  /** Creates an iterator for all values in this map.
    *
    *  @return an iterator over all values that are associated with some key in this map.
    */
  def valuesIterator: Iterator[V] = new AbstractIterator[V] {
    val iter = MapOps.this.iterator
    def hasNext = iter.hasNext
    def next() = iter.next()._2
  }

  /** Apply `f` to each key/value pair for its side effects
   *  Note: [U] parameter needed to help scalac's type inference.
   */
  def foreachEntry[U](f: (K, V) => U): Unit = {
    val it = iterator
    while (it.hasNext) {
      val next = it.next()
      f(next._1, next._2)
    }
  }

  /** Filters this map by retaining only keys satisfying a predicate.
    *  @param  p   the predicate used to test keys
    *  @return an immutable map consisting only of those key value pairs of this map where the key satisfies
    *          the predicate `p`. The resulting map wraps the original map without copying any elements.
    */
  @deprecated("Use .view.filterKeys(f). A future version will include a strict version of this method (for now, .view.filterKeys(p).toMap).", "2.13.0")
  def filterKeys(p: K => Boolean): MapView[K, V] = new MapView.FilterKeys(this, p)

  /** Transforms this map by applying a function to every retrieved value.
    *  @param  f   the function used to transform values of this map.
    *  @return a map view which maps every key of this map
    *          to `f(this(key))`. The resulting map wraps the original map without copying any elements.
    */
  @deprecated("Use .view.mapValues(f). A future version will include a strict version of this method (for now, .view.mapValues(f).toMap).", "2.13.0")
  def mapValues[W](f: V => W): MapView[K, W] = new MapView.MapValues(this, f)

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

  /** Builds a new map by applying a function to all elements of this $coll.
    *
    *  @param f      the function to apply to each element.
    *  @return       a new $coll resulting from applying the given function
    *                `f` to each element of this $coll and collecting the results.
    */
  def map[K2, V2](f: ((K, V)) => (K2, V2)): CC[K2, V2] = mapFactory.from(new View.Map(this, f))

  /** Builds a new collection by applying a partial function to all elements of this $coll
    *  on which the function is defined.
    *
    *  @param pf     the partial function which filters and maps the $coll.
    *  @tparam K2    the key type of the returned $coll.
    *  @tparam V2    the value type of the returned $coll.
    *  @return       a new $coll resulting from applying the given partial function
    *                `pf` to each element on which it is defined and collecting the results.
    *                The order of the elements is preserved.
    */
  def collect[K2, V2](pf: PartialFunction[(K, V), (K2, V2)]): CC[K2, V2] =
    mapFactory.from(new View.Collect(this, pf))

  /** Builds a new map by applying a function to all elements of this $coll
    *  and using the elements of the resulting collections.
    *
    *  @param f      the function to apply to each element.
    *  @return       a new $coll resulting from applying the given collection-valued function
    *                `f` to each element of this $coll and concatenating the results.
    */
  def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)]): CC[K2, V2] = mapFactory.from(new View.FlatMap(this, f))

  /** Returns a new $coll containing the elements from the left hand operand followed by the elements from the
    *  right hand operand. The element type of the $coll is the most specific superclass encompassing
    *  the element types of the two operands.
    *
    *  @param suffix   the traversable to append.
    *  @return       a new $coll which contains all elements
    *                of this $coll followed by all elements of `suffix`.
    */
  def concat[V2 >: V](suffix: collection.IterableOnce[(K, V2)]): CC[K, V2] = mapFactory.from(suffix match {
    case it: Iterable[(K, V2)] => new View.Concat(this, it)
    case _ => iterator.concat(suffix.iterator)
  })

  // Not final because subclasses refine the result type, e.g. in SortedMap, the result type is
  // SortedMap's CC, while Map's CC is fixed to Map
  /** Alias for `concat` */
  /*@`inline` final*/ def ++ [V2 >: V](xs: collection.IterableOnce[(K, V2)]): CC[K, V2] = concat(xs)

  override def addString(sb: StringBuilder, start: String, sep: String, end: String): sb.type =
    iterator.map { case (k, v) => s"$k -> $v" }.addString(sb, start, sep, end)

  @deprecated("Consider requiring an immutable Map or fall back to Map.concat.", "2.13.0")
  def + [V1 >: V](kv: (K, V1)): CC[K, V1] =
    mapFactory.from(new View.Appended(this, kv))

  @deprecated("Use ++ with an explicit collection argument instead of + with varargs", "2.13.0")
  def + [V1 >: V](elem1: (K, V1), elem2: (K, V1), elems: (K, V1)*): CC[K, V1] =
    mapFactory.from(new View.Concat(new View.Appended(new View.Appended(this, elem1), elem2), elems))

  @deprecated("Consider requiring an immutable Map.", "2.13.0")
  @`inline` def -- (keys: IterableOnce[K]): C = {
    lazy val keysSet = keys.iterator.to(immutable.Set)
    fromSpecific(this.view.filterKeys(k => !keysSet.contains(k)))
  }

  @deprecated("Use ++ instead of ++: for collections of type Iterable", "2.13.0")
  def ++: [V1 >: V](that: IterableOnce[(K,V1)]): CC[K,V1] = {
    val thatIterable: Iterable[(K, V1)] = that match {
      case that: Iterable[(K, V1)] => that
      case that => View.from(that)
    }
    mapFactory.from(new View.Concat(thatIterable, this))
  }
}

object MapOps {
  /** Specializes `WithFilter` for Map collection types by adding overloads to transformation
    * operations that can return a Map.
    *
    * @define coll map collection
    */
  @SerialVersionUID(3L)
  class WithFilter[K, +V, +IterableCC[_], +CC[_, _] <: IterableOps[_, AnyConstr, _]](
    self: MapOps[K, V, CC, _] with IterableOps[(K, V), IterableCC, _],
    p: ((K, V)) => Boolean
  ) extends IterableOps.WithFilter[(K, V), IterableCC](self, p) with Serializable {

    def map[K2, V2](f: ((K, V)) => (K2, V2)): CC[K2, V2] =
      self.mapFactory.from(new View.Map(filtered, f))

    def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)]): CC[K2, V2] =
      self.mapFactory.from(new View.FlatMap(filtered, f))

    override def withFilter(q: ((K, V)) => Boolean): WithFilter[K, V, IterableCC, CC] =
      new WithFilter[K, V, IterableCC, CC](self, (kv: (K, V)) => p(kv) && q(kv))

  }

}

/**
  * $factoryInfo
  * @define coll map
  * @define Coll `Map`
  */
@SerialVersionUID(3L)
object Map extends MapFactory.Delegate[Map](immutable.Map) {
  private val DefaultSentinel: AnyRef = new AnyRef
  private val DefaultSentinelFn: () => AnyRef = () => DefaultSentinel
}

/** Explicit instantiation of the `Map` trait to reduce class file size in subclasses. */
abstract class AbstractMap[K, +V] extends AbstractIterable[(K, V)] with Map[K, V]
