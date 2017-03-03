/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

import generic._
import mutable.{ Builder, MapBuilder }
import scala.annotation.migration
import parallel.ParMap

/** A template trait for maps, which associate keys with values.
 *
 *  $mapNote
 *  $mapTags
 *  @since 2.8
 *
 *  @define mapNote
 *    '''Implementation note:'''
 *    This trait provides most of the operations of a `Map` independently of its representation.
 *    It is typically inherited by concrete implementations of maps.
 *
 *    To implement a concrete map, you need to provide implementations of the
 *    following methods:
 *    {{{
 *       def get(key: K): Option[V]
 *       def iterator: Iterator[(K, V)]
 *       def + [V1 >: V](kv: (K, V1)): This
 *       def -(key: K): This
 *    }}}
 *    If you wish that methods like `take`, `drop`, `filter` also return the same kind of map
 *    you should also override:
 *    {{{
 *       def empty: This
 *    }}}
 *    It is also good idea to override methods `foreach` and
 *    `size` for efficiency.
 *
 *  @define mapTags
 *  @tparam K    the type of the keys.
 *  @tparam V    the type of associated values.
 *  @tparam This the type of the map itself.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *
 *  @define coll map
 *  @define Coll Map
 *  @define willNotTerminateInf
 *  @define mayNotTerminateInf
 */
trait MapLike[K, +V, +This <: MapLike[K, V, This] with Map[K, V]]
  extends PartialFunction[K, V]
     with IterableLike[(K, V), This]
     with GenMapLike[K, V, This]
     with Subtractable[K, This]
     with Parallelizable[(K, V), ParMap[K, V]]
{
self =>

  /** The empty map of the same type as this map
   *   @return   an empty map of type `This`.
   */
  def empty: This

  /** A common implementation of `newBuilder` for all maps in terms of `empty`.
   *  Overridden for mutable maps in `mutable.MapLike`.
   */
  override protected[this] def newBuilder: Builder[(K, V), This] = new MapBuilder[K, V, This](empty)

  /** Optionally returns the value associated with a key.
   *
   *  @param  key    the key value
   *  @return an option value containing the value associated with `key` in this map,
   *          or `None` if none exists.
   */
  def get(key: K): Option[V]

  /** Creates a new iterator over all key/value pairs of this map
   *
   *  @return the new iterator
   */
  def iterator: Iterator[(K, V)]

  /** Adds a key/value pair to this map, returning a new map.
   *  @param    kv the key/value pair
   *  @tparam   V1 the type of the value in the key/value pair.
   *  @return   a new map with the new binding added to this map
   *
   *  @usecase  def + (kv: (K, V)): Map[K, V]
   *    @inheritdoc
   */
  def + [V1 >: V] (kv: (K, V1)): Map[K, V1]

  /** Removes a key from this map, returning a new map.
   *  @param    key the key to be removed
   *  @return   a new map without a binding for `key`
   *
   *  @usecase  def - (key: K): Map[K, V]
   *    @inheritdoc
   */
  def - (key: K): This

  /** Tests whether the map is empty.
   *
   *  @return `true` if the map does not contain any key/value binding, `false` otherwise.
   */
  override def isEmpty: Boolean = size == 0

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
  def isDefinedAt(key: K) = contains(key)

  override /*PartialFunction*/
  def applyOrElse[K1 <: K, V1 >: V](x: K1, default: K1 => V1): V1 =
    getOrElse(x, default(x))

  /** Collects all keys of this map in a set.
   * @return  a set containing all keys of this map.
   */
  def keySet: Set[K] = new DefaultKeySet

  /** The implementation class of the set returned by `keySet`.
   */
  protected class DefaultKeySet extends AbstractSet[K] with Set[K] with Serializable {
    def contains(key : K) = self.contains(key)
    def iterator = keysIterator
    def + (elem: K): Set[K] = (Set[K]() ++ this + elem).asInstanceOf[Set[K]] // !!! concrete overrides abstract problem
    def - (elem: K): Set[K] = (Set[K]() ++ this - elem).asInstanceOf[Set[K]] // !!! concrete overrides abstract problem
    override def size = self.size
    override def foreach[U](f: K => U) = self.keysIterator foreach f
  }

  /** Creates an iterator for all keys.
   *
   *  @return an iterator over all keys.
   */
  def keysIterator: Iterator[K] = new AbstractIterator[K] {
    val iter = self.iterator
    def hasNext = iter.hasNext
    def next() = iter.next()._1
  }

  /** Collects all keys of this map in an iterable collection.
   *
   *  @return the keys of this map as an iterable.
   */
  @migration("`keys` returns `Iterable[K]` rather than `Iterator[K]`.", "2.8.0")
  def keys: Iterable[K] = keySet

  /** Collects all values of this map in an iterable collection.
   *
   *  @return the values of this map as an iterable.
   */
  @migration("`values` returns `Iterable[V]` rather than `Iterator[V]`.", "2.8.0")
  def values: Iterable[V] = new DefaultValuesIterable

  /** The implementation class of the iterable returned by `values`.
   */
  protected class DefaultValuesIterable extends AbstractIterable[V] with Iterable[V] with Serializable {
    def iterator = valuesIterator
    override def size = self.size
    override def foreach[U](f: V => U) = self.valuesIterator foreach f
  }

  /** Creates an iterator for all values in this map.
   *
   *  @return an iterator over all values that are associated with some key in this map.
   */
  def valuesIterator: Iterator[V] = new AbstractIterator[V] {
    val iter = self.iterator
    def hasNext = iter.hasNext
    def next() = iter.next()._2
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

  protected class FilteredKeys(p: K => Boolean) extends AbstractMap[K, V] with DefaultMap[K, V] {
    override def foreach[U](f: ((K, V)) => U): Unit = for (kv <- self) if (p(kv._1)) f(kv)
    def iterator = self.iterator.filter(kv => p(kv._1))
    override def contains(key: K) = p(key) && self.contains(key)
    def get(key: K) = if (!p(key)) None else self.get(key)
  }

  /** Filters this map by retaining only keys satisfying a predicate.
   *
   *  '''Note''': the predicate must accept any key of type `K`, not just those already
   *  present in the map, as the predicate is tested before the underlying map is queried.
   *
   *  @param  p   the predicate used to test keys
   *  @return an immutable map consisting only of those key value pairs of this map where the key satisfies
   *          the predicate `p`. The resulting map wraps the original map without copying any elements.
   */
  def filterKeys(p: K => Boolean): Map[K, V] = new FilteredKeys(p)

  protected class MappedValues[W](f: V => W) extends AbstractMap[K, W] with DefaultMap[K, W] {
    override def foreach[U](g: ((K, W)) => U): Unit = for ((k, v) <- self) g((k, f(v)))
    def iterator = for ((k, v) <- self.iterator) yield (k, f(v))
    override def size = self.size
    override def contains(key: K) = self.contains(key)
    def get(key: K) = self.get(key).map(f)
  }

  /** Transforms this map by applying a function to every retrieved value.
   *  @param  f   the function used to transform values of this map.
   *  @return a map view which maps every key of this map
   *          to `f(this(key))`. The resulting map wraps the original map without copying any elements.
   */
  def mapValues[W](f: V => W): Map[K, W] = new MappedValues(f)

  // The following 5 operations (updated, two times +, two times ++) should really be
  // generic, returning This[V]. We need better covariance support to express that though.
  // So right now we do the brute force approach of code duplication.

  /** Creates a new map obtained by updating this map with a given key/value pair.
   *  @param    key the key
   *  @param    value the value
   *  @tparam   V1 the type of the added value
   *  @return   A new map with the new key/value mapping added to this map.
   *
   *  @usecase  def updated(key: K, value: V): Map[K, V]
   *    @inheritdoc
   */
  def updated [V1 >: V](key: K, value: V1): Map[K, V1] = this + ((key, value))

  /** Adds key/value pairs to this map, returning a new map.
   *
   *  This method takes two or more key/value pairs. Another overloaded
   *  variant of this method handles the case where a single key/value pair is
   *  added.
   *  @param    kv1 the first key/value pair
   *  @param    kv2 the second key/value pair
   *  @param    kvs the remaining key/value pairs
   *  @tparam   V1  the type of the added values
   *  @return   a new map with the given bindings added to this map
   *
   *  @usecase  def + (kvs: (K, V)*): Map[K, V]
   *    @inheritdoc
   *    @param    kvs the key/value pairs
   */
  def + [V1 >: V] (kv1: (K, V1), kv2: (K, V1), kvs: (K, V1) *): Map[K, V1] =
    this + kv1 + kv2 ++ kvs

  /** Adds all key/value pairs in a traversable collection to this map, returning a new map.
   *
   *  @param    xs  the collection containing the added key/value pairs
   *  @tparam   V1  the type of the added values
   *  @return   a new map with the given bindings added to this map
   *
   *  @usecase  def ++ (xs: Traversable[(K, V)]): Map[K, V]
   *    @inheritdoc
   */
  def ++[V1 >: V](xs: GenTraversableOnce[(K, V1)]): Map[K, V1] =
    ((repr: Map[K, V1]) /: xs.seq) (_ + _)

  /** Returns a new map obtained by removing all key/value pairs for which the predicate
   *  `p` returns `true`.
   *
   *  '''Note:'''    This method works by successively removing elements for which the
   *           predicate is true from this set.
   *           If removal is slow, or you expect that most elements of the set
   *           will be removed, you might consider using `filter`
   *           with a negated predicate instead.
   *  @param p    A predicate over key-value pairs
   *  @return     A new map containing elements not satisfying the predicate.
   */
  override def filterNot(p: ((K, V)) => Boolean): This = {
    var res: This = repr
    for (kv <- this)
      if (p(kv)) res = (res - kv._1).asInstanceOf[This] // !!! concrete overrides abstract problem
    res
  }

  override def toSeq: Seq[(K, V)] = {
    if (isEmpty) Vector.empty[(K, V)]
    else {
      // Default appropriate for immutable collections; mutable collections override this
      val vb = Vector.newBuilder[(K, V)]
      foreach(vb += _)
      vb.result
    }
  }

  override def toBuffer[E >: (K, V)]: mutable.Buffer[E] = {
    val result = new mutable.ArrayBuffer[E](size)
    // Faster to let the map iterate itself than to defer through copyToBuffer
    foreach(result += _)
    result
  }

  protected[this] override def parCombiner = ParMap.newCombiner[K, V]

  /** Appends all bindings of this map to a string builder using start, end, and separator strings.
   *  The written text begins with the string `start` and ends with the string
   *  `end`. Inside, the string representations of all bindings of this map
   *  in the form of `key -> value` are separated by the string `sep`.
   *
   *  @param b     the builder to which strings are appended.
   *  @param start the starting string.
   *  @param sep   the separator string.
   *  @param end   the ending string.
   *  @return      the string builder `b` to which elements were appended.
   */
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
    this.iterator.map { case (k, v) => k+" -> "+v }.addString(b, start, sep, end)

  /** Defines the prefix of this object's `toString` representation.
   *  @return  a string representation which starts the result of `toString` applied to this $coll.
   *           Unless overridden in subclasses, the string prefix of every map is `"Map"`.
   */
  override def stringPrefix: String = "Map"

  override /*PartialFunction*/
  def toString = super[IterableLike].toString

}
