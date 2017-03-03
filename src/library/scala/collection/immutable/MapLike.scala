/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package immutable

import generic._
import parallel.immutable.ParMap

/**
 *  A generic template for immutable maps from keys of type `K`
 *  to values of type `V`.
 *  To implement a concrete map, you need to provide implementations of the
 *  following methods (where `This` is the type of the actual map implementation):
 *
 *  {{{
 *    def get(key: K): Option[V]
 *    def iterator: Iterator[(K, V)]
 *    def + [V1 >: V](kv: (K, V)): Map[K, V1]
 *    def - (key: K): This
 *  }}}
 *
 *  If you wish that transformer methods like `take`, `drop`, `filter` return the
 *  same kind of map, you should also override:
 *
 *  {{{
 *    def empty: This
 *  }}}
 *
 *  It is also good idea to override methods `foreach` and
 *  `size` for efficiency.
 *
 *  @tparam K     the type of the keys contained in this collection.
 *  @tparam V     the type of the values associated with the keys.
 *  @tparam This  The type of the actual map implementation.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @define Coll immutable.Map
 *  @define coll immutable map
 */
trait MapLike[K, +V, +This <: MapLike[K, V, This] with Map[K, V]]
  extends scala.collection.MapLike[K, V, This]
     with Parallelizable[(K, V), ParMap[K, V]]
{
self =>

  protected[this] override def parCombiner = ParMap.newCombiner[K, V]

  /** A new immutable map containing updating this map with a given key/value mapping.
   *  @param    key the key
   *  @param    value the value
   *  @return   A new map with the new key/value mapping
   */
  override def updated [V1 >: V](key: K, value: V1): immutable.Map[K, V1] = this + ((key, value))

  /** Add a key/value pair to this map, returning a new map.
   *  @param    kv the key/value pair.
   *  @return   A new map with the new binding added to this map.
   */
  def + [V1 >: V] (kv: (K, V1)): immutable.Map[K, V1]

  /** Adds two or more elements to this collection and returns
   *  a new collection.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   *  @return A new map with the new bindings added to this map.
   */
  override def + [V1 >: V] (elem1: (K, V1), elem2: (K, V1), elems: (K, V1) *): immutable.Map[K, V1] =
    this + elem1 + elem2 ++ elems

  /** Adds a number of elements provided by a traversable object
   *  and returns a new collection with the added elements.
   *
   *  @param xs      the traversable object consisting of key-value pairs.
   *  @return        a new immutable map with the bindings of this map and those from `xs`.
   */
  override def ++[V1 >: V](xs: GenTraversableOnce[(K, V1)]): immutable.Map[K, V1] =
    ((repr: immutable.Map[K, V1]) /: xs.seq) (_ + _)

  /** Filters this map by retaining only keys satisfying a predicate.
   *  @param  p   the predicate used to test keys
   *  @return an immutable map consisting only of those key value pairs of this map where the key satisfies
   *          the predicate `p`. The resulting map wraps the original map without copying any elements.
   */
  override def filterKeys(p: K => Boolean): Map[K, V] = new FilteredKeys(p) with DefaultMap[K, V]

  /** Transforms this map by applying a function to every retrieved value.
   *  @param  f   the function used to transform values of this map.
   *  @return a map view which maps every key of this map
   *          to `f(this(key))`. The resulting map wraps the original map without copying any elements.
   */
  override def mapValues[W](f: V => W): Map[K, W] = new MappedValues(f) with DefaultMap[K, W]

  /** Collects all keys of this map in a set.
   *  @return  a set containing all keys of this map.
   */
  override def keySet: immutable.Set[K] = new ImmutableDefaultKeySet

  protected class ImmutableDefaultKeySet extends super.DefaultKeySet with immutable.Set[K] {
    override def + (elem: K): immutable.Set[K] =
      if (this(elem)) this
      else immutable.Set[K]() ++ this + elem
    override def - (elem: K): immutable.Set[K] =
      if (this(elem)) immutable.Set[K]() ++ this - elem
      else this
      
    // ImmutableDefaultKeySet is only protected, so we won't warn on override.
    // Someone could override in a way that makes widening not okay
    // (e.g. by overriding +, though the version in this class is fine)
    override def toSet[B >: K]: Set[B] = this.asInstanceOf[Set[B]]
  }

  /** This function transforms all the values of mappings contained
   *  in this map with function `f`.
   *
   *  @param f A function over keys and values
   *  @return  the updated map
   */
  def transform[W, That](f: (K, V) => W)(implicit bf: CanBuildFrom[This, (K, W), That]): That = {
    val b = bf(repr)
    for ((key, value) <- this) b += ((key, f(key, value)))
    b.result()
  }
}
