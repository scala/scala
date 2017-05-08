/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package collection
package mutable

import generic._
import scala.annotation.migration
import scala.collection.parallel.mutable.ParMap

/** A template trait for mutable maps.
 *  $mapNote
 *  $mapTags
 *  @define Coll `mutable.Map`
 *  @define coll mutable map
 *  @since   2.8
 *
 * @define mapNote
 *    '''Implementation note:'''
 *    This trait provides most of the operations of a mutable `Map`
 *    independently of its representation. It is typically inherited by
 *    concrete implementations of maps.
 *
 *    To implement a concrete mutable map, you need to provide
 *    implementations of the following methods:
 *    {{{
 *       def get(key: K): Option[V]
 *       def iterator: Iterator[(K, V)]
 *       def += (kv: (K, V)): This
 *       def -= (key: K): This
 *    }}}
 *    If you wish that methods like `take`, `drop`, `filter` also return the same kind of map
 *    you should also override:
 *    {{{
 *       def empty: This
 *    }}}
 *    It is also good idea to override methods `foreach` and
 *    `size` for efficiency.
 */
trait MapLike[K, V, +This <: MapLike[K, V, This] with Map[K, V]]
  extends scala.collection.MapLike[K, V, This]
     with Builder[(K, V), This]
     with Growable[(K, V)]
     with Shrinkable[K]
     with Cloneable[This]
     with Parallelizable[(K, V), ParMap[K, V]]
{ self =>

  /** A common implementation of `newBuilder` for all mutable maps
   *    in terms of `empty`.
   *
   *    Overrides `MapLike` implementation for better efficiency.
   */
  override protected[this] def newBuilder: Builder[(K, V), This] = empty

  protected[this] override def parCombiner = ParMap.newCombiner[K, V]
  
  /** Converts this $coll to a sequence.
    *
    * ```Note```: assumes a fast `size` method.  Subclasses should override if this is not true.
    */
  override def toSeq: collection.Seq[(K, V)] = {
    // ArrayBuffer for efficiency, preallocated to the right size.
    val result = new ArrayBuffer[(K, V)](size)
    foreach(result += _)
    result
  }


  /** Adds a new key/value pair to this map and optionally returns previously bound value.
   *  If the map already contains a
   *  mapping for the key, it will be overridden by the new value.
   *
   * @param key    the key to update
   * @param value  the new value
   * @return an option value containing the value associated with the key
   *         before the `put` operation was executed, or `None` if `key`
   *         was not defined in the map before.
   */
  def put(key: K, value: V): Option[V] = {
    val r = get(key)
    update(key, value)
    r
  }

  /** Adds a new key/value pair to this map.
   *  If the map already contains a
   *  mapping for the key, it will be overridden by the new value.
   *
   *  @param key    The key to update
   *  @param value  The new value
   */
  def update(key: K, value: V) { this += ((key, value)) }

  /** Adds a new key/value pair to this map.
   *  If the map already contains a
   *  mapping for the key, it will be overridden by the new value.
   *  @param    kv the key/value pair.
   *  @return   the map itself
   */
  def += (kv: (K, V)): this.type

  /** Creates a new map consisting of all key/value pairs of the current map
   *  plus a new pair of a given key and value.
   *
   *  @param key    The key to add
   *  @param value  The new value
   *  @return       A fresh immutable map with the binding from `key` to
   *                `value` added to this map.
   */
  override def updated[V1 >: V](key: K, value: V1): Map[K, V1] = this + ((key, value))

  /** Creates a new map containing a new key/value mapping and all the key/value mappings
   *  of this map.
   *
   *  Mapping `kv` will override existing mappings from this map with the same key.
   *
   *  @param kv    the key/value mapping to be added
   *  @return      a new map containing mappings of this map and the mapping `kv`.
   */
  @migration("`+` creates a new map. Use `+=` to add an element to this map and return that map itself.", "2.8.0")
  def + [V1 >: V] (kv: (K, V1)): Map[K, V1] = clone().asInstanceOf[Map[K, V1]] += kv

  /** Creates a new map containing two or more key/value mappings and all the key/value
   *  mappings of this map.
   *
   *  Specified mappings will override existing mappings from this map with the same keys.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   *  @return      a new map containing mappings of this map and two or more specified mappings.
   */
  @migration("`+` creates a new map. Use `+=` to add an element to this map and return that map itself.", "2.8.0")
  override def + [V1 >: V] (elem1: (K, V1), elem2: (K, V1), elems: (K, V1) *): Map[K, V1] =
    clone().asInstanceOf[Map[K, V1]] += elem1 += elem2 ++= elems

  /** Creates a new map containing the key/value mappings provided by the specified traversable object
   *  and all the key/value mappings of this map.
   *
   *  Note that existing mappings from this map with the same key as those in `xs` will be overridden.
   *
   *  @param xs     the traversable object.
   *  @return       a new map containing mappings of this map and those provided by `xs`.
   */
  @migration("`++` creates a new map. Use `++=` to add an element to this map and return that map itself.", "2.8.0")
  override def ++[V1 >: V](xs: GenTraversableOnce[(K, V1)]): Map[K, V1] =
    clone().asInstanceOf[Map[K, V1]] ++= xs.seq

  /** Removes a key from this map, returning the value associated previously
   *  with that key as an option.
   *  @param    key the key to be removed
   *  @return   an option value containing the value associated previously with `key`,
   *            or `None` if `key` was not defined in the map before.
   */
  def remove(key: K): Option[V] = {
    val r = get(key)
    this -= key
    r
  }

  /** Removes a key from this map.
   *  @param    key the key to be removed
   *  @return   the map itself.
   */
  def -= (key: K): this.type

  /** Creates a new map with all the key/value mappings of this map except the key/value mapping
   *  with the specified key.
   *
   *  @param    key the key to be removed
   *  @return   a new map with all the mappings of this map except that with a key `key`.
   */
  @migration("`-` creates a new map. Use `-=` to remove an element from this map and return that map itself.", "2.8.0")
  override def -(key: K): This = clone() -= key

  /** Removes all bindings from the map. After this operation has completed,
   *  the map will be empty.
   */
  def clear() { keysIterator foreach -= }

  /** If given key is already in this map, returns associated value.
   *
   *  Otherwise, computes value from given expression `op`, stores with key
   *  in map and returns that value.
   *
   *  Concurrent map implementations may evaluate the expression `op`
   *  multiple times, or may evaluate `op` without inserting the result.
   *  
   *  @param  key the key to test
   *  @param  op  the computation yielding the value to associate with `key`, if
   *              `key` is previously unbound.
   *  @return     the value associated with key (either previously or as a result
   *              of executing the method).
   */
  def getOrElseUpdate(key: K, op: => V): V =
    get(key) match {
      case Some(v) => v
      case None => val d = op; this(key) = d; d
    }

  /** Applies a transformation function to all values contained in this map.
   *  The transformation function produces new values from existing keys
   *  associated values.
   *
   * @param f  the transformation to apply
   * @return   the map itself.
   */
  def transform(f: (K, V) => V): this.type = {
    this.iterator foreach {
      case (key, value) => update(key, f(key, value))
    }
    this
  }

  /** Retains only those mappings for which the predicate
   *  `p` returns `true`.
   *
   * @param p  The test predicate
   */
  def retain(p: (K, V) => Boolean): this.type = {
    for ((k, v) <- this.toList) // scala/bug#7269 toList avoids ConcurrentModificationException
      if (!p(k, v)) this -= k

    this
  }

  override def clone(): This = empty ++= repr

  /** The result when this map is used as a builder
   *  @return  the map representation itself.
   */
  def result: This = repr

  /** Creates a new map with all the key/value mappings of this map except mappings with keys
   *  equal to any of the two or more specified keys.
   *
   *  @param elem1 the first element to remove.
   *  @param elem2 the second element to remove.
   *  @param elems the remaining elements to remove.
   *  @return      a new map containing all the mappings of this map except mappings
   *               with a key equal to `elem1`, `elem2` or any of `elems`.
   */
  @migration("`-` creates a new map. Use `-=` to remove an element from this map and return that map itself.", "2.8.0")
  override def -(elem1: K, elem2: K, elems: K*): This =
    clone() -= elem1 -= elem2 --= elems

  /** Creates a new map with all the key/value mappings of this map except mappings with keys
   *  equal to any of those provided by the specified traversable object.
   *
   *  @param xs       the traversable object.
   *  @return         a new map with all the key/value mappings of this map except mappings
   *                  with a key equal to a key from `xs`.
   */
  @migration("`--` creates a new map. Use `--=` to remove an element from this map and return that map itself.", "2.8.0")
  override def --(xs: GenTraversableOnce[K]): This = clone() --= xs.seq
}
