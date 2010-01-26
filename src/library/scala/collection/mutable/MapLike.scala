/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._

/** A template trait for mutable maps of type `mutable.Map[A, B]` which
 *  associate keys of type `A` with values of type `B`.
 *
 *  @tparam A    the type of the keys.
 *  @tparam B    the type of associated values.
 *  @tparam This the type of the `Map` itself.
 *
 * $mapnote
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since 2.8
 *  @define mapnote
 *    To implement a concrete mutable map, you need to provide implementations
 *    of the following methods:
 *  {{{
 *    def get(key: A): Option[B]
 *    def iterator: Iterator[(A, B)]
 *    def += (kv: (A, B)): this.type
 *    def -= (key: A): this.type
 *  }}}
 *    If you wish that methods like `take`,
 *    `drop`, `filter` return the same kind of map, you
 *    should also override:
 *  {{{
 *    def> empty: This
 *  }}}
 *    If you wish to avoid the unncessary construction of an `Option`
 *    object, you could also override `apply`, `update`,
 *    and `delete`.

 *    It is also good idea to override methods `foreach` and
 *    `size` for efficiency.
 *  @define coll mutable map
 *  @define Coll mutable.Map
 */
trait MapLike[A, B, +This <: MapLike[A, B, This] with Map[A, B]]
  extends MapLikeBase[A, B, This]
     with Builder[(A, B), This]
     with Growable[(A, B)]
     with Shrinkable[A]
     with Cloneable[This]
{ self =>

  import scala.collection.Traversable

  /** A common implementation of `newBuilder` for all mutable maps
   *    in terms of `empty`.
   *
   *    Overrides `MapLike` implementation for better efficiency.
   */
  override protected[this] def newBuilder: Builder[(A, B), This] = empty

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
  def put(key: A, value: B): Option[B] = {
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
  def update(key: A, value: B) { this += ((key, value)) }

  /** Adds a new key/value pair to this map.
   *  If the map already contains a
   *  mapping for the key, it will be overridden by the new value.
   *  @param    kv the key/value pair.
   *  @return   the map itself
   */
  def += (kv: (A, B)): this.type

  /** Creates a new map consisting of all key/value pairs of the current map
   *  plus a new pair of a guven key and value.
   *
   *  @param key    The key to add
   *  @param value  The new value
   *  @return       A fresh immutable map with the binding from `key` to
   *                `value` added to this map.
   */
  override def updated[B1 >: B](key: A, value: B1): mutable.Map[A, B1] = this + ((key, value))

  /** Add a new key/value mapping and return the map itself.
   *
   *  @param kv    the key/value mapping to be added
   */
  @deprecated("This operation will create a new map in the future. To add an element as a side\n"+
              "effect to an existing map and return that map itself, use +=. If you do want\n"+
              "to create a fresh map, you can use `clone() +=' to avoid a @deprecated warning.")
 def + (kv: (A, B)): this.type = { update(kv._1, kv._2); this }

  /** Adds two or more key/value mappings and return the map itself.
   *  with the added elements.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  @deprecated("This operation will create a new map in the future. To add an element as a side\n"+
              "effect to an existing map and return that map itself, use +=. If you do want to\n"+
              "create a fresh map, you can use `clone() +=` to avoid a @deprecated warning.")
  def +(elem1: (A, B), elem2: (A, B), elems: (A, B)*): this.type =
    this += elem1 += elem2 ++= elems

  /** Adds a number of elements provided by a traversable object
   *  via its `iterator` method and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the added elements.
   *
   *  @param iter     the traversable object.
   */
  @deprecated("This operation will create a new map in the future. To add elements as a side\n"+
              "effect to an existing map and return that map itself, use ++=. If you do want\n"+
              "to create a fresh map, you can use `clone() ++=` to avoid a @deprecated warning.")
  def ++(iter: Traversable[(A, B)]): this.type = { for (elem <- iter) +=(elem); this }

  /** Adds a number of elements provided by an iterator
   *  via its `iterator` method and returns
   *  the collection itself.
   *
   *  @param iter   the iterator
   */
  @deprecated("This operation will create a new map in the future. To add elements as a side\n"+
              "effect to an existing map and return that map itself, use ++=. If you do want\n"+
              "to create a fresh map, you can use `clone() +=` to avoid a @deprecated warning.")
  def ++(iter: Iterator[(A, B)]): this.type = { for (elem <- iter) +=(elem); this }

  /** Removes a key from this map, returning the value associated previously
   *  with that key as an option.
   *  @param    key the key to be removed
   *  @return   an option value containing the value associated previously with `key`,
   *            or `None` if `key` was not defined in the map before.
   */
  def remove(key: A): Option[B] = {
    val r = get(key)
    this -= key
    r
  }

  /** Removes a key from this map.
   *  @param    key the key to be removed
   *  @return   the map itself.
   */
  def -= (key: A): this.type

  /** Delete a key from this map if it is present and return the map itself.
   *  @param    key the key to be removed
   */
  @deprecated("This operation will create a new map in the future. To add elements as a side\n"+
              "effect to an existing map and return that map itself, use -=. If you do want\n"+
              "to create a fresh map, you can use `clone() -=` to avoid a @deprecated warning.")
  override def -(key: A): This = { -=(key); repr }

  /** If given key is defined in this map, remove it and return associated value as an Option.
   *  If key is not present return None.
   *  @param    key the key to be removed
   */
   @deprecated("Use `remove' instead") def removeKey(key: A): Option[B] = remove(key)

  /** Removes all bindings from the map. After this operation has completed,
   *  the map will be empty.
   */
  def clear() { for ((k, v) <- this.iterator) -=(k) }

  /** If given key is already in this map, returns associated value
   *  Otherwise, computes value from given expression `op`, stores with key
   *  in map and returns that value.
   *  @param  key the key to test
   *  @param  op  the computation yielding the value to associate with `key`, if
   *              `key` is previosuly unbound.
   *  @return     the value associated with key (either previously or as a result
   *              of executing the method).
   */
  def getOrElseUpdate(key: A, op: => B): B =
    get(key) match {
      case Some(v) => v
      case None => val d = op; this(key) = d; d
    }

  /** Applies a transformation function to all values contained in this map.
   *  The transformation function produces new values from existing keys
   *  asssociated values.
   *
   * @param f  the transformation to apply
   * @return   the map itself.
   */
  def transform(f: (A, B) => B): this.type = {
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
  @deprecated("cannot be type inferred because of retain in Iterable.")
  def retain(p: (A, B) => Boolean): this.type = {
    for ((k, v) <- this) if (!p(k, v)) -=(k)
    this
  }

  override def clone(): This =
    empty ++= repr

  /** The result when this map is used as a builder
   *  @return  the map representation itself.
   */
  def result: This = repr

  /** Removes two or more elements from this collection and returns
   *  the collection itself.
   *
   *  @param elem1 the first element to remove.
   *  @param elem2 the second element to remove.
   *  @param elems the remaining elements to remove.
   */
  @deprecated("Use -= instead if you intend to remove by side effect from an existing collection.\n"+
              "Use `clone() -=' if you intend to create a new collection.")
  override def -(elem1: A, elem2: A, elems: A*): This = {
    this -= elem1 -= elem2 --= elems
    repr
  }

  /** Removes a number of elements provided by a Traversable object and returns
   *  the collection itself.
   *
   *  @param iter     the Traversable object.
   */
  @deprecated("Use --= instead if you intend to remove by side effect from an existing collection.\n"+
              "Use `clone() --=' if you intend to create a new collection.")
  override def --(iter: Traversable[A]): This = {
    for (elem <- iter) -=(elem)
    repr
  }


  /** Removes a number of elements provided by an iterator and returns
   *  the collection itself.
   *
   *  @param iter   the iterator
   */
  @deprecated("Use --= instead if you intend to remove by side effect from an existing collection.\n"+
              "Use `clone() --=' if you intend to create a new collection.")
  override def --(iter: Iterator[A]): This = {
    for (elem <- iter) -=(elem)
    repr
  }
}
