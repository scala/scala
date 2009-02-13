/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Map.scala 16884 2009-01-09 16:52:09Z cunei $


package scalax.collection.generic

import collection.immutable.Set
import collection.mutable.ArrayBuffer
import annotation.unchecked.uncheckedVariance

/** <p>
*     A map is a collection that maps each key to one or zero values.
 *  </p>
 *  <p>
 *    This trait provides a limited interface, only allowing reading of elements.
 *    There are two extensions of this trait, in packages
 *    <code><a href="mutable$content.html" target="contentFrame">
 *    scala.collection.mutable</a></code>
 *    and <code><a href="immutable$content.html" target="contentFrame">
 *    scala.collection.immutable</a></code>, which provide functionality for
 *    adding new key/value mappings to a map. The trait in the first package is
 *    for maps that are modified destructively, whereas the trait in
 *    the second package is for immutable maps which create a new map
 *    when something is added or removed from them.
 *  </p>
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 1.2, 31/12/2006
 */
trait MapTemplate[A, B, +CC[A1, B1] <: Map[A1, B1] with MapTemplate[A1, B1, CC]]
  extends PartialFunction[A, B]
     with SizedIterable[(A, B)]
     with Addable[CC[A, B], (A, B)]
     with Subtractable[CC[A, B], A] {
self: CC[A, B] =>

  def newBuilder[B]: Builder[SizedIterable, B] = new ArrayBuffer[B]

  override def thisCC: CC[A, B] = this

  /** This method returns a new map instance of the same class
   *  mapping keys of the same type to values of type <code>C</code>.
   */
  def empty[C]: CC[A, C]

  /** Compute the number of key-to-value mappings.
   *
   *  @return the number of mappings
   */
  def size: Int

  /** Check if this map maps <code>key</code> to a value and return the
   *  value if it exists.
   *
   *  @param  key the key of the mapping of interest
   *  @return     the value of the mapping, if it exists
   */
  def get(key: A): Option[B]

  /** Check if this map maps <code>key</code> to a value.
    *  Return that value if it exists, otherwise return <code>default</code>.
    */
  def getOrElse[B2 >: B](key: A, default: => B2): B2 =
    get(key) match {
      case Some(v) => v
      case None => default
    }

  /** Is this an empty map?
   *
   *  @return <code>true</code> iff the map is empty.
   */
  override def isEmpty: Boolean = size == 0

  /** Retrieve the value which is associated with the given key. This
   *  method throws an exception if there is no mapping from the given
   *  key to a value.
   *
   *  @param  key the key
   *  @return     the value associated with the given key.
   */
  def apply(key: A): B = get(key) match {
    case None => default(key)
    case Some(value) => value
  }

  /** Is the given key mapped to a value by this map?
   *
   *  @param key the key
   *  @return    <code>true</code> iff there is a mapping for key in this map
   */
  def contains(key: A): Boolean = get(key) match {
    case None => false
    case Some(_) => true
  }

  /** Does this map contain a mapping from the given key to a value?
   *
   *  @param key the key
   *  @return    <code>true</code> iff there is a mapping for key in this map
   */
  def isDefinedAt(key: A) = contains(key)

  /** Creates an iterator for all keys.
   *
   *  @return an iterator over all keys.
   */
  def keys: Iterator[A] = new Iterator[A] {
    val iter = self.elements
    def hasNext = iter.hasNext
    def next = iter.next._1
  }

  /** @return the keys of this map as a set.
   */
  def keySet: Set[A] = new Set[A] {
    def size = self.size
    def contains(key : A) = self.contains(key)
    def elements = self.elements.map(_._1)
    def + (elem: A): Set[A] = immutable.Set[A]() ++ this + elem
    def - (elem: A): Set[A] = immutable.Set[A]() ++ this - elem
    override def newBuilder[B]: Builder[Set, B] = Set.newBuilder[B]
  }

  /** Creates an iterator for a contained values.
   *
   *  @return an iterator over all values.
   */
  def values: Iterator[B] = new Iterator[B] {
    val iter = self.elements
    def hasNext = iter.hasNext
    def next = iter.next._2
  }

  /** Creates a string representation for this map.
   *
   *  @return    a string showing all mappings
   */
  override def toString() =
    elements.toList.map(kv => kv._1 + " -> " + kv._2).mkString(stringPrefix + "(", ", ", ")")

  /** The default value for the map, returned when a key is not found
   *  The method implemented here yields an error,
   *  but it might be overridden in subclasses.
   *
   *  @param key the given key value
   *  @throws Predef.NoSuchElementException
   */
  def default(key: A): B =
    throw new NoSuchElementException("key not found: " + key)

/*
  override def view: Map.View[A,B] = new Map.View[A, B] {
    override def elements = self.elements
    override def size = self.size
    override def get(key: A): Option[B] = self.get(key)
  }
*/
  def filterKeys(p: A => Boolean) = new MapView[CC, A, B] {
    val origin = self
    override def foreach(f: ((A, B)) => Unit): Unit = for (kv <- self) if (p(kv._1)) f(kv)
    def elements = self.elements.filter(kv => p(kv._1))
    def size = { var sz = 0; foreach(_ => sz += 1); sz }
    override def contains(key: A) = self.contains(key) && p(key)
    def get(key: A) = if (!p(key)) None else self.get(key)
  }

  def mapElements[C](f: B => C) = new MapView[CC, A,C] {
    val origin = self
    override def foreach(g: ((A, C)) => Unit): Unit = for ((k, v) <- self) g((k, f(v)))
    def elements = for ((k, v) <- self.elements) yield (k, f(v))
    def size = self.size
    override def contains(key: A) = self.contains(key)
    def get(key: A) = self.get(key).map(f)
  }

  /** Defines the prefix of this object's <code>toString</code> representation.
   */
  override def stringPrefix: String = "Map"

  /** Add a key/value pair to this map.
   *  @param    key the key
   *  @param    value the value
   *  @return   A new map with the new binding added to this map
   */
  def update (key: A, value: B): CC[A, B]

  /** Add a key/value pair to this map.
   *  @param    kv the key/value pair.
   *  @return   A new map with the new binding added to this map
   */
  def + (kv: (A, B)): CC[A, B] = update(kv._1, kv._2)

  /** Remove a key from this map
   *  @param    key the key to be removed
   *  @return   If the map does not contain a binding for <code>key</code>
   *            it is returned unchanged. Otherwise, return a new map
   *            without a binding for <code>key</code>
   */
  def - (key: A): CC[A, B]

  /** This function transforms all the values of mappings contained
   *  in this map with function <code>f</code>.
   *
   *  @param f A function over keys and values
   *  @return  the updated map
   */
  def transform[C](f: (A, B) => C): CC[A, C] = {
    var res = empty[C]
    for ((key, value) <- this) res += ((key, f(key, value)))
    res
  }

  /** Builds a new map with all key/value pairs of this map
   *  for which the predicate <code>p</code> returns <code>true</code>.
   *
   *  @param p A predicate over key-value pairs
   *  @return  the updated map
   */
  override def filter(p: ((A, B)) => Boolean): CC[A, B] = {
    var res = empty[B]
    for (kv <- this)
      if (p(kv)) res += kv
    res
  }

  /** Removes all the mappings for which the predicate
   *  <code>p</code> returns <code>false</code>.
   *
   *  @param p A predicate over key-value pairs
   *  @return  the updated map
   */
  override def remove(p: ((A, B)) => Boolean): CC[A, B] = {
    var res = this
    for (kv <- this)
      if (!p(kv)) res -= kv._1
    res
  }
}
