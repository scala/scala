/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import Predef._


/** <p>
 *    This class defines the interface of collections that unambiguously map
 *     keys to values (i.e. a key is mapped to at least one value).
 *  </p>
 *  <p>
 *    Class <code>Map</code> may only be used for accessing elements from map
 *    implementations. Two different extensions of class <code>Map</code> in
 *    the package <code><a href="mutable$content.html" target="contentFrame">
 *    scala.collection.mutable</a></code>
 *    and  <code><a href="immutable$content.html" target="contentFrame">
 *    scala.collection.immutable</a></code> provide functionality for
 *    adding new key/value mappings to a map. The class in the first package is
 *    implemented by maps that are modified destructively, whereas the class in
 *    the second package is used by functional map implementations that rely on
 *    immutable data structures.
 *  </p>
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 1.2, 31/12/2006
 */
trait Map[A, +B] extends PartialFunction[A, B] with Iterable[(A, B)] {

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

  /** Is this an empty map?
   *
   *  @return <code>true</code> iff the map is empty.
   */
  def isEmpty: Boolean = size == 0

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
    val iter = Map.this.elements
    def hasNext = iter.hasNext
    def next = iter.next._1
  }

  /** @return the keys of this map as a set.
   */
  def keySet: Set[A] = new Set[A] {
    def size = Map.this.size
    def contains(key : A) = Map.this.contains(key)
    def elements = Map.this.elements.map(._1)
  }

  /** Creates an iterator for a contained values.
   *
   *  @return an iterator over all values.
   */
  def values: Iterator[B] = new Iterator[B] {
    val iter = Map.this.elements
    def hasNext = iter.hasNext
    def next = iter.next._2
  }

  /** Compares two maps structurally; i.e. checks if all mappings
   *  contained in this map are also contained in the other map,
   *  and vice versa.
   *
   *  @param that the other map
   *  @return     <code>true</code> iff both maps contain exactly the
   *              same mappings.
   */
  override def equals(that: Any): Boolean = that match {
    case other: Map[a, b] =>
      this.size == other.size && this.elements.forall {
        case (key, value) => other.get(key.asInstanceOf[a]) match {
          case None => false
          case Some(otherval) => value == otherval
        }
      }
    case _ => false
  }

  /** A hash method compatible with <code>equals</code>
   */
  override def hashCode() =
    (0 /: elements) ((hash, kv) => hash + kv.hashCode)

  /** Creates a string representation for this map.
   *
   *  @return    a string showing all mappings
   */
  override def toString() =
    elements.toList.map(kv => kv._1 + " -> " + kv._2).mkString("Map(", ", ", ")")

  /** The default value for the map, returned when a key is not found
   *  The method implemented here yields an error,
   *  but it might be overridden in subclasses.
   *
   *  @param key the given key value
   *  @throws Predef.NoSuchElementException
   */
  def default(key: A): B =
    throw new NoSuchElementException("key not found: " + key)

}
