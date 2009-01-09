/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import Predef._

object Map {
  trait Projection[A, +B] extends Iterable.Projection[(A, B)] with Map[A, B]
}


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
trait Map[A, +B] extends PartialFunction[A, B] with Collection[(A, B)] {

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
    val iter = Map.this.elements
    def hasNext = iter.hasNext
    def next = iter.next._1
  }

  /** @return the keys of this map as a set.
   */
  def keySet: Set[A] = new Set[A] {
    def size = Map.this.size
    def contains(key : A) = Map.this.contains(key)
    def elements = Map.this.elements.map(_._1)
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

  override def projection: Map.Projection[A,B] = new Map.Projection[A, B] {
    override def elements = Map.this.elements
    override def size = Map.this.size
    override def get(key: A): Option[B] = Map.this.get(key)
  }
  /** non-strict filter based on keys only */
  def filterKeys(p: A => Boolean): Map.Projection[A, B] = new Map.Projection[A, B] {
    def elements = Map.this.elements.filter(x => p(x._1))
    def size = {
      var sz = 0
      Map.this.foreach(x => if (p(x._1)) sz = sz + 1)
      sz
    }
    override def contains(key: A) = Map.this.contains(key) && p(key)
    override def get(key: A) = if (!p(key)) None else Map.this.get(key)
  }
  /** non-strict map elements using existing key set */
  def mapElements[C](f: B => C) : Map.Projection[A,C] = new Map.Projection[A,C] {
    def elements = Map.this.elements.map(e => (e._1, f(e._2)))
    def size = Map.this.size
    override def contains(key: A) = Map.this.contains(key)
    override def get(key: A) = Map.this.get(key).map(f)
  }

  /** Defines the prefix of this object's <code>toString</code> representation.
   */
  override protected def stringPrefix: String = "Map"
}
