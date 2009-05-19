/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Map.scala 16884 2009-01-09 16:52:09Z cunei $


package scala.collection.generic

/** A generic template for maps from keys of type A to values of type B.
 *  To implement a concrete map, you need to provide implementations of the following methods:
 *  (where `This` is the type of the map in question):
 *
 *   def get(key: A): Option[B]
 *   def elements: Iterator[(A, B)]
 *   def + [B1 >: B](kv: (A, B1)): This
 *   def -(key: A): This
 *
 * If you wish that methods like, take, drop, filter return the same kind of map, you should also
 * override:
 *
 *   def empty: This
 *
 * It is also good idea to override methods foreach and size for efficiency.
 */
trait MapTemplate[A, +B, +This <: MapTemplate[A, B, This] with Map[A, B]]
  extends PartialFunction[A, B]
     with IterableTemplate[(A, B), This]
     with Subtractable[A, This] {
self =>

  /* The empty map of the dame type as this map */
  def empty: This

  /** A common implementation of `newBuilder` for all maps in terms of `empty`.
   *  Overridden for mutable maps in `MutableMapTemplate`.
   */
  override protected[this] def newBuilder: Builder[(A, B), This] = new MapBuilder[A, B, This](empty)

  /** Check if this map maps <code>key</code> to a value and return the
   *  value as an option if it exists, None if not.
   *
   *  @param      key the key of the mapping of interest.
   *  @return     the value of the mapping as an option, if it exists, or None.
   */
  def get(key: A): Option[B]

  /** An iterator yielding all key/value mappings of this map. */
  def elements: Iterator[(A, B)]

  /** Add a key/value pair to this map, returning a new map.
   *  @param    kv the key/value pair
   *  @return   A new map with the new binding added to this map
   */
  def + [B1 >: B] (kv: (A, B1)): Map[A, B1]

  /** Removes a key from this map, returning a new map
   *  @param    key the key to be removed
   *  @return   A new map without a binding for <code>key</code>
   */
  def - (key: A): This

  /** Is this an empty map?
   *
   *  @return <code>true</code> iff the map does not contain any key/value mapping.
   */
  override def isEmpty: Boolean = size == 0

  /**  Check if this map maps <code>key</code> to a value.
   *   Return that value if it exists, otherwise return <code>default</code>.
   *   @param   key      the key.
   *   @param   default  a computation that yields a default value in case no binding for the key is
   *                     found in the map.
   */
  def getOrElse[B1 >: B](key: A, default: => B1): B1 = get(key) match {
    case Some(v) => v
    case None => default
  }

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

  protected class DefaultKeySet extends Set[A] {
    def contains(key : A) = self.contains(key)
    def elements = self.elements.map(_._1)
    def + (elem: A): Set[A] = (Set[A]() ++ this + elem).asInstanceOf[Set[A]] // !!! concrete overrides abstract problem
    def - (elem: A): Set[A] = (Set[A]() ++ this - elem).asInstanceOf[Set[A]] // !!! concrete overrides abstract problem
    override def size = self.size
    override def foreach[B](f: A => B) = for ((k, v) <- self) f(k)
  }

  /** @return the keys of this map as a set.
   */
  def keySet: Set[A] = new DefaultKeySet

  /** Creates an iterator for a contained values.
   *
   *  @return an iterator over all values.
   */
  def values: Iterator[B] = new Iterator[B] {
    val iter = self.elements
    def hasNext = iter.hasNext
    def next = iter.next._2
  }

  /** @return the values of this map as a set.
   *  @note  Can't return a Set[B] here because sets are non-variant.
   *         The operation is overridden with the sharper type in MutableMapTemplate.
   */
  def valueSet: immutable.Set[_ <: B] = immutable.Set.empty[B] ++ (self map (_._2))

  /** The default value for the map, returned when a key is not found
   *  The method implemented here yields an error,
   *  but it might be overridden in subclasses.
   *
   *  @param key the given key value
   *  @throws Predef.NoSuchElementException
   */
  def default(key: A): B =
    throw new NoSuchElementException("key not found: " + key)

  /** A map view consisting only of those key value pairs where the key satisfies a given
   *  predicate `p`.
   */
  def filterKeys(p: A => Boolean): This = this filter (kv => p(kv._1))

  /** A map view resulting from applying a given function `f` to each value.
   */
  def mapValues[C](f: B => C) = new DefaultMap[A, C] {
    override def foreach[D](g: ((A, C)) => D): Unit = for ((k, v) <- self) g((k, f(v)))
    def elements = for ((k, v) <- self.elements) yield (k, f(v))
    override def size = self.size
    override def contains(key: A) = self.contains(key)
    def get(key: A) = self.get(key).map(f)
  }

  /** @deprecated  use mapValues instead
   */
  @deprecated def mapElements[C](f: B => C) = mapValues(f)

  // The following 5 operations (updated, two times +, two times ++) should really be
  // generic, returning This[B]. We need better covariance support to express that though.
  // So right now we do the brute force approach of code duplication.

  /** A new immutable map containing updating this map with a given key/value mapping.
   *  @param    key the key
   *  @param    value the value
   *  @return   A new map with the new key/value mapping
   */
  def updated [B1 >: B](key: A, value: B1): Map[A, B1] = this + ((key, value))

  /** Adds two or more elements to this collection and returns
   *  a new collection.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): Map[A, B1] =
    this + elem1 + elem2 ++ elems

  /** Adds a number of elements provided by a traversable object
   *  and returns a new collection with the added elements.
   *
   *  @param elems     the traversable object.
   */
  def ++[B1 >: B](elems: Traversable[(A, B1)]): Map[A, B1] =
    ((thisCollection: Map[A, B1]) /: elems) (_ + _)

  /** Adds a number of elements provided by an iterator
   *  and returns a new collection with the added elements.
   *
   *  @param iter   the iterator
   */
  def ++[B1 >: B] (iter: Iterator[(A, B1)]): Map[A, B1] =
    ((thisCollection: Map[A, B1]) /: iter) (_ + _)

  /** Creates a string representation for this map.
   *
   *  @return    a string showing all mappings
   */
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
    elements.map { case (k, v) => k+" -> "+v }.addString(b, start, sep, end)

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
      if (this.size == other.size)
        try { // can we find a safer way to do this?
          this forall {
            case (key, value) => other.get(key.asInstanceOf[a]) match {
              case None => false
              case Some(otherval) => value == otherval
            }
          }
        } catch {
          case ex: ClassCastException => false
        }
      else false
    case _ => false
  }

  /** Defines the prefix of this object's <code>toString</code> representation.
   */
  override def stringPrefix: String = "Map"

  /** Need to override string, so that it's not the Function1's string that gets mixed in.
   */
  override def toString = super[IterableTemplate].toString
}
