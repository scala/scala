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
 *  (where This is the type of the map in question):
 *
 *   def get(key: A): Option[B]
 *   def elements: Iterator[(A, B)]
 *   def update(key: A, value: B)
 *   def -(key: A): This
 *
 * If you wish that methods like, take, drop, filter return the same kind of map, you should also
 * override:
 *
 *   def empty: This
 *
 * It is also good idea to override methods foreach and size for efficiency.
 */
trait MutableMapTemplate[A, B, +This <: MutableMapTemplate[A, B, This] with mutable.Map[A, B]]
  extends MapTemplate[A, B, This]
     with Builder[(A, B), This]
     with Growable[(A, B)]
     with Shrinkable[A]
     with Cloneable[This]
{ self =>

  override protected[this] def newBuilder: Builder[(A, B), This] = new MutableMapBuilder[A, B, This](empty.asInstanceOf[This]) // !!! concrete overrides abstract problem

  /** This method allows one to add a new mapping from <code>key</code>
   *  to <code>value</code> to the map. If the map already contains a
   *  mapping for <code>key</code>, it will be overridden by this
   *  function.
   *
   * @param key    The key to update
   * @param value  The new value
   */
  def update(key: A, value: B)

  /** Add a key/vaue pair to this map, and return the map itself
   */
  def add(key: A, value: B): this.type = { update(key, value); this }

  /** Map <code>key</code> to <code>elem</code> in this map and return the element
   *  that the key was previously mapped to (if any).
   */
  def put(key: A, elem: B): Option[B] = {
    val ret = get(key)
    this(key) = elem
    ret
  }

  /** Add a key/value pair to this map.
   *  @param    kv the key/value pair.
   */
  def += (kv: (A, B)) { update(kv._1, kv._2) }

  /** A default implementation of addition where the value is not of the same type as
   *  the current map value type. This will always create a new immutable Map.
   */
  def add[B1 >: B](key: A, value: B1): collection.Map[A, B1] =
    Map[A, B1]() ++ thisCollection + ((key, value))

  /** Adds a single element to this collection and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the added element.
   *
   *  @param elem  the element to add.
   */
  //override
   def +(kv: (A, B)): this.type = add(kv._1, kv._2)

  /** Adds two or more elements to this collection and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the added elements.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  //override
  def +(elem1: (A, B), elem2: (A, B), elems: (A, B)*): this.type =
    this + elem1 + elem2 ++ Iterable.fromOld(elems)

  /** Adds a number of elements provided by an iterable object
   *  via its <code>elements</code> method and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the added elements.
   *
   *  @param iter     the iterable object.
   */
  //override
  def ++(iter: Traversable[(A, B)]): this.type = { (this /: iter) (_ + _); this }

  /** Adds a number of elements provided by an iterator
   *  via its <code>elements</code> method and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the added elements.
   *
   *  @param iter   the iterator
   */
  //override
  def ++(iter: Iterator[(A, B)]): this.type = { (this /: iter) (_ + _); this }

  /** Remove a key from this map, noop if key is not presentg.
   *  @param    key the key to be removed
   */
  def -= (key: A)

  def -(key: A): This = { -=(key); thisCollection }

  def removeKey(key: A): Option[B] = {
    val ret = get(key)
    this -= key
    ret
  }

  // todo: add other -'s

  /** Removes all elements from the set. After this operation is completed,
   *  the set will be empty.
   */
  def clear() { for ((k, v) <- elements) -=(k) }

  /** Check if this map maps <code>key</code> to a value.
    * Return that value if it exists, otherwise put <code>default</code>
    * as that key's value and return it.
    */
  def getOrElseUpdate(key: A, default: => B): B =
    get(key) match {
      case Some(v) => v
      case None => val d = default; this(key) = d; d
    }

  /** This function transforms all the values of mappings contained
   *  in this map with function <code>f</code>.
   *
   * @param f  The transformation to apply
   */
  def transform(f: (A, B) => B): this.type = {
    elements foreach {
      case (key, value) => update(key, f(key, value))
    }
    this
  }

  /** This method retains only those mappings for which the predicate
   *  <code>p</code> returns <code>true</code>.
   *
   * @param p  The test predicate
   * @deprecated cannot be type inferred because if retain in Iterable.
   */
  def retain(p: (A, B) => Boolean): this.type = {
    for ((k, v) <- this) if (!p(k, v)) -=(k)
    this
  }

  override def clone(): This =
    empty ++ thisCollection

  /** The result when this map is used as a builder */
  def result: This = thisCollection
}
