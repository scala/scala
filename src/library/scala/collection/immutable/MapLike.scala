/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package immutable

import generic._

/** <p>
 *    A generic template for immutable maps from keys of type <code>A</code>
 *    to values of type <code>B</code>.<br/>
 *    To implement a concrete map, you need to provide implementations of the
 *    following methods (where <code>This</code> is the type of the map in
 *    question):
 *  </p>
 *  <pre>
 *    <b>def</b> get(key: A): Option[B]
 *    <b>def</b> iterator: Iterator[(A, B)]
 *    <b>def</b> + [B1 >: B](kv: (A, B)): Map[A, B1]
 *    <b>def</b> - (key: A): This</pre>
 *  <p>
 *    If you wish that methods <code>like</code>, <code>take</code>, <code>drop</code>,
 *    <code>filter</code> return the same kind of map, you should also override:
 *  </p>
 *  <pre>
 *    <b>def</b> empty: This</pre>
 *  <p>
 *    It is also good idea to override methods <code>foreach</code> and
 *    <code>size</code> for efficiency.
 *  </p>
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait MapLike[A, +B, +This <: MapLike[A, B, This] with Map[A, B]]
  extends scala.collection.MapLike[A, B, This]
{ self =>

  import scala.collection.Traversable

  /** A new immutable map containing updating this map with a given key/value mapping.
   *  @param    key the key
   *  @param    value the value
   *  @return   A new map with the new key/value mapping
   */
  override def updated [B1 >: B](key: A, value: B1): immutable.Map[A, B1] = this + ((key, value))

  /** Add a key/value pair to this map, returning a new map.
   *  @param    kv the key/value pair
   *  @return   A new map with the new binding added to this map
   */
  def + [B1 >: B] (kv: (A, B1)): immutable.Map[A, B1]

  /** Adds two or more elements to this collection and returns
   *  a new collection.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): immutable.Map[A, B1] =
    this + elem1 + elem2 ++ elems

  /** Adds a number of elements provided by a traversable object
   *  and returns a new collection with the added elements.
   *
   *  @param elems     the traversable object.
   */
  override def ++[B1 >: B](xs: TraversableOnce[(A, B1)]): immutable.Map[A, B1] =
    ((repr: immutable.Map[A, B1]) /: xs) (_ + _)

  /** Filters this map by retaining only keys satisfying a predicate.
   *  @param  p   the predicate used to test keys
   *  @return an immutable map consisting only of those key value pairs of this map where the key satisfies
   *          the predicate `p`. The resulting map wraps the original map without copying any elements.
   */
  override def filterKeys(p: A => Boolean): Map[A, B] = new DefaultMap[A, B] {
    override def foreach[C](f: ((A, B)) => C): Unit = for (kv <- self) if (p(kv._1)) f(kv)
    def iterator = self.iterator.filter(kv => p(kv._1))
    override def contains(key: A) = self.contains(key) && p(key)
    def get(key: A) = if (!p(key)) None else self.get(key)
  }

  /** Transforms this map by applying a function to every retrieved value.
   *  @param  d   the function used to transform values of this map.
   *  @return an immutable map which maps every key of this map
   *          to `f(this(key))`. The resulting map wraps the original map without copying any elements.
   */
  /** A map view resulting from applying a given function `f` to each value
   *  associated with a key in this map.
   */
  override def mapValues[C](f: B => C): Map[A, C] = new DefaultMap[A, C] {
    override def foreach[D](g: ((A, C)) => D): Unit = for ((k, v) <- self) g((k, f(v)))
    def iterator = for ((k, v) <- self.iterator) yield (k, f(v))
    override def size = self.size
    override def contains(key: A) = self.contains(key)
    def get(key: A) = self.get(key).map(f)
  }

  /** This function transforms all the values of mappings contained
   *  in this map with function <code>f</code>.
   *
   *  @param f A function over keys and values
   *  @return  the updated map
   */
  def transform[C, That](f: (A, B) => C)(implicit bf: CanBuildFrom[This, (A, C), That]): That = {
    val b = bf(repr)
    for ((key, value) <- this) b += ((key, f(key, value)))
    b.result
  }

  @deprecated("use `updated' instead")
  def update[B1 >: B](key: A, value: B1): immutable.Map[A, B1] = updated(key, value).asInstanceOf[immutable.Map[A, B1]]
}
