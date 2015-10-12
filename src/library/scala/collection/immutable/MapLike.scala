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
 *  A generic template for immutable maps from keys of type `A`
 *  to values of type `B`.
 *  To implement a concrete map, you need to provide implementations of the
 *  following methods (where `This` is the type of the actual map implementation):
 *
 *  {{{
 *    def get(key: A): Option[B]
 *    def iterator: Iterator[(A, B)]
 *    def + [B1 >: B](kv: (A, B)): Map[A, B1]
 *    def - (key: A): This
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
 *  @tparam A     the type of the keys contained in this collection.
 *  @tparam B     the type of the values associated with the keys.
 *  @tparam This  The type of the actual map implementation.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @define Coll immutable.Map
 *  @define coll immutable map
 */
trait MapLike[A, +B, +This <: MapLike[A, B, This] with Map[A, B]]
  extends scala.collection.MapLike[A, B, This]
     with Parallelizable[(A, B), ParMap[A, B]]
{
self =>

  protected[this] override def parCombiner = ParMap.newCombiner[A, B]

  /** A new immutable map containing updating this map with a given key/value mapping.
   *  @param    key the key
   *  @param    value the value
   *  @return   A new map with the new key/value mapping
   */
  override def updated [B1 >: B](key: A, value: B1): immutable.Map[A, B1] = this + ((key, value))

  /** Add a key/value pair to this map, returning a new map.
   *  @param    kv the key/value pair.
   *  @return   A new map with the new binding added to this map.
   */
  def + [B1 >: B] (kv: (A, B1)): immutable.Map[A, B1]

  /** Adds two or more elements to this collection and returns
   *  a new collection.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   *  @return A new map with the new bindings added to this map.
   */
  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): immutable.Map[A, B1] =
    this + elem1 + elem2 ++ elems

  /** Adds a number of elements provided by a traversable object
   *  and returns a new collection with the added elements.
   *
   *  @param xs      the traversable object consisting of key-value pairs.
   *  @return        a new immutable map with the bindings of this map and those from `xs`.
   */
  override def ++[B1 >: B](xs: GenTraversableOnce[(A, B1)]): immutable.Map[A, B1] =
    ((repr: immutable.Map[A, B1]) /: xs.seq) (_ + _)

  /** Filters this map by retaining only keys satisfying a predicate.
   *  @param  p   the predicate used to test keys
   *  @return an immutable map consisting only of those key value pairs of this map where the key satisfies
   *          the predicate `p`. The resulting map wraps the original map without copying any elements.
   */
  override def filterKeys(p: A => Boolean): Map[A, B] = new FilteredKeys(p) with DefaultMap[A, B]

  /** Transforms this map by applying a function to every retrieved value.
   *  @param  f   the function used to transform values of this map.
   *  @return a map view which maps every key of this map
   *          to `f(this(key))`. The resulting map wraps the original map without copying any elements.
   */
  override def mapValues[C](f: B => C): Map[A, C] = new MappedValues(f) with DefaultMap[A, C]

  /** Collects all keys of this map in a set.
   *  @return  a set containing all keys of this map.
   */
  override def keySet: immutable.Set[A] = new ImmutableDefaultKeySet

  protected class ImmutableDefaultKeySet extends super.DefaultKeySet with immutable.Set[A] {
    override def + (elem: A): immutable.Set[A] =
      if (this(elem)) this
      else immutable.Set[A]() ++ this + elem
    override def - (elem: A): immutable.Set[A] =
      if (this(elem)) immutable.Set[A]() ++ this - elem
      else this
      
    // ImmutableDefaultKeySet is only protected, so we won't warn on override.
    // Someone could override in a way that makes widening not okay
    // (e.g. by overriding +, though the version in this class is fine)
    override def toSet[B >: A]: Set[B] = this.asInstanceOf[Set[B]]
  }

  /** This function transforms all the values of mappings contained
   *  in this map with function `f`.
   *
   *  @param f A function over keys and values
   *  @return  the updated map
   */
  def transform[C, That](f: (A, B) => C)(implicit bf: CanBuildFrom[This, (A, C), That]): That = {
    val b = bf(repr)
    for ((key, value) <- this) b += ((key, f(key, value)))
    b.result()
  }
}

