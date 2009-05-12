/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Map.scala 16884 2009-01-09 16:52:09Z cunei $


package scala.collection.generic

/** A generic template for immutable maps from keys of type A to values of type B.
 *  To implement a concrete map, you need to provide implementations of the following methods:
 *  (where This is the type of the map in question):
 *
 *   def get(key: A): Option[B]
 *   def elements: Iterator[(A, B)]
 *   def updated(key: A, value: B): This
 *   def minus(key: A): This
 *
 * If you wish that methods like, take, drop, filter return the same kind of map, you should also
 * override:
 *
 *   def empty: This
 *
 * It is also good idea to override methods foreach and size for efficiency.
 */
trait ImmutableMapTemplate[A, +B, +This <: ImmutableMapTemplate[A, B, This] with immutable.Map[A, B]]
  extends MapTemplate[A, B, This] {
self =>

  override protected[this] def newBuilder: Builder[(A, B), This] = new ImmutableMapBuilder[A, B, This](empty)

  /** A new immutable map containing updating this map with a given key/value mapping.
   *  @param    key the key
   *  @param    value the value
   *  @return   A new map with the new key/value mapping
   */
  def updated [B1 >: B](key: A, value: B1): immutable.Map[A, B1]

  /** Add a key/value pair to this map, returning a new map.
   *  @param    kv the key/value pair
   *  @return   A new map with the new binding added to this map
   */
  override def plus [B1 >: B] (kv: (A, B1)): immutable.Map[A, B1] = updated(kv._1, kv._2)

  /** Add a key/value pair to this map, returning a new map.
   *  @param    kv the key/value pair
   *  @return   A new map with the new binding added to this map
   *  @note  same as `plus`
   */
  override def + [B1 >: B] (kv: (A, B1)): immutable.Map[A, B1] = updated(kv._1, kv._2)

  /** Adds two or more elements to this collection and returns
   *  a new collection.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  override def plus [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): immutable.Map[A, B1] =
    this plus elem1 plus elem2 plusAll elems

  /** Adds two or more elements to this collection and returns
   *  a new collection.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): immutable.Map[A, B1] =
    plus(elem1, elem2, elems: _*)

  /** Adds a number of elements provided by a traversable object
   *  and returns a new collection with the added elements.
   *
   *  @param elems     the traversable object.
   */
  override def plusAll[B1 >: B](elems: Traversable[(A, B1)]): immutable.Map[A, B1] =
    ((thisCollection: immutable.Map[A, B1]) /: elems) (_ plus _)

  /** Adds a number of elements provided by a traversable object
   *  and returns a new collection with the added elements.
   *
   *  @param elems     the traversable object.
   *  @note  same as `plusAll`
   *  @note  This is a more efficient version of Traversable.++ which avoids
   *         copying of the collection's elements. However, it applies only if
   *         the type of the added elements is a subtype of the element type of the
   *         collection.
   */
  override def ++ [B1 >: B](elems: Traversable[(A, B1)]): immutable.Map[A, B1] = plusAll(elems)

  /** Adds a number of elements provided by an iterator
   *  and returns a new collection with the added elements.
   *
   *  @param iter   the iterator
   */
  override def plusAll[B1 >: B] (iter: Iterator[(A, B1)]): immutable.Map[A, B1] =
    ((thisCollection: immutable.Map[A, B1]) /: iter) (_ plus _)

  /** Adds a number of elements provided by an iterator
   *  and returns a new collection with the added elements.
   *
   *  @param iter   the iterator
   *  @note  same as `plusAll`
   *  @note  This is a more efficient version of Traversable.++ which avoids
   *         copying of the collection's elements. However, it applies only if
   *         the type of the added elements is a subtype of the element type of the
   *         collection.
   */
  override def ++ [B1 >: B](iter: Iterator[(A, B1)]): immutable.Map[A, B1] = plusAll(iter)

  /** This function transforms all the values of mappings contained
   *  in this map with function <code>f</code>.
   *
   *  @param f A function over keys and values
   *  @return  the updated map
   */
  def transform[C, That](f: (A, B) => C)(implicit bf: BuilderFactory[(A, C), That, This]): That = {
    val b = bf(thisCollection)
    for ((key, value) <- this) b += ((key, f(key, value)))
    b.result
  }

  /** Returns a new map with all key/value pairs for which the predicate
   *  <code>p</code> returns <code>true</code>.
   *
   *  @param p A predicate over key-value pairs
   *  @note    This method works by successively removing elements fro which the predicate is false from this set.
   *           If removal is slow, or you expect that most elements of the set will be removed,
   *           you might consider using `filter` with a negated predicate instead.
   */
  override def filterNot(p: ((A, B)) => Boolean): This = {
    var res: This = thisCollection
    for (kv <- this)
      if (p(kv)) res = (res minus kv._1).asInstanceOf[This] // !!! concrete overrides abstract problem
    res
  }

  /** @deprecated use updated instead */
  @deprecated def update[B1 >: B](key: A, value: B1): immutable.Map[A, B1] = updated(key, value).asInstanceOf[immutable.Map[A, B1]]
}
