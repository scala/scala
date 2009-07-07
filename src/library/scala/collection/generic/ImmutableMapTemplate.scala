/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.generic

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
 */
trait ImmutableMapTemplate[A, +B, +This <: ImmutableMapTemplate[A, B, This] with immutable.Map[A, B]]
  extends MapTemplate[A, B, This] {
self =>

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
  override def ++[B1 >: B](elems: Traversable[(A, B1)]): immutable.Map[A, B1] =
    ((thisCollection: immutable.Map[A, B1]) /: elems) (_ + _)

  /** Adds a number of elements provided by an iterator
   *  and returns a new collection with the added elements.
   *
   *  @param iter   the iterator
   */
  override def ++[B1 >: B] (iter: Iterator[(A, B1)]): immutable.Map[A, B1] =
    ((thisCollection: immutable.Map[A, B1]) /: iter) (_ + _)

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
   *  @note    This method works by successively removing elements fro which the
   *           predicate is false from this set.
   *           If removal is slow, or you expect that most elements of the set$
   *           will be removed, you might consider using <code>filter</code>
   *           with a negated predicate instead.
   */
  override def filterNot(p: ((A, B)) => Boolean): This = {
    var res: This = thisCollection
    for (kv <- this)
      if (p(kv)) res = (res - kv._1).asInstanceOf[This] // !!! concrete overrides abstract problem
    res
  }

  @deprecated("use `updated' instead")
  def update[B1 >: B](key: A, value: B1): immutable.Map[A, B1] = updated(key, value).asInstanceOf[immutable.Map[A, B1]]
}
