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
 *   def add(key: A, value: B): This
 *   def -(key: A): This
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

  override protected[this] def newBuilder: Builder[(A, B), This, Any] = new ImmutableMapBuilder[A, B, This](empty)

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

  /** @deprecated use add instead */
  @deprecated def update[B1 >: B](key: A, value: B1): immutable.Map[A, B1] = add(key, value).asInstanceOf[immutable.Map[A, B1]]
}
