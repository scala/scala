/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import generic._

/** <p>
 *    A map from keys of type <code>A</code> to values of type <code>B</code>.
 *    To implement a concrete map, you need to provide implementations of the
 *    following methods (where <code>This</code> is the type of the map in question):
 *  </p><pre>
 *    <b>def</b> get(key: A): Option[B]
 *    <b>def</b> iterator: Iterator[(A, B)]
 *    <b>def</b> + [B1 >: B](kv: (A, B1)): This
 *    <b>def</b> -(key: A): This</pre>
 *  <p>
 *    If you wish that methods like, take, drop, filter return the same kind
 *    of map, you should also override:
 *  </p><pre>
 *    <b>def</b> empty: This</pre>
 *  <p>
 *    It might also be a good idea to override methods <code>foreach</code>
 *    and <code>size</code> for efficiency.
 *  </p>
 *
 * @note If you do not have specific implementations for `add` and `-` in mind,
 *       you might consider inheriting from <code>DefaultMap</code> instead.
 *
 * @note If your additions and mutations return the same kind of map as the map
 *       you are defining, you should inherit from <code>MapLike</code> as well.
 *
 * @since 1
 */
trait Map[A, +B] extends Iterable[(A, B)] with MapLike[A, B, Map[A, B]] {
  def empty: Map[A, B] = Map.empty
}

/* Factory object for `Map` class
 *
 * @since 2.5
 */
object Map extends MapFactory[Map] {
  def empty[A, B]: immutable.Map[A, B] = immutable.Map.empty

  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), Map[A, B]] = new MapCanBuildFrom[A, B]
}
