/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import generic._

/** A map from keys of type A to values of type B.
 *  To implement a concrete map, you need to provide implementations of the following methods:
 *  (where This is the type of the map in question):
 *
 *   def get(key: A): Option[B]
 *   def elements: Iterator[(A, B)]
 *   def add[B1 >: B](key: A, value: B1): This
 *   def -(key: A): This
 *
 * If you wish that methods like, take, drop, filter return the same kind of map, you should also
 * override:
 *
 *   def empty: This
 *
 * It might also be a good idea to override methods foreach and size for efficiency.
 *
 * @note If you do not have speicifc implementations for `add` and `-` in mind, you
 *       might consider inheriting from `DefaultMap` instead.
 *
 * @note Of you additions and mutations return the same kind of map as the map you are defining,
 *       you should inherit from `MapTemplate` as well.
 */
trait Map[A, +B] extends Iterable[(A, B)] with MapTemplate[A, B, Map[A, B]] {
  def empty: Map[A, B] = Map.empty

  override protected[this] def newBuilder : Builder[(A, B), Map[A, B]] =
    throw new UnsupportedOperationException("Map.newBuilder")

  def mapBuilder[A, B]: Builder[(A, B), Map[A, B]] = Map.newBuilder[A, B]
}

/* Factory object for `Map` class */
object Map extends ImmutableMapFactory[immutable.Map] {
  type Coll = Map[_, _]
  def empty[A, B]: immutable.Map[A, B] = immutable.Map.empty
  implicit def builderFactory[A, B]: BuilderFactory[(A, B), Map[A, B], Coll] = new BuilderFactory[(A, B), Map[A, B], Coll] { def apply(from: Coll) = from.mapBuilder[A, B] }
}
