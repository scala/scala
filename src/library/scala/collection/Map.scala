/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection

import generic._

/**
 *  A map from keys of type `A` to values of type `B`.
 *
 *  $mapnote
 *
 *  '''Note:''' If you do not have specific implementations for `add` and `-` in mind,
 *        you might consider inheriting from `DefaultMap` instead.
 *
 *  '''Note:''' If your additions and mutations return the same kind of map as the map
 *        you are defining, you should inherit from `MapLike` as well.
 *
 *  @tparam A     the type of the keys in this map.
 *  @tparam B     the type of the values associated with keys.
 *
 *  @since 1
 */
trait Map[A, +B] extends Iterable[(A, B)] with MapLike[A, B, Map[A, B]] {
  def empty: Map[A, B] = Map.empty
}

/** $factoryInfo
 *  @define Coll Map
 *  @define coll map
 */
object Map extends MapFactory[Map] {
  def empty[A, B]: immutable.Map[A, B] = immutable.Map.empty

  /** $mapCanBuildFromInfo */
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), Map[A, B]] = new MapCanBuildFrom[A, B]
}
