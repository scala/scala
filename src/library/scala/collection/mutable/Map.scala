/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._

/** This trait represents mutable maps.
 *  All implementations od mutable maps inherit from it.
 *
 *  $mapnote
 *
 *  @tparam A    the type of the keys of the map.
 *  @tparam B    the type of associated values.
 */
trait Map[A, B]
  extends Iterable[(A, B)]
     with scala.collection.Map[A, B]
     with MapLike[A, B, Map[A, B]] {

  override def empty: Map[A, B] = Map.empty

  /* Return a read-only projection of this map.  !!! or just use an (immutable) MapProxy?
  def readOnly : scala.collection.Map[A, B] = new scala.collection.Map[A, B] {
    override def size = self.size
    override def update(key: A, value: B) = self.update(key, value)
    override def - (elem: A) = self - elem
    override def iterator = self.iterator
    override def foreach[U](f: ((A, B)) =>  U) = self.foreach(f)
    override def empty[C] = self.empty[C]
    def get(key: A) = self.get(key)
  }
  */
}

/** $factoryInfo
 *  @define Coll Map
 *  @define coll map
 */
object Map extends MutableMapFactory[Map] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), Map[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B]: Map[A, B] = new HashMap[A, B]
}

