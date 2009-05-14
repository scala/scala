/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

import generic._

trait Map[A, B]
  extends Iterable[(A, B)]
     with collection.Map[A, B]
     with MutableMapTemplate[A, B, Map[A, B]]
     with Unhashable {

  override def empty: Map[A, B] = Map.empty

  /** Return a read-only projection of this map.  !!! or just use an (immutable) MapProxy?
  def readOnly : collection.Map[A, B] = new collection.Map[A, B] {
    override def size = self.size
    override def update(key: A, value: B) = self.update(key, value)
    override def - (elem: A) = self - elem
    override def elements = self.elements
    override def foreach[U](f: ((A, B)) =>  U) = self.foreach(f)
    override def empty[C] = self.empty[C]
    def get(key: A) = self.get(key)
  }
  */
}
/* Factory object for `Map` class
 * Currently this returns a HashMap.
 */
object Map extends MutableMapFactory[Map] {
  implicit def builderFactory[A, B]: BuilderFactory[(A, B), Map[A, B], Coll] = new MapBuilderFactory[A, B]
  def empty[A, B]: Map[A, B] = new HashMap[A, B]
}

