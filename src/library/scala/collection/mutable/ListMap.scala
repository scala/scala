/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: HashMap.scala 17715 2009-05-12 18:33:22Z odersky $


package scala.collection.mutable

import scala.collection.generic._

@serializable
class ListMap[A, B] extends Map[A, B] with MutableMapTemplate[A, B, ListMap[A, B]] {


  override def empty = ListMap.empty[A, B]

  private var elems: List[(A, B)] = List()
  private var siz: Int = 0

  def get(key: A): Option[B] = elems find (_._1 == key) map (_._2)
  def iterator: Iterator[(A, B)] = elems.iterator
  def += (kv: (A, B)) = { elems = remove(kv._1, elems); elems = kv :: elems; siz += 1; this }
  def -= (key: A) = { elems = remove(key, elems); this }

  private def remove(key: A, elems: List[(A, B)]): List[(A, B)] =
    if (elems.isEmpty) elems
    else if (elems.head._1 == key) { siz -= 1; elems.tail }
    else elems.head :: remove(key, elems.tail)

  override def clear() = elems = List()
  override def size: Int = siz
}

/** This class implements mutable maps using a list.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
object ListMap extends MutableMapFactory[ListMap] {
  implicit def builderFactory[A, B]: BuilderFactory[(A, B), ListMap[A, B], Coll] = new MapBuilderFactory[A, B]
  def empty[A, B]: ListMap[A, B] = new ListMap[A, B]
}
