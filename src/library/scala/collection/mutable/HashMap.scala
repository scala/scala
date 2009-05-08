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


@serializable
class HashMap[A, B] extends Map[A, B] with MutableMapTemplate[A, B, HashMap[A, B]] with HashTable[A] with DefaultMapModel[A, B] {

  override def empty: HashMap[A, B] = HashMap.empty[A, B]
  override def mapBuilder[A, B]: Builder[(A, B), HashMap[A, B], Any] = HashMap.newBuilder[A, B]

  def -= (key: A) { removeEntry(key) }

  override def clear() = super.clear()

  override def size: Int = super[HashTable].size
}

/** This class implements mutable maps using a hashtable.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 */
object HashMap extends MutableMapFactory[HashMap] {
  type Coll = HashMap[_, _]
  implicit def builderFactory[A, B]: BuilderFactory[(A, B), HashMap[A, B], Coll] = new BuilderFactory[(A, B), HashMap[A, B], Coll] { def apply(from: Coll) = from.mapBuilder[A, B] }
  def empty[A, B]: HashMap[A, B] = new HashMap[A, B]
}
