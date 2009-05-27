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
class HashMap[A, B] extends Map[A, B]
                       with MutableMapTemplate[A, B, HashMap[A, B]]
                       with HashTable[A] {

  override def empty: HashMap[A, B] = HashMap.empty[A, B]
  override def clear() = super.clear()
  override def size: Int = super[HashTable].size

  type Entry = DefaultEntry[A, B]

  def get(key: A): Option[B] = {
    val e = findEntry(key)
    if (e == null) None
    else Some(e.value)
  }

  override def put(key: A, value: B): Option[B] = {
    val e = findEntry(key)
    if (e == null) { addEntry(new Entry(key, value)); None }
    else { val v = e.value; e.value = value; Some(v) }
  }

  override def update(key: A, value: B): Unit = put(key, value)

  override def remove(key: A): Option[B] = {
    val e = removeEntry(key)
    if (e ne null) Some(e.value)
    else None
  }

  def += (kv: (A, B)): this.type = {
    val e = findEntry(kv._1)
    if (e == null) addEntry(new Entry(kv._1, kv._2))
    else e.value = kv._2
    this
  }

  def -=(key: A): this.type = { removeEntry(key); this }

  def iterator = entriesIterator map {e => (e.key, e.value)}
}

/** This class implements mutable maps using a hashtable.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 */
object HashMap extends MutableMapFactory[HashMap] {
  implicit def builderFactory[A, B]: BuilderFactory[(A, B), HashMap[A, B], Coll] = new MapBuilderFactory[A, B]
  def empty[A, B]: HashMap[A, B] = new HashMap[A, B]
}
