/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._

/** This class implements mutable maps using a hashtable.
 *  The iterator and all traversal methods of this class visit elements in the order they were inserted.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.7
 */
object LinkedHashMap extends MutableMapFactory[LinkedHashMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), LinkedHashMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B] = new LinkedHashMap[A, B]
}

/**
 * @since 2.7
 */
@serializable @SerialVersionUID(1L)
class LinkedHashMap[A, B] extends Map[A, B]
                             with MapLike[A, B, LinkedHashMap[A, B]]
                             with HashTable[A] {

  override def empty = LinkedHashMap.empty[A, B]
  override def size = tableSize

  type Entry = LinkedEntry[A, B]

  @transient protected var firstEntry: Entry = null
  @transient protected var lastEntry: Entry = null

  def get(key: A): Option[B] = {
    val e = findEntry(key)
    if (e == null) None
    else Some(e.value)
  }

  override def put(key: A, value: B): Option[B] = {
    val e = findEntry(key)
    if (e == null) {
      val e = new Entry(key, value)
      addEntry(e)
      updateLinkedEntries(e)
      None
    } else {
      val v = e.value
      e.value = value
      Some(v)
    }
  }

  private def updateLinkedEntries(e: Entry) {
    if (firstEntry == null) firstEntry = e
    else { lastEntry.later = e; e.earlier = lastEntry }
    lastEntry = e
  }

  override def remove(key: A): Option[B] = {
    val e = removeEntry(key)
    if (e eq null) None
    else {
      if (e.earlier eq null) firstEntry = e.later
      else e.earlier.later = e.later
      if (e.later eq null) lastEntry = e.earlier
      else e.later.earlier = e.earlier
      Some(e.value)
    }
  }

  def += (kv: (A, B)): this.type = { put(kv._1, kv._2); this }
  def -=(key: A): this.type = { remove(key); this }

  def iterator: Iterator[(A, B)] = new Iterator[(A, B)] {
    private var cur = firstEntry
    def hasNext = cur ne null
    def next =
      if (hasNext) { val res = (cur.key, cur.value); cur = cur.later; res }
      else Iterator.empty.next
  }

  override def keysIterator: Iterator[A] = new Iterator[A] {
    private var cur = firstEntry
    def hasNext = cur ne null
    def next =
      if (hasNext) { val res = cur.key; cur = cur.later; res }
      else Iterator.empty.next
  }

  override def valuesIterator: Iterator[B] = new Iterator[B] {
    private var cur = firstEntry
    def hasNext = cur ne null
    def next =
      if (hasNext) { val res = cur.value; cur = cur.later; res }
      else Iterator.empty.next
  }

  override def foreach[U](f: ((A, B)) => U) = {
    var cur = firstEntry
    while (cur ne null) {
      f((cur.key, cur.value))
      cur = cur.later
    }
  }

  override def clear() {
    clearTable()
    firstEntry = null
  }

  private def writeObject(out: java.io.ObjectOutputStream) {
    serializeTo(out, _.value)
  }

  private def readObject(in: java.io.ObjectInputStream) {
    init[B](in, { (key, value) =>
      val entry = new Entry(key, value)
      updateLinkedEntries(entry)
      entry
    })
  }
}
