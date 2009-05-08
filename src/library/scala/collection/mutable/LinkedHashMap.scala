/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

import Predef._

/** This class implements mutable maps using a hashtable.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 31/12/2006
 */
object LinkedHashMap {

  /** The empty map of this type */
  def empty[A, B] = new LinkedHashMap[A, B]

  /** The canonical factory for this type
   */
  def apply[A, B](elems: (A, B)*) = empty[A, B] ++ elems
}

@serializable
class LinkedHashMap[A, B] extends Map[A,B] with HashTable[A] with DefaultMapModel[A,B] {

  override def empty = LinkedHashMap.empty
  override def size = super[HashTable].size

  private var ordered = List[Entry]()

  def remove(key: A): Option[B] = removeEntry(key) match {
    case None => None
    case Some(e) =>
      ordered = ordered.filter(_ ne e)
      Some(e.value)
    }

  def -= (key: A) { remove(key) }

  override def put(key: A, value: B): Option[B] = {
    val e = findEntry(key)
    if (e == null) {
      val e = new Entry(key, value)
      ordered = e :: ordered
      addEntry(e)
      None
    } else {
      val ret = Some(e.value)
      e.value = value
      ret
    }
  }
  override def update(key: A, value: B) { put(key, value) }

  override def clear() {
    ordered = Nil
    super.clear()
  }

  override def clone(): Map[A, B] = new LinkedHashMap[A, B] ++ this

  override def elements = ordered.reverse.elements map {e => (e.key, e.value)}
}
