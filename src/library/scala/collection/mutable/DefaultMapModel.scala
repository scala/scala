/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable

/** This class is used internally. It implements the mutable `Map`
 *  class in terms of three functions: `findEntry`, `addEntry`, and `entries`.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 *  @since   1
 */
@deprecated("this trait will be removed", "2.11.0")
trait DefaultMapModel[A, B] extends Map[A, B] {

  type Entry = DefaultEntry[A, B]

  protected def findEntry(key: A): Entry
  protected def addEntry(e: Entry)
  protected def entries: Iterator[Entry]

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

  def += (kv: (A, B)): this.type = { put(kv._1, kv._2); this }

  def iterator = entries map {e => (e.key, e.value)}

}
