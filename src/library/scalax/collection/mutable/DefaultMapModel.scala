/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: DefaultMapModel.scala 16893 2009-01-13 13:09:22Z cunei $


package scalax.collection.mutable

/** This class is used internally. It implements the mutable <code>Map</code>
 *  class in terms of three functions: <code>findEntry</code>,
 *  <code>addEntry</code>, and <code>entries</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
trait DefaultMapModel[A, B] extends Map[A, B] {

  type Entry = DefaultEntry[A, B]

  protected def findEntry(key: A): Entry
  protected def addEntry(e: Entry)
  protected def entries: Iterator[Entry]

  def get(key: A): Option[B] = {
    val e = findEntry(key)
    if (e == null) None
    else Some(e.value);
  }

  def update(key: A, value: B): this.type = {
    val e = findEntry(key)
    if (e == null) addEntry(new Entry(key, value))
    else e.value = value
    this
  }

  def elements = entries map {e => (e.key, e.value)}
}

