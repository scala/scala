/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package mutable

/** This class is used internally. It implements the mutable `Map`
 *  class in terms of three functions: `findEntry`, `addEntry`, and `entries`.
 *
 *  @author  Matthias Zenger
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
