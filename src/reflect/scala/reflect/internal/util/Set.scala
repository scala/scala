/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect.internal.util

/** A common class for lightweight sets.
 */
abstract class Set[T <: AnyRef] {

  def findEntry(x: T): T

  def addEntry(x: T): Unit

  def iterator: Iterator[T]

  def foreach[U](f: T => U): Unit = iterator foreach f

  def apply(x: T): Boolean = contains(x)

  def contains(x: T): Boolean =
    findEntry(x) ne null

  def toList = iterator.toList
}
