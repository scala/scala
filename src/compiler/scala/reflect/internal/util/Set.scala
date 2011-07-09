/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.reflect.internal.util

/** A common class for lightweight sets.
 */
abstract class Set[T <: AnyRef] {

  def findEntry(x: T): T

  def addEntry(x: T): Unit

  def iterator: Iterator[T]

  def foreach[U](f: T => U): Unit = iterator foreach f

  def apply(x: T): Boolean = contains(x)

  @deprecated("use `iterator` instead", "2.9.0") def elements = iterator

  def contains(x: T): Boolean =
    findEntry(x) ne null

  def toList = iterator.toList

}
