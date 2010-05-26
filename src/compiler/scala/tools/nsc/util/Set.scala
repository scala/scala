/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package util

/** A common class for lightweight sets.
 */
abstract class Set[T <: AnyRef] {

  def findEntry(x: T): T

  def addEntry(x: T): Unit

  def iterator: Iterator[T]

  @deprecated("use `iterator' instead") def elements = iterator

  def contains(x: T): Boolean =
    findEntry(x) ne null

  def toList = iterator.toList

}
