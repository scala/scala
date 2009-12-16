/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package immutable

import generic._
import mutable.{Builder, StringBuilder}
import scala.util.matching.Regex

/**
 * @since 2.8
 */
class WrappedString(override val self: String) extends IndexedSeq[Char] with StringLike[WrappedString] with Proxy {

  override protected[this] def thisCollection: WrappedString = this
  override protected[this] def toCollection(repr: WrappedString): WrappedString = repr

  /** Creates a string builder buffer as builder for this class */
  override protected[this] def newBuilder = WrappedString.newBuilder
}

/**
 * @since 2.8
 */
object WrappedString {
  def newBuilder: Builder[Char, WrappedString] = new StringBuilder() mapResult (new WrappedString(_))
}
