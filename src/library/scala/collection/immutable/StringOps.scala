/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package immutable

import mutable.StringBuilder

/** This class serves as a wrapper providing [[scala.Predef.String]]s with all
 *  the operations found in indexed sequences. Where needed, `String`s are
 *  implicitly converted into instances of this class.
 *
 *  The difference between this class and `WrappedString` is that calling transformer
 *  methods such as `filter` and `map` will yield a `String` object, whereas a
 *  `WrappedString` will remain a `WrappedString`.
 *
 *  @param repr     the actual representation of this string operations object.
 *
 *  @since 2.8
 *  @define Coll `String`
 *  @define coll string
 */
final class StringOps(override val repr: String) extends AnyVal with StringLike[String] {

  override protected[this] def thisCollection: WrappedString = new WrappedString(repr)
  override protected[this] def toCollection(repr: String): WrappedString = new WrappedString(repr)

  /** Creates a string builder buffer as builder for this class */
  override protected[this] def newBuilder = StringBuilder.newBuilder

  override def apply(index: Int): Char = repr charAt index
  override def slice(from: Int, until: Int): String = {
    val start = if (from < 0) 0 else from
    if (until <= start || start >= repr.length)
      return ""

    val end = if (until > length) length else until
    repr.substring(start, end)
  }
  override def toString = repr
  override def length = repr.length

  def seq = new WrappedString(repr)
}
