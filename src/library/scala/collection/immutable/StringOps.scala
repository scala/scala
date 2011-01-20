/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package immutable

import mutable.StringBuilder

/**
 *  This class serves as a wrapper providing `String`s with all the operations
 *  found in indexed sequences. Where needed, instances of `String` object
 *  are implicitly converted into this class.
 *
 *  The difference between this class and `WrappedString` is that calling transformer
 *  methods such as `filter` and `map` will yield a `String` object, whereas a
 *  `WrappedString` will remain a `WrappedString`.
 *
 *  @param repr     the actual representation of this string operations object.
 *
 *  @since 2.8
 *  @define Coll StringOps
 *  @define coll string
 */
final class StringOps(override val repr: String) extends StringLike[String] {

  override protected[this] def thisCollection: WrappedString = new WrappedString(repr)
  override protected[this] def toCollection(repr: String): WrappedString = new WrappedString(repr)

  /** Creates a string builder buffer as builder for this class */
  override protected[this] def newBuilder = StringBuilder.newBuilder

  override def slice(from: Int, until: Int): String = {
    /** Slice must be forgiving on all out of bounds indices and
     *  substring is not.
     */
    val start = from max 0
    val end   = until min repr.length

    if (start >= end) ""
    else repr.substring(start, end)
  }

  override def toString = repr
}
