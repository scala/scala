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
