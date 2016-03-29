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

import generic._
import mutable.{Builder, StringBuilder}

/**
 *  This class serves as a wrapper augmenting `String`s with all the operations
 *  found in indexed sequences.
 *
 *  The difference between this class and `StringOps` is that calling transformer
 *  methods such as `filter` and `map` will yield an object of type `WrappedString`
 *  rather than a `String`.
 *
 *  @param self    a string contained within this wrapped string
 *
 *  @since 2.8
 *  @define Coll `WrappedString`
 *  @define coll wrapped string
 */
final class WrappedString(val self: String) extends AbstractSeq[Char] with IndexedSeq[Char] with StringLike[WrappedString] {

  override protected[this] def thisCollection: WrappedString = this
  override protected[this] def toCollection(repr: WrappedString): WrappedString = repr

  /** Creates a string builder buffer as builder for this class */
  override protected[this] def newBuilder = WrappedString.newBuilder

  override def slice(from: Int, until: Int): WrappedString = {
    val start = if (from < 0) 0 else from
    if (until <= start || start >= repr.length)
      return new WrappedString("")

    val end = if (until > length) length else until
    new WrappedString(repr.substring(start, end))
  }
  override def length = self.length
  override def toString = self
}

/** A companion object for wrapped strings.
 *
 *  @since 2.8
 */
object WrappedString {
  implicit def canBuildFrom: CanBuildFrom[WrappedString, Char, WrappedString] = new CanBuildFrom[WrappedString, Char, WrappedString] {
    def apply(from: WrappedString) = newBuilder
    def apply() = newBuilder
  }

  def newBuilder: Builder[Char, WrappedString] = StringBuilder.newBuilder mapResult (x => new WrappedString(x))
}
