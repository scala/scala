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
package reflect
package internal
package util

trait StripMarginInterpolator {
  def stringContext: StringContext

  /**
   * A safe combination of [[scala.collection.StringOps#stripMargin]]
   * and [[scala.StringContext#raw]].
   *
   * The margin of each line is defined by whitespace leading up to a '|' character.
   * This margin is stripped '''before''' the arguments are interpolated into the string.
   *
   * String escape sequences are '''not''' processed; this interpolator is designed to
   * be used with triple quoted Strings.
   *
   * {{{
   * scala> val foo = "f|o|o"
   * foo: String = f|o|o
   * scala> sm"""|${foo}|
   *             |"""
   * res0: String =
   * "f|o|o|
   * "
   * }}}
   */
  final def sm(args: Any*): String = impl('|', args: _*)

  private final def impl(sep: Char, args: Any*): String = {
    def isLineBreak(c: Char) = c == '\n' || c == '\f' // compatible with StringOps#isLineBreak
    def stripTrailingPart(s: String) = {
      val (pre, post) = s.span(c => !isLineBreak(c))
      pre + post.stripMargin(sep)
    }
    val stripped: List[String] = stringContext.parts.toList match {
      case head :: tail => head.stripMargin(sep) :: tail.map(stripTrailingPart)
      case Nil => Nil
    }
    new StringContext(stripped: _*).raw(args: _*)
  }

  /** Like the `sm` interpolator, but strips quotation-style delimiter `>`
   *  and merges the resulting lines into a single line string.
   */
  final def sq(args: Any*): String = impl('>', args: _*).linesIterator.mkString
}
