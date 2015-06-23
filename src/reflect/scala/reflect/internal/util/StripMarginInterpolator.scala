package scala
package reflect
package internal
package util

trait StripMarginInterpolator {
  def stringContext: StringContext

  /**
   * A safe combination of [[scala.collection.immutable.StringLike#stripMargin]]
   * and [[scala.StringContext#raw]].
   *
   * The margin of each line is defined by whitespace leading up to a '|' character.
   * This margin is stripped '''before''' the arguments are interpolated into to string.
   *
   * String escape sequences are '''not''' processed; this interpolator is designed to
   * be used with triple quoted Strings.
   *
   * {{{
   * scala> val foo = "f|o|o"
   * foo: String = f|o|o
   * scala> sm"""|${foo}
   *             |"""
   * res0: String =
   * "f|o|o
   * "
   * }}}
   */
  final def sm(args: Any*): String = {
    def isLineBreak(c: Char) = c == '\n' || c == '\f' // compatible with StringLike#isLineBreak
    def stripTrailingPart(s: String) = {
      val (pre, post) = s.span(c => !isLineBreak(c))
      pre + post.stripMargin
    }
    val stripped: List[String] = stringContext.parts.toList match {
      case head :: tail => head.stripMargin :: (tail map stripTrailingPart)
      case Nil => Nil
    }
    new StringContext(stripped: _*).raw(args: _*)
  }
}
