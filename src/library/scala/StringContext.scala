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

import java.lang.{ StringBuilder => JLSBuilder }
import scala.annotation.tailrec

/** This class provides the basic mechanism to do String Interpolation.
 * String Interpolation allows users
 * to embed variable references directly in *processed* string literals.
 * Here's an example:
 * {{{
 *   val name = "James"
 *   println(s"Hello, \$name")  // Hello, James
 * }}}
 *
 * Any processed string literal is rewritten as an instantiation and
 * method call against this class.   For example:
 * {{{
 *   s"Hello, \$name"
 * }}}
 *
 * is rewritten to be:
 *
 * {{{
 *   StringContext("Hello, ", "").s(name)
 * }}}
 *
 * By default, this class provides the `raw`, `s` and `f` methods as
 * available interpolators.
 *
 * To provide your own string interpolator, create an implicit class
 * which adds a method to `StringContext`.  Here's an example:
 * {{{
 *    implicit class JsonHelper(private val sc: StringContext) extends AnyVal {
 *      def json(args: Any*): JSONObject = ...
 *    }
 *    val x: JSONObject = json"{ a: \$a }"
 * }}}
 *
 *  Here the `JsonHelper` extension class implicitly adds the `json` method to
 *  `StringContext` which can be used for `json` string literals.
 *
 *  @since 2.10.0
 *  @param   parts  The parts that make up the interpolated string,
 *                  without the expressions that get inserted by interpolation.
 */
case class StringContext(parts: String*) {

  import StringContext._

  /** Checks that the length of the given argument `args` is one less than the number
   *  of `parts` supplied to the enclosing `StringContext`.
   *  @param `args` The arguments to be checked.
   *  @throws IllegalArgumentException  if this is not the case.
   */
  def checkLengths(args: Seq[Any]): Unit =
    if (parts.length != args.length + 1)
      throw new IllegalArgumentException("wrong number of arguments ("+ args.length
        +") for interpolated string with "+ parts.length +" parts")


  /** The simple string interpolator.
   *
   *  It inserts its arguments between corresponding parts of the string context.
   *  It also treats standard escape sequences as defined in the Scala specification.
   *  Here's an example of usage:
   *  {{{
   *    val name = "James"
   *    println(s"Hello, \$name")  // Hello, James
   *  }}}
   *  In this example, the expression \$name is replaced with the `toString` of the
   *  variable `name`.
   *  The `s` interpolator can take the `toString` of any arbitrary expression within
   *  a `\${}` block, for example:
   *  {{{
   *    println(s"1 + 1 = \${1 + 1}")
   *  }}}
   *  will print the string `1 + 1 = 2`.
   *
   *  @param `args` The arguments to be inserted into the resulting string.
   *  @throws IllegalArgumentException
   *          if the number of `parts` in the enclosing `StringContext` does not exceed
   *          the number of arguments `arg` by exactly 1.
   *  @throws StringContext.InvalidEscapeException
   *          if a `parts` string contains a backslash (`\`) character
   *          that does not start a valid escape sequence.
   *  @note   The Scala compiler may replace a call to this method with an equivalent, but more efficient,
   *          use of a StringBuilder.
   */
  def s(args: Any*): String = standardInterpolator(treatEscapes, args)

  /** The raw string interpolator.
   *
   *  It inserts its arguments between corresponding parts of the string context.
   *  As opposed to the simple string interpolator `s`, this one does not treat
   *  standard escape sequences as defined in the Scala specification.
   *
   *  For example, the raw processed string `raw"a\nb"` is equal to the scala string `"a\\nb"`.
   *
   *  ''Note:'' Even when using the raw interpolator, Scala will preprocess unicode escapes.
   *  For example:
   *  {{{
   *    scala> raw"\u005cu0023"
   *    res0: String = #
   *  }}}
   *
   *  @param `args` The arguments to be inserted into the resulting string.
   *  @throws IllegalArgumentException
   *          if the number of `parts` in the enclosing `StringContext` does not exceed
   *          the number of arguments `arg` by exactly 1.
   *  @note   The Scala compiler may replace a call to this method with an equivalent, but more efficient,
   *          use of a StringBuilder.
   */
  def raw(args: Any*): String = standardInterpolator(identity, args)

  def standardInterpolator(process: String => String, args: Seq[Any]): String = {
    checkLengths(args)
    val pi = parts.iterator
    val ai = args.iterator
    val bldr = new JLSBuilder(process(pi.next()))
    while (ai.hasNext) {
      bldr append ai.next
      bldr append process(pi.next())
    }
    bldr.toString
  }

  /** The formatted string interpolator.
   *
   *  It inserts its arguments between corresponding parts of the string context.
   *  It also treats standard escape sequences as defined in the Scala specification.
   *  Finally, if an interpolated expression is followed by a `parts` string
   *  that starts with a formatting specifier, the expression is formatted according to that
   *  specifier. All specifiers allowed in Java format strings are handled, and in the same
   *  way they are treated in Java.
   *
   *  For example:
   *  {{{
   *    val height = 1.9d
   *    val name = "James"
   *    println(f"\$name%s is \$height%2.2f meters tall")  // James is 1.90 meters tall
   *  }}}
   *
   *  @param `args` The arguments to be inserted into the resulting string.
   *  @throws IllegalArgumentException
   *          if the number of `parts` in the enclosing `StringContext` does not exceed
   *          the number of arguments `arg` by exactly 1.
   *  @throws StringContext.InvalidEscapeException
   *          if a `parts` string contains a backslash (`\`) character
   *          that does not start a valid escape sequence.
   *
   *  Note: The `f` method works by assembling a format string from all the `parts` strings and using
   *  `java.lang.String.format` to format all arguments with that format string. The format string is
   *  obtained by concatenating all `parts` strings, and performing two transformations:
   *
   *   1. Let a _formatting position_ be a start of any `parts` string except the first one.
   *      If a formatting position does not refer to a `%` character (which is assumed to
   *      start a format specifier), then the string format specifier `%s` is inserted.
   *
   *   2. Any `%` characters not in formatting positions must begin one of the conversions
   *      `%%` (the literal percent) or `%n` (the platform-specific line separator).
   */
  // The implementation is hardwired to `scala.tools.reflect.MacroImplementations.macro_StringInterpolation_f`
  // Using the mechanism implemented in `scala.tools.reflect.FastTrack`
  def f[A >: Any](args: A*): String = macro ???
}

object StringContext {

  /** An exception that is thrown if a string contains a backslash (`\`) character
   *  that does not start a valid escape sequence.
   *  @param  str   The offending string
   *  @param  index   The index of the offending backslash character in `str`.
   */
  class InvalidEscapeException(str: String, @deprecatedName('idx) val index: Int) extends IllegalArgumentException(
    s"""invalid escape ${
      require(index >= 0 && index < str.length)
      val ok = """[\b, \t, \n, \f, \r, \\, \", \']"""
      if (index == str.length - 1) "at terminal" else s"'\\${str(index + 1)}' not one of $ok at"
    } index $index in "$str". Use \\\\ for literal \\."""
  )

  /** Expands standard Scala escape sequences in a string.
   *  Escape sequences are:
   *   control: `\b`, `\t`, `\n`, `\f`, `\r`
   *   escape:  `\\`, `\"`, `\'`
   *   octal:   `\d` `\dd` `\ddd` where `d` is an octal digit between `0` and `7`.
   *
   *  @param  str  A string that may contain escape sequences
   *  @return The string with all escape sequences expanded.
   */
  def treatEscapes(str: String): String = treatEscapes0(str, strict = false)

  /** Treats escapes, but disallows octal escape sequences. */
  def processEscapes(str: String): String = treatEscapes0(str, strict = true)

  private def treatEscapes0(str: String, strict: Boolean): String = {
    val len = str.length
    // replace escapes with given first escape
    def replace(first: Int): String = {
      val b = new JLSBuilder
      // append replacement starting at index `i`, with `next` backslash
      @tailrec def loop(i: Int, next: Int): String = {
        if (next >= 0) {
          //require(str(next) == '\\')
          if (next > i) b.append(str, i, next)
          var idx = next + 1
          if (idx >= len) throw new InvalidEscapeException(str, next)
          val c = str(idx) match {
            case 'b'  => '\b'
            case 't'  => '\t'
            case 'n'  => '\n'
            case 'f'  => '\f'
            case 'r'  => '\r'
            case '"'  => '"'
            case '\'' => '\''
            case '\\' => '\\'
            case o if '0' <= o && o <= '7' =>
              if (strict) throw new InvalidEscapeException(str, next)
              val leadch = str(idx)
              var oct = leadch - '0'
              idx += 1
              if (idx < len && '0' <= str(idx) && str(idx) <= '7') {
                oct = oct * 8 + str(idx) - '0'
                idx += 1
                if (idx < len && leadch <= '3' && '0' <= str(idx) && str(idx) <= '7') {
                  oct = oct * 8 + str(idx) - '0'
                  idx += 1
                }
              }
              idx -= 1   // retreat
              oct.toChar
            case _    => throw new InvalidEscapeException(str, next)
          }
          idx += 1       // advance
          b append c
          loop(idx, str.indexOf('\\', idx))
        } else {
          if (i < len) b.append(str, i, len)
          b.toString
        }
      }
      loop(0, first)
    }
    str indexOf '\\' match {
      case -1 => str
      case  i => replace(i)
    }
  }
}
