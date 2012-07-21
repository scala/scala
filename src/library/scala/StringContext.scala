/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2012, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** A class to support string interpolation.
 *  This class supports string interpolation as outlined in Scala SIP-11.
 *  It needs to be fully documented once the SIP is accepted.
 *
 *  @param   parts  The parts that make up the interpolated string,
 *                  without the expressions that get inserted by interpolation.
 */
case class StringContext(parts: String*) {

  import StringContext._

  /** Checks that the given arguments `args` number one less than the number
   *  of `parts` supplied to the enclosing `StringContext`.
   *  @param `args` The arguments to be checked.
   *  @throws An `IllegalArgumentException` if this is not the case.
   */
  def checkLengths(args: Seq[Any]): Unit =
    if (parts.length != args.length + 1)
      throw new IllegalArgumentException("wrong number of arguments for interpolated string")


  /** The simple string interpolator.
   *
   *  It inserts its arguments between corresponding parts of the string context.
   *  It also treats standard escape sequences as defined in the Scala specification.
   *  @param `args` The arguments to be inserted into the resulting string.
   *  @throws An `IllegalArgumentException`
   *          if the number of `parts` in the enclosing `StringContext` does not exceed
   *          the number of arguments `arg` by exactly 1.
   *  @throws A `StringContext.InvalidEscapeException` if if a `parts` string contains a backslash (`\`) character
   *          that does not start a valid escape sequence.
   */
  def s(args: Any*): String = standardInterpolator(treatEscapes, args)

  /** The raw string interpolator.
   *
   *  It inserts its arguments between corresponding parts of the string context.
   *  As opposed to the simple string interpolator `s`, this one does not treat
   *  standard escape sequences as defined in the Scala specification.
   *  @param `args` The arguments to be inserted into the resulting string.
   *  @throws An `IllegalArgumentException`
   *          if the number of `parts` in the enclosing `StringContext` does not exceed
   *          the number of arguments `arg` by exactly 1.
   *  @throws A `StringContext.InvalidEscapeException` if if a `parts` string contains a backslash (`\`) character
   *          that does not start a valid escape sequence.
   */
  def raw(args: Any*): String = standardInterpolator(identity, args)

  def standardInterpolator(process: String => String, args: Seq[Any]): String = {
    checkLengths(args)
    val pi = parts.iterator
    val ai = args.iterator
    val bldr = new java.lang.StringBuilder(process(pi.next()))
    while (ai.hasNext) {
      bldr append ai.next
      bldr append treatEscapes(pi.next())
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
   *  @param `args` The arguments to be inserted into the resulting string.
   *  @throws An `IllegalArgumentException`
   *          if the number of `parts` in the enclosing `StringContext` does not exceed
   *          the number of arguments `arg` by exactly 1.
   *  @throws A `StringContext.InvalidEscapeException` if a `parts` string contains a backslash (`\`) character
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
   *   2. Any `%` characters not in formatting positions are left in the resulting
   *      string literally. This is achieved by replacing each such occurrence by the
   *      format specifier `%%`.
   */
  // The implementation is magically hardwired into `scala.tools.reflect.MacroImplementations.macro_StringInterpolation_f`
  def f(args: Any*): String = ??? // macro
}

object StringContext {

  /** An exception that is thrown if a string contains a backslash (`\`) character that
   *  that does not start a valid escape sequence.
   *  @param  str   The offending string
   *  @param  idx   The index of the offending backslash character in `str`.
   */
  class InvalidEscapeException(str: String, idx: Int)
    extends IllegalArgumentException("invalid escape character at index "+idx+" in \""+str+"\"")

  /** Expands standard Scala escape sequences in a string.
   *  Escape sequences are:
   *   control: `\b`, `\t`, `\n`, `\f`, `\r`
   *   escape:  `\\`, `\"`, `\'`
   *   octal:   `\d` `\dd` `\ddd` where `d` is an octal digit between `0` and `7`.
   *
   *  @param  str  A string that may contain escape sequences
   *  @return The string with all escape sequences expanded.
   */
  def treatEscapes(str: String): String = {
    lazy val bldr = new java.lang.StringBuilder
    val len = str.length
    var start = 0
    var cur = 0
    var idx = 0
    def output(ch: Char) = {
      bldr append str.substring (start, cur)
      bldr append ch
      start = idx
    }
    while (idx < len) {
      cur = idx
      if (str(idx) == '\\') {
        idx += 1
        if ('0' <= str(idx) && str(idx) <= '7') {
          val leadch = str(idx)
          var oct = leadch - '0'
          idx += 1
          if ('0' <= str(idx) && str(idx) <= '7') {
            oct = oct * 8 + str(idx) - '0'
            idx += 1
            if (leadch <= '3' && '0' <= str(idx) && str(idx) <= '7') {
              oct = oct * 8 + str(idx) - '0'
              idx += 1
            }
          }
          output(oct.toChar)
        } else {
          val ch = str(idx)
          idx += 1
          output {
            ch match {
              case 'b'  => '\b'
              case 't'  => '\t'
              case 'n'  => '\n'
              case 'f'  => '\f'
              case 'r'  => '\r'
              case '\"' => '\"'
              case '\'' => '\''
              case '\\' => '\\'
              case _    => throw new InvalidEscapeException(str, cur)
            }
          }
        }
      } else  {
        idx += 1
      }
    }
    if (start == 0) str
    else (bldr append str.substring(start, idx)).toString
  }
}
