/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import java.lang.{ StringBuilder => JLSBuilder }
import scala.annotation.tailrec

/** This class provides the basic mechanism to do String Interpolation.
 * String Interpolation allows users
 * to embed variable references directly in *processed* string literals.
 * Here's an example:
 * {{{
 *   val name = "James"
 *   println(s"Hello, $name")  // Hello, James
 * }}}
 *
 * Any processed string literal is rewritten as an instantiation and
 * method call against this class.   For example:
 * {{{
 *   s"Hello, $name"
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
 *    val x: JSONObject = json"{ a: $a }"
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
  def checkLengths(args: scala.collection.Seq[Any]): Unit =
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
   *    println(s"Hello, $name")  // Hello, James
   *  }}}
   *  In this example, the expression $name is replaced with the `toString` of the
   *  variable `name`.
   *  The `s` interpolator can take the `toString` of any arbitrary expression within
   *  a `${}` block, for example:
   *  {{{
   *    println(s"1 + 1 = ${1 + 1}")
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
  def s(args: Any*): String = standardInterpolator(processEscapes, args)

  /** The raw string interpolator.
   *
   *  It inserts its arguments between corresponding parts of the string context.
   *  As opposed to the simple string interpolator `s`, this one does not treat
   *  standard escape sequences as defined in the Scala specification.
   *
   *  For example, the raw processed string `raw"a\nb"` is equal to the scala string `"a\\nb"`.
   *
   *  ''Note:'' Even when using the raw interpolator, Scala will process unicode escapes.
   *  For example:
   *  {{{
   *    scala> raw"\u005cu0023"
   *    res0: String = #
   *  }}}
   *
   *  Processing unicode escapes in raw interpolations is deprecated and will be removed
   *  in a future version of scala.
   *
   *  @param `args` The arguments to be inserted into the resulting string.
   *  @throws IllegalArgumentException
   *          if the number of `parts` in the enclosing `StringContext` does not exceed
   *          the number of arguments `arg` by exactly 1.
   *  @note   The Scala compiler may replace a call to this method with an equivalent, but more efficient,
   *          use of a StringBuilder.
   */
  def raw(args: Any*): String = standardInterpolator(processUnicodeEscapes, args)

  def standardInterpolator(process: String => String, args: scala.collection.Seq[Any]): String = {
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
   *    println(f"$name%s is $height%2.2f meters tall")  // James is 1.90 meters tall
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
  class InvalidEscapeException(str: String, val index: Int) extends IllegalArgumentException(
    s"""invalid escape ${
      require(index >= 0 && index < str.length)
      val ok = """[\b, \t, \n, \f, \r, \\, \", \']"""
      if (index == str.length - 1) "at terminal" else s"'\\${str(index + 1)}' not one of $ok at"
    } index $index in "$str". Use \\\\ for literal \\."""
  )//"""" //remove quotes


  private[this] final def muee(str: String, index: Int): String = {
    require(index >= 0 && index < str.length)
    val ok = """[\b, \t, \n, \f, \r, \\, \", \']"""
    val msg = if (index == str.length - 1) "at terminal" else s"'\${str(index + 1)}' not a valid hex digit at"
    def q(str: String) = "\""  + str + "\""
    s"malformed unicode escape $msg index $index in ${q(str)}"
  }

  class MalformedUnicodeEscapeException(str: String, val index: Int) extends IllegalArgumentException(muee(str, index))

  /** Expands standard Scala escape sequences in a string.
   *  Escape sequences are:
   *   control: `\b`, `\t`, `\n`, `\f`, `\r`
   *   escape:  `\\`, `\"`, `\'`
   *
   *  @param  str  A string that may contain escape sequences
   *  @return The string with all escape sequences expanded.
   */
  @deprecated("use processEscapes", "2.13.0")
  def treatEscapes(str: String): String = processEscapes(str)

  def processUnicodeEscapes(str: String): String = {
    val len = str.length
    // replace escapes with given first escape
    def replace(first: Int): String = {
      val b = new JLSBuilder(str.length)

      def processUEscape(afterFirstU: Int): (Char, Int) = {
        def uDigitAt(index: Int): Int = {
          if(index >= str.length) -1
          else {
            val ch = str(index)
            if(ch >= '0' && ch <= '9') ch - '0'
            else if(ch >= 'a' && ch <= 'f') 10 + ch - 'a'
            else if(ch >= 'A' && ch <= 'F') 10 + ch - 'A'
            else -1
          }
        }

        var numberIndex = afterFirstU
        var codepoint = 0

        def error = throw new MalformedUnicodeEscapeException(str, numberIndex)

        while(numberIndex < str.length && str(numberIndex) == 'u') numberIndex += 1

        var digit = uDigitAt(numberIndex)
        if(digit >= 0){
          codepoint = 0x1000 * digit
          numberIndex += 1
          digit = uDigitAt(numberIndex)
          if(digit >= 0) {
            codepoint += 0x100 * digit
            numberIndex += 1
            digit = uDigitAt(numberIndex)
            if(digit >= 0) {
              codepoint += 0x10 * digit
              numberIndex += 1
              digit = uDigitAt(numberIndex)
              if(digit >= 0) {
                codepoint += digit
                numberIndex += 1
                (codepoint.asInstanceOf[Char], numberIndex - afterFirstU + 1)
              } else error
            } else error
          } else error
        } else error
      }

      @tailrec def loop(i: Int, nextBackslash: Int): String = {
        if (nextBackslash >= 0) {
          if (nextBackslash > i) b.append(str, i, nextBackslash)
          val idx = nextBackslash + 1
          if (idx >= len) throw new InvalidEscapeException(str, nextBackslash)
          val (c, consumed) = str(idx) match {
            case 'u'  => processUEscape(idx + 1)
            case _    => throw new InvalidEscapeException(str, nextBackslash)
          }
          b append c
          loop(idx + consumed, str.indexOf("\\u", idx + consumed))
        } else {
          if (i < len) b.append(str, i, len)
          b.toString
        }
      }
      loop(0, first)
    }
    str indexOf "\\u" match {
      case -1 => str
      case  i => replace(i)
    }
  }

  /** Expands standard Scala escape sequences in a string.
   *  Escape sequences are:
   *   control: `\b`, `\t`, `\n`, `\f`, `\r`
   *   escape:  `\\`, `\"`, `\'`
   *   unicode: `\ u(u*)hexdigit{4}`
   *
   *  @param  str  A string that may contain escape sequences
   *  @return The string with all escape sequences expanded.
   */
  def processEscapes(str: String): String = {
    val len = str.length
    // replace escapes with given first escape
    def replace(first: Int): String = {
      val b = new JLSBuilder(str.length)
      // append replacement starting at index `i`, with `next` backslash

      def processUEscape(afterFirstU: Int): (Char, Int) = {
        def uDigitAt(index: Int): Int = {
          if(index >= str.length) -1
          else {
            val ch = str(index)
            if(ch >= '0' && ch <= '9') ch - '0'
            else if(ch >= 'a' && ch <= 'f') 10 + ch - 'a'
            else if(ch >= 'A' && ch <= 'F') 10 + ch - 'A'
            else -1
          }
        }

        var numberIndex = afterFirstU
        def error = throw new MalformedUnicodeEscapeException(str, numberIndex)
        var codepoint = 0
        while(numberIndex < str.length && str(numberIndex) == 'u') numberIndex += 1

        var digit = uDigitAt(numberIndex)
        if(digit >= 0){
          codepoint = 0x1000 * digit
          numberIndex += 1
          digit = uDigitAt(numberIndex)
          if(digit >= 0) {
            codepoint += 0x100 * digit
            numberIndex += 1
            digit = uDigitAt(numberIndex)
            if(digit >= 0) {
              codepoint += 0x10 * digit
              numberIndex += 1
              digit = uDigitAt(numberIndex)
              if(digit >= 0) {
                codepoint += digit
                numberIndex += 1
                (codepoint.asInstanceOf[Char], numberIndex - afterFirstU + 1)
              } else error
            } else error
          } else error
        } else error
      }

      @tailrec def loop(i: Int, nextBackslash: Int): String = {
        if (nextBackslash >= 0) {
          //require(str(next) == '\\')
          if (nextBackslash > i) b.append(str, i, nextBackslash)
          val idx = nextBackslash + 1
          if (idx >= len) throw new InvalidEscapeException(str, nextBackslash)
          val (c, consumed) = str(idx) match {
            case 'b'  => ('\b', 1)
            case 't'  => ('\t', 1)
            case 'n'  => ('\n', 1)
            case 'f'  => ('\f', 1)
            case 'r'  => ('\r', 1)
            case '"'  => ('"', 1)
            case '\'' => ('\'', 1)
            case '\\' => ('\\', 1)
            case 'u'  => processUEscape(idx + 1)
            case _    => throw new InvalidEscapeException(str, nextBackslash)
          }
          b append c
          loop(idx + consumed, str.indexOf('\\', idx + consumed))
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
