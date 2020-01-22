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
 *  @param   parts  The parts that make up the interpolated string,
 *                  without the expressions that get inserted by interpolation.
 */
case class StringContext(parts: String*) {

  import StringContext.{checkLengths => scCheckLengths, glob, standardInterpolator => scStandardInterpolator}

  @deprecated("use same-named method on StringContext companion object", "2.13.0")
  def checkLengths(args: scala.collection.Seq[Any]): Unit = scCheckLengths(args, parts)

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
  def s(args: Any*): String = macro ??? // fasttracked to scala.tools.reflect.FastStringInterpolator::interpolateS

  /** The strict string interpolator.
    *
    *  It inserts its arguments of type String between corresponding parts of the string context.
    *  It also treats standard escape sequences as defined in the Scala specification.
    *  Here's an example of usage:
    *  {{{
    *    val name = "James"
    *    println(ss"Hello, \$name")  // Hello, James
    *  }}}
    *  In this example, the expression \$name is replaced with the value of the
    *  variable `name`.
    *  The `ss` interpolator accepts only Strings or any arbitrary expression of type String within
    *  a `\${}` block, for example:
    *  {{{
    *    val list = List(1, 2, 3)
    *    println(ss"list = \${list.mkString(",")}")
    *  }}}
    *  will print the string `list = 1,2,3`.
    *
    *  Note that it won't accept any non-string values or expressions, for example:
    *  {{{
    *    println(ss"1 + 1 = \${1 + 1}")
    *  }}}
    *  will not compile, for this use case either use
    *  {{{
    *    println(s"1 + 1 = \${1 + 1}")
    *  }}}
    *  or explicitly convert to String
    *  {{{
    *    println(ss"1 + 1 = \${(1 + 1).toString}")
    *  }}}
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
  def ss(args: String*): String = s(args: _*)

  object s {
    /** The simple string matcher.
     *
     *  Attempts to match the input string to the given interpolated patterns via 
     *  a naive globbing, that is the reverse of the simple interpolator.
     *
     *  Here is an example usage:
     *
     *  {{{
     *    val s"Hello, \$name" = "Hello, James"
     *    println(name)  // "James"
     *  }}}
     *
     *  In this example, the string "James" ends up matching the location where the pattern
     *  `\$name` is positioned, and thus ends up bound to that variable.
     *
     *  Multiple matches are supported:
     *
     *  {{{
     *    val s"\$greeting, \$name" = "Hello, James"
     *    println(greeting)  // "Hello"
     *    println(name)  // "James"
     *  }}}
     *
     *  And the `s` matcher can match an arbitrary pattern within the `\${}` block, for example:
     *
     *  {{{
     *    val TimeSplitter = "([0-9]+)[.:]([0-9]+)".r
     *    val s"The time is \${TimeSplitter(hours, mins)}" = "The time is 10.50"
     *    println(hours) // 10
     *    println(mins) // 50
     *  }}}
     *
     *  Here, we use the `TimeSplitter` regex within the `s` matcher, further splitting the
     *  matched string "10.50" into its constituent parts
     */
    def unapplySeq(s: String): Option[Seq[String]] = glob(parts, s)
  }
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
  def raw(args: Any*): String = macro ??? // fasttracked to scala.tools.reflect.FastStringInterpolator::interpolateRaw

  @deprecated("Use the static method StringContext.standardInterpolator instead of the instance method", "2.13.0")
  def standardInterpolator(process: String => String, args: Seq[Any]): String = scStandardInterpolator(process, args, parts)

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
  def f[A >: Any](args: A*): String = macro ??? // fasttracked to scala.tools.reflect.FormatInterpolator::interpolateF
}

object StringContext {
  /**
    * Linear time glob-matching implementation.
    * Adapted from https://research.swtch.com/glob
    *
    * @param patternChunks The non-wildcard portions of the input pattern,
    *                      separated by wildcards
    * @param input The input you wish to match against
    * @return None if there is no match, Some containing the sequence of matched
    *         wildcard strings if there is a match 
    */
  def glob(patternChunks: Seq[String], input: String): Option[Seq[String]] = {
    var patternIndex = 0
    var inputIndex = 0
    var nextPatternIndex = 0
    var nextInputIndex = 0

    val numWildcards = patternChunks.length - 1
    val matchStarts = Array.fill(numWildcards)(-1)
    val matchEnds = Array.fill(numWildcards)(-1)

    val nameLength = input.length
    // The final pattern is as long as all the chunks, separated by 1-character
    // glob-wildcard placeholders
    val patternLength = {
      var n = numWildcards
      for(chunk <- patternChunks) {
        n += chunk.length
      }
      n
    }

    // Convert the input pattern chunks into a single sequence of shorts; each
    // non-negative short represents a character, while -1 represents a glob wildcard
    val pattern = {
      val arr = new Array[Short](patternLength)
      var i = 0
      var first = true
      for(chunk <- patternChunks) {
        if (first) first = false
        else {
          arr(i) = -1
          i += 1
        }
        for(c <- chunk) {
          arr(i) = c.toShort
          i += 1
        }
      }
      arr
    }

    // Lookup table for each character in the pattern to check whether or not
    // it refers to a glob wildcard; a non-negative integer indicates which
    // glob wildcard it represents, while -1 means it doesn't represent any
    val matchIndices = {
      val arr = Array.fill(patternLength + 1)(-1)
      var i = 0
      var j = 0
      for(chunk <- patternChunks) {
        if (j < numWildcards) {
          i += chunk.length
          arr(i) = j
          i += 1
          j += 1
        }
      }
      arr
    }

    while(patternIndex < patternLength || inputIndex < nameLength) {
      matchIndices(patternIndex) match {
        case -1 => // do nothing
        case n =>
          matchStarts(n) = matchStarts(n) match {
            case -1 => inputIndex
            case s => math.min(s, inputIndex)
          }
          matchEnds(n) = matchEnds(n) match {
            case -1 => inputIndex
            case s => math.max(s, inputIndex)
          }
      }

      val continue = if (patternIndex < patternLength) {
        val c = pattern(patternIndex)
        c match {
          case -1 =>  // zero-or-more-character wildcard
            // Try to match at nx. If that doesn't work out, restart at nx+1 next.
            nextPatternIndex = patternIndex
            nextInputIndex = inputIndex + 1
            patternIndex += 1
            true
          case _ => // ordinary character
            if (inputIndex < nameLength && input(inputIndex) == c) {
              patternIndex += 1
              inputIndex += 1
              true
            } else {
              false
            }
        }
      } else false

      // Mismatch. Maybe restart.
      if (!continue) {
        if (0 < nextInputIndex && nextInputIndex <= nameLength) {
          patternIndex = nextPatternIndex
          inputIndex = nextInputIndex
        } else {
          return None
        }
      }
    }

    // Matched all of pattern to all of name. Success.
    Some(collection.immutable.ArraySeq.unsafeWrapArray(
      Array.tabulate(patternChunks.length - 1)(n => input.slice(matchStarts(n), matchEnds(n)))
    ))
  }

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
  )

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

  /** Expands standard Scala escape sequences in a string.
   *  Escape sequences are:
   *   control: `\b`, `\t`, `\n`, `\f`, `\r`
   *   escape:  `\\`, `\"`, `\'`
   *
   *  @param  str  A string that may contain escape sequences
   *  @return The string with all escape sequences expanded.
   */
  def processEscapes(str: String): String = {
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

  def standardInterpolator(process: String => String, args: scala.collection.Seq[Any], parts: Seq[String]): String = {
    StringContext.checkLengths(args, parts)
    val pi = parts.iterator
    val ai = args.iterator
    val bldr = new JLSBuilder(process(pi.next()))
    while (ai.hasNext) {
      bldr append ai.next()
      bldr append process(pi.next())
    }
    bldr.toString
  }

  /** Checks that the length of the given argument `args` is one less than the number
   *  of `parts` supplied to the `StringContext`.
   *
   *  @throws IllegalArgumentException  if this is not the case.
   */
  def checkLengths(args: scala.collection.Seq[Any], parts: Seq[String]): Unit =
    if (parts.length != args.length + 1)
      throw new IllegalArgumentException("wrong number of arguments ("+ args.length
        +") for interpolated string with "+ parts.length +" parts")

}
