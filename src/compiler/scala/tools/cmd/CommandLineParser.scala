/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package cmd

import scala.annotation.tailrec

/** A simple (overly so) command line parser.
 *  !!! This needs a thorough test suite to make sure quoting is
 *  done correctly and portably.
 */
object CommandLineParser {
  // splits a string into a quoted prefix and the rest of the string,
  // taking escaping into account (using \)
  // `"abc"def` will match as `DoubleQuoted(abc, def)`
  private class QuotedExtractor(quote: Char) {
    def unapply(in: String): Option[(String, String)] = {
      val del = quote.toString
      if (in startsWith del) {
        var escaped = false
        val (quoted, next) = (in substring 1) span {
          case `quote` if !escaped => false
          case '\\'    if !escaped => escaped = true; true
          case _                   => escaped = false; true
        }
        // the only way to get out of the above loop is with an empty next or !escaped
        // require(next.isEmpty || !escaped)
        if (next startsWith del) Some((quoted, next substring 1))
        else None
      } else None
    }
  }
  private object DoubleQuoted extends QuotedExtractor('"')
  private object SingleQuoted extends QuotedExtractor('\'')
  private val Word = """(\S+)(.*)""".r

  // parse `in` for an argument, return it and the remainder of the input (or an error message)
  // (argument may be in single/double quotes, taking escaping into account, quotes are stripped)
  private def argument(in: String): Either[String, (String, String)] = in match {
    case DoubleQuoted(arg, rest) => Right((arg, rest))
    case SingleQuoted(arg, rest) => Right((arg, rest))
    case Word(arg, rest)         => Right((arg, rest))
    case _                       => Left(s"Illegal argument: $in")
  }

  // parse a list of whitespace-separated arguments (ignoring whitespace in quoted arguments)
  @tailrec private def commandLine(in: String, accum: List[String] = Nil): Either[String, (List[String], String)] = {
    val trimmed = in.trim
    if (trimmed.isEmpty) Right((accum.reverse, ""))
    else argument(trimmed) match {
      case Right((arg, next)) =>
        (next span Character.isWhitespace) match {
          case("", rest) if rest.nonEmpty => Left("Arguments should be separated by whitespace.") // TODO: can this happen?
          case(ws, rest)                  => commandLine(rest, arg :: accum)
        }
      case Left(msg) => Left(msg)
    }
  }

  class ParseException(msg: String) extends RuntimeException(msg)

  def tokenize(line: String): List[String] = tokenize(line, x => throw new ParseException(x))
  def tokenize(line: String, errorFn: String => Unit): List[String] = {
    commandLine(line) match {
      case Right((args, _)) => args
      case Left(msg)        => errorFn(msg) ; Nil
    }
  }
}
