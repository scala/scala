/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Parsers.scala 12357 2007-07-18 21:55:08Z moors $

package scala.util.parsing.combinator

import java.util.regex.Pattern
import scala.util.matching.Regex
import scala.util.parsing.input.CharSequenceReader

trait RegexParsers extends Parsers {

  type Elem = Char

  var skipWhitespace = true

  private val whiteSpacePat = Pattern compile """\s+"""

  private def handleWhiteSpace(source: CharSequence, offset: Int): Int = {
    var start = offset
    if (skipWhitespace) {
      val wsm = whiteSpacePat.matcher(source.subSequence(offset, source.length))
      if (wsm.lookingAt) start += wsm.end
    }
    start
  }

  /** A parser that matches a literal string */
  implicit def literal(s: String): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      var i = 0
      var j = start
      while (i < s.length && j < source.length && s.charAt(i) == source.charAt(j)) {
        i += 1
        j += 1
      }
      if (i == s.length)
        Success(source.subSequence(start, j).toString, in.drop(j - offset))
      else
        Failure("`"+s+"' expected", in.drop(start - offset))
    }
  }

  /** A parser that matches a regex string */
  implicit def regex(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      val pm = r.pattern.matcher(source.subSequence(start, source.length))
      if (pm.lookingAt)
        Success(source.subSequence(start, start + pm.end).toString,
                in.drop(start + pm.end - offset))
      else
        Failure("string matching regex `"+r.regex+"' expected", in.drop(start - offset))
    }
  }

  /** Parse some prefix of character sequence `in' with parser `p' */
  def parse[T](p: Parser[T])(in: CharSequence): ParseResult[T] =
    p(new CharSequenceReader(in))

  /** Parse all of character sequence `in' with parser `p' */
  def parseAll[T](p: Parser[T])(in: CharSequence): ParseResult[T] =
    parse(phrase(p))(in)
}
