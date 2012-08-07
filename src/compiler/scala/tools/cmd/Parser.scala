/* NEST (New Scala Test)
 * Copyright 2007-2012 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package cmd

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharArrayReader.EofCh

/** A simple (overly so) command line parser.
 *  !!! This needs a thorough test suite to make sure quoting is
 *  done correctly and portably.
 */
trait ParserUtil extends Parsers {
  class ParserPlus[+T](underlying: Parser[T]) {
    def !~>[U](p: => Parser[U]): Parser[U] = (underlying ~! p) ^^ { case a~b  => b }
    def <~![U](p: => Parser[U]): Parser[T] = (underlying ~! p) ^^ { case a~b  => a }
  }
  protected implicit def parser2parserPlus[T](p: Parser[T]): ParserPlus[T] = new ParserPlus(p)
}

object Parser extends RegexParsers with ParserUtil {
  override def skipWhitespace = false

  def elemExcept(xs: Elem*): Parser[Elem] = elem("elemExcept", x => x != EofCh && !(xs contains x))
  def elemOf(xs: Elem*): Parser[Elem]     = elem("elemOf", xs contains _)
  def escaped(ch: Char): Parser[String] = "\\" + ch
  def mkQuoted(ch: Char): Parser[String] = (
      elem(ch) !~> rep(escaped(ch) | elemExcept(ch)) <~ ch ^^ (_.mkString)
    | failure("Unmatched %s in input." format ch)
  )

  /** Apparently windows can't deal with the quotes sticking around. */
  lazy val squoted: Parser[String] = mkQuoted('\'')   // ^^ (x => "'%s'" format x)
  lazy val dquoted: Parser[String] = mkQuoted('"')    // ^^ (x => "\"" + x + "\"")
  lazy val token: Parser[String]   = """\S+""".r

  lazy val argument: Parser[String] = squoted | dquoted | token
  lazy val commandLine: Parser[List[String]] = phrase(repsep(argument, whiteSpace))

  class ParseException(msg: String) extends RuntimeException(msg)

  def tokenize(line: String): List[String] = tokenize(line, x => throw new ParseException(x))
  def tokenize(line: String, errorFn: String => Unit): List[String] = {
    parse(commandLine, line.trim) match {
      case Success(args, _)     => args
      case NoSuccess(msg, rest) => errorFn(msg) ; Nil
    }
  }
}
