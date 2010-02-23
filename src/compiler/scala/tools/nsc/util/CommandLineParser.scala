/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package util

import scala.util.parsing.combinator._
import scala.util.parsing.input.{ Reader }
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.collection.mutable.ListBuffer

/** A simple command line parser to replace the several different
 *  simple ones spread around trunk.
 */

trait ParserUtil extends Parsers {
  class ParserPlus[+T](underlying: Parser[T]) {
    def !~>[U](p: => Parser[U]): Parser[U] = (underlying ~! p) ^^ { case a~b  => b }
    def <~![U](p: => Parser[U]): Parser[T] = (underlying ~! p) ^^ { case a~b  => a }
  }
  protected implicit def parser2parserPlus[T](p: Parser[T]): ParserPlus[T] = new ParserPlus(p)
}

class CommandLine(val args: List[String], val unaryArguments: List[String]) {
  def this(args: List[String]) = this(args, Nil)
  def this(line: String) = this(CommandLineParser tokenize line)

  def withUnaryArguments(xs: List[String]) = new CommandLine(args, xs)
  def enforceArity = true
  def errorFn(msg: String) = println(msg)

  /** argMap is option -> argument (or "" if it is a unary argument)
   *  residualArgs are what is left after removing the options and their args.
   */
  lazy val (argMap, residualArgs) = {
    val residual = new ListBuffer[String]
    def isOption(s: String) = s startsWith "-"
    def isUnary(s: String) = isOption(s) && (unaryArguments contains s)
    def isBinary(s: String) = isOption(s) && !(unaryArguments contains s)
    def missingArg(opt: String, what: String) =
      errorFn("Option '%s' requires argument, found %s instead.".format(opt, what))

    def loop(args: List[String]): Map[String, String] = args match {
      case Nil                      => Map()
      case x :: xs if !isOption(x)  => residual += x ; loop(xs)
      case x :: xs if isUnary(x)    => Map(x -> "") ++ loop(xs)
      case x :: Nil                 => if (enforceArity) missingArg(x, "EOF") ; Map(x -> "")
      case "--" :: xs               => residual ++= xs ; Map()
      case x :: "--" :: xs          => residual ++= xs ; Map(x -> "")
      case x1 :: x2 :: xs           =>
        if (enforceArity && isOption(x2))
          missingArg(x1, x2)

        if (isOption(x2)) Map(x1 -> "") ++ loop(x2 :: xs)
        else Map(x1 -> x2) ++ loop(xs)
    }

    val (unaries, rest) = args partition (unaryArguments contains _)
    val map = loop(rest)

    (map ++ Map(unaries map (x => x -> ""): _*), residual.toList)
  }

  def isSet(arg: String) = args contains arg
  def get(arg: String) = argMap get arg
  def apply(arg: String) = argMap(arg)

  override def toString() = "CommandLine(\n%s)\n" format (args map ("  " + _ + "\n") mkString)
}

object CommandLineParser extends RegexParsers with ParserUtil {
  override def skipWhitespace = false

  def elemExcept(xs: Elem*): Parser[Elem] = elem("elemExcept", x => x != EofCh && !(xs contains x))
  def elemOf(xs: Elem*): Parser[Elem]     = elem("elemOf", xs contains _)
  def escaped(ch: Char): Parser[String] = "\\" + ch
  def mkQuoted(ch: Char): Parser[String] = (
      elem(ch) !~> rep(escaped(ch) | elemExcept(ch)) <~ ch ^^ (_.mkString)
    | failure("Unmatched %s in input." format ch)
  )

  lazy val squoted: Parser[String] = mkQuoted('\'') ^^ (x => "'%s'" format x)
  lazy val dquoted: Parser[String] = mkQuoted('"') ^^ (x => "\"" + x + "\"")
  lazy val token: Parser[String]   = """\S+""".r

  lazy val argument: Parser[String] = squoted | dquoted | token
  lazy val commandLine: Parser[List[String]] = phrase(repsep(argument, whiteSpace))

  class ParseError(msg: String) extends RuntimeException(msg)

  def tokenize(line: String): List[String] = tokenize(line, x => throw new ParseError(x))
  def tokenize(line: String, errorFn: String => Unit): List[String] = {
    parse(commandLine, line.trim) match {
      case Success(args, _)     => args
      case NoSuccess(msg, rest) => errorFn(msg) ; Nil
    }
  }
  def apply(line: String) = new CommandLine(tokenize(line))
}
