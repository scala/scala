/* NEST (New Scala Test)
 * Copyright 2007-2012 LAMP/EPFL
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
 *
 *  XXX Note this has been completely obsolesced by scala.tools.cmd.
 *  I checked it back in as part of rolling partest back a month
 *  rather than go down the rabbit hole of unravelling dependencies.
 */

trait ParserUtil extends Parsers {
  protected implicit class ParserPlus[+T](underlying: Parser[T]) {
    def !~>[U](p: => Parser[U]): Parser[U] = (underlying ~! p) ^^ { case a~b  => b }
    def <~![U](p: => Parser[U]): Parser[T] = (underlying ~! p) ^^ { case a~b  => a }
  }
}

case class CommandLine(
  args: List[String],
  unaryArguments: List[String],
  binaryArguments: List[String]
) {
  def this(args: List[String]) = this(args, Nil, Nil)
  def this(args: Array[String]) = this(args.toList, Nil, Nil)
  def this(line: String) = this(CommandLineParser tokenize line, Nil, Nil)

  def withUnaryArgs(xs: List[String]) = copy(unaryArguments = xs)
  def withBinaryArgs(xs: List[String]) = copy(binaryArguments = xs)

  def originalArgs = args
  def assumeBinary = true
  def enforceArity = true
  def onlyKnownOptions = false

  val Terminator = "--"
  val ValueForUnaryOption = "true"  // so if --opt is given, x(--opt) = true

  def mapForUnary(opt: String) = Map(opt -> ValueForUnaryOption)
  def errorFn(msg: String) = println(msg)

  /** argMap is option -> argument (or "" if it is a unary argument)
   *  residualArgs are what is left after removing the options and their args.
   */
  lazy val (argMap, residualArgs) = {
    val residualBuffer = new ListBuffer[String]

    def stripQuotes(s: String) = {
      def isQuotedBy(c: Char) = s.length > 0 && s.head == c && s.last == c
      if (List('"', '\'') exists isQuotedBy) s.tail.init else s
    }

    def isValidOption(s: String) = !onlyKnownOptions || (unaryArguments contains s) || (binaryArguments contains s)
    def isOption(s: String) = (s startsWith "-") && (isValidOption(s) || { unknownOption(s) ; false })
    def isUnary(s: String) = isOption(s) && (unaryArguments contains s)
    def isBinary(s: String) = isOption(s) && !isUnary(s) && (assumeBinary || (binaryArguments contains s))

    def unknownOption(opt: String) =
      errorFn("Option '%s' not recognized.".format(opt))
    def missingArg(opt: String, what: String) =
      errorFn("Option '%s' requires argument, found %s instead.".format(opt, what))

    def loop(args: List[String]): Map[String, String] = {
      def residual(xs: List[String]) = { residualBuffer ++= xs ; Map[String, String]() }
      if (args.isEmpty) return Map()
      val hd :: rest = args
      if (rest.isEmpty) {
        if (isBinary(hd) && enforceArity)
          missingArg(hd, "EOF")

        if (isOption(hd)) mapForUnary(hd) else residual(args)
      }
      else
        if (hd == Terminator) residual(rest)
      else {
        val hd1 :: hd2 :: rest = args

        if (hd2 == Terminator) mapForUnary(hd1) ++ residual(rest)
        else if (isUnary(hd1)) mapForUnary(hd1) ++ loop(hd2 :: rest)
        else if (isBinary(hd1)) {
          // Disabling this check so
          //  --scalacopts "-verbose" works.  We can't tell if it's quoted,
          // the shell does us in.
          //
          // if (isOption(hd2) && enforceArity)
          //   missingArg(hd1, hd2)

          Map(hd1 -> hd2) ++ loop(rest)
        }
        else { residual(List(hd1)) ++ loop(hd2 :: rest) }
      }
    }

    (loop(args), residualBuffer map stripQuotes toList)
  }

  def isSet(arg: String) = args contains arg
  def get(arg: String) = argMap get arg
  def getOrElse(arg: String, orElse: => String) = if (isSet(arg)) apply(arg) else orElse
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
  def apply(line: String) = new CommandLine(tokenize(line))
}
