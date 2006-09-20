/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.parsing

/** This class ...
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
abstract class Parsers {

  type inputType

  abstract class Parser[a] {

    type Result = Option[Pair[a, inputType]]

    def apply(in: inputType): Result

    def filter(pred: a => boolean) = new Parser[a] {
      def apply(in: inputType): Result = Parser.this.apply(in) match {
        case None => None
        case Some(Pair(x, in1)) => if (pred(x)) Some(Pair(x, in1)) else None
      }
    }

    def map[b](f: a => b) = new Parser[b] {
      def apply(in: inputType): Result = Parser.this.apply(in) match {
        case None => None
        case Some(Pair(x, in1)) => Some(Pair(f(x), in1))
      }
    }

    def flatMap[b](f: a => Parser[b]) = new Parser[b] {
      def apply(in: inputType): Result = Parser.this.apply(in) match {
        case None => None
        case Some(Pair(x, in1)) => f(x).apply(in1)
      }
    }

    def ||| (p: => Parser[a]) = new Parser[a] {
      def apply(in: inputType): Result = Parser.this.apply(in) match {
        case None => p(in)
        case s => s
      }
    }

    def &&& [b](p: => Parser[b]): Parser[b] =
      for (val _ <- this; val x <- p) yield x
  }

  def not[a](p: Parser[a]) = new Parser[unit] {
    def apply(in: inputType): Result = p.apply(in) match {
      case None => Some(Pair((), in))
      case Some(_) => None
    }
  }

  def succeed[a](x: a) = new Parser[a] {
    def apply(in: inputType): Result = Some(Pair(x, in))
  }

  def rep[a](p: Parser[a]): Parser[List[a]] =
    rep1(p) ||| succeed(List())

  def rep1[a](p: Parser[a]): Parser[List[a]] =
    for (val x <- p; val xs <- rep(p)) yield x :: xs

  def repWith[a, b](p: Parser[a], sep: Parser[b]): Parser[List[a]] =
    for (val x <- p; val xs <- rep(sep &&& p)) yield x :: xs

  def opt[a](p: Parser[a]): Parser[List[a]] =
    (for (val x <- p) yield List(x)) ||| succeed(List())
}
