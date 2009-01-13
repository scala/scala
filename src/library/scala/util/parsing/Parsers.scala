/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.parsing

/** Documentation for this class is currently missing.
 *  However, the Scala By Examples document contains a
 *  chapter on combinator parsing that comes close.
 *
 *  @author  Burak Emir
 *  @version 1.0
 *
 *  @deprecated use <a target="contentFrame" href="combinator/Parsers.html">
 *              <code>scala.util.parsing.combinator.Parsers</code></a>
 *              instead.
 */
@deprecated
abstract class Parsers {

  type inputType

  abstract class Parser[A] {

    type Result = Option[(A, inputType)]

    def apply(in: inputType): Result

    def filter(pred: A => Boolean) = new Parser[A] {
      def apply(in: inputType): Result = Parser.this.apply(in) match {
        case None => None
        case Some((x, in1)) => if (pred(x)) Some((x, in1)) else None
      }
    }

    def map[B](f: A => B) = new Parser[B] {
      def apply(in: inputType): Result = Parser.this.apply(in) match {
        case None => None
        case Some((x, in1)) => Some((f(x), in1))
      }
    }

    def flatMap[B](f: A => Parser[B]) = new Parser[B] {
      def apply(in: inputType): Result = Parser.this.apply(in) match {
        case None => None
        case Some((x, in1)) => f(x).apply(in1)
      }
    }

    def ||| (p: => Parser[A]) = new Parser[A] {
      def apply(in: inputType): Result = Parser.this.apply(in) match {
        case None => p(in)
        case s => s
      }
    }

    def &&& [B](p: => Parser[B]): Parser[B] =
      for (_ <- this; val x <- p) yield x
  }

  def not[A](p: Parser[A]) = new Parser[Unit] {
    def apply(in: inputType): Result = p.apply(in) match {
      case None => Some(((), in))
      case Some(_) => None
    }
  }

  def succeed[A](x: A) = new Parser[A] {
    def apply(in: inputType): Result = Some((x, in))
  }

  def rep[A](p: Parser[A]): Parser[List[A]] =
    rep1(p) ||| succeed(List())

  def rep1[A](p: Parser[A]): Parser[List[A]] =
    for (x <- p; val xs <- rep(p)) yield x :: xs

  def repWith[A, B](p: Parser[A], sep: Parser[B]): Parser[List[A]] =
    for (x <- p; val xs <- rep(sep &&& p)) yield x :: xs

  def opt[A](p: Parser[A]): Parser[List[A]] =
    (for (x <- p) yield List(x)) ||| succeed(List())
}
