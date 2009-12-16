/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.util.parsing.combinator

/** This object contains implicit conversions that come in handy when using the `^^' combinator
 * {@see Parsers} to construct an AST from the concrete syntax.
 *<p>
 * The reason for this is that the sequential composition combinator (`~') combines its constituents
 * into a ~. When several `~'s are combined, this results in nested `~'s (to the left).
 * The `flatten*' coercions makes it easy to apply an `n'-argument function to a nested ~ of
 * depth (`n-1')</p>
 *<p>
 * The `headOptionTailToFunList' converts a function that takes a List[A] to a function that
 * accepts a ~[A, Option[List[A]]] (this happens when, e.g., parsing something of the following
 * shape: p ~ opt("." ~ repsep(p, ".")) -- where `p' is a parser that yields an A)</p>
 *
 * @author Martin Odersky, Iulian Dragos, Adriaan Moors
 */
trait ImplicitConversions { self: Parsers =>
  implicit def flatten2[A, B, C]         (f: (A, B) => C) =
    (p: ~[A, B]) => p match {case a ~ b => f(a, b)}
  implicit def flatten3[A, B, C, D]      (f: (A, B, C) => D) =
    (p: ~[~[A, B], C]) => p match {case a ~ b ~ c => f(a, b, c)}
  implicit def flatten4[A, B, C, D, E]   (f: (A, B, C, D) => E) =
    (p: ~[~[~[A, B], C], D]) => p match {case a ~ b ~ c ~ d => f(a, b, c, d)}
  implicit def flatten5[A, B, C, D, E, F](f: (A, B, C, D, E) => F) =
    (p: ~[~[~[~[A, B], C], D], E]) => p match {case a ~ b ~ c ~ d ~ e=> f(a, b, c, d, e)}
  implicit def headOptionTailToFunList[A, T] (f: List[A] => T)=
    (p: ~[A, Option[List[A]]]) => f(p._1 :: (p._2 match { case Some(xs) => xs case None => Nil}))
}
