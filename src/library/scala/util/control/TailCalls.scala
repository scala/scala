/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.util.control

import annotation.tailrec

/** Methods exported by this object implement tail calls via trampolining.
 *
 *  Tail calling methods must either return their result using `done` or call the
 *  next method using `tailcall`. Both return an instance of `TailRec`. The result
 *  of evaluating a tailcalling function can be retrieved from a `TailRec`
 *  value using method `result`.
 *
 *  Implemented as described in "Stackless Scala with Free Monads"
 *  [[https://blog.higher-order.com/assets/trampolines.pdf]]
 *
 *  Here's a usage example:
 *  {{{
 *  import scala.util.control.TailCalls._
 *
 *  def isEven(xs: List[Int]): TailRec[Boolean] =
 *    if (xs.isEmpty) done(true) else tailcall(isOdd(xs.tail))
 *
 *  def isOdd(xs: List[Int]): TailRec[Boolean] =
 *   if (xs.isEmpty) done(false) else tailcall(isEven(xs.tail))
 *
 *  isEven((1 to 100000).toList).result
 *
 *  def fib(n: Int): TailRec[Int] =
 *    if (n < 2) done(n) else for {
 *      x <- tailcall(fib(n - 1))
 *      y <- tailcall(fib(n - 2))
 *    } yield x + y
 *
 *  fib(40).result
 *  }}}
 */
object TailCalls {

  /** This class represents a tailcalling computation.
   */
  sealed abstract class TailRec[+A] {

    /** Continue the computation with `f`. */
    final def map[B](f: A => B): TailRec[B] = flatMap(a => Call(() => Done(f(a))))

    /** Continue the computation with `f` and merge the trampolining
     *  of this computation with that of `f`. */
    final def flatMap[B](f: A => TailRec[B]): TailRec[B] = this match {
      case Done(a)         => Call(() => f(a))
      case Call(_)         => Cont(this, f)
      // Take advantage of the monad associative law to optimize the size of the required stack
      case c: Cont[a1, b1] => Cont(c.a, (x: a1) => c.f(x).flatMap(f))
    }

    /** Returns either the next step of the tailcalling computation,
      * or the result if there are no more steps. */
    @tailrec final def resume: Either[() => TailRec[A], A] = this match {
      case Done(a)    => Right(a)
      case Call(k)    => Left(k)
      case Cont(a, f) => a match {
        case Done(v)    => f(v).resume
        case Call(k)    => Left(() => k().flatMap(f))
        case Cont(b, g) => b.flatMap(x => g(x).flatMap(f)).resume
      }
    }

    /** Returns the result of the tailcalling computation.
     */
    @tailrec final def result: A = this match {
      case Done(a)    => a
      case Call(t)    => t().result
      case Cont(a, f) => a match {
        case Done(v)    => f(v).result
        case Call(t)    => t().flatMap(f).result
        case Cont(b, g) => b.flatMap(x => g(x).flatMap(f)).result
      }
    }
  }

  /** Internal class representing a tailcall. */
  protected case class Call[A](rest: () => TailRec[A]) extends TailRec[A]

  /** Internal class representing the final result returned from a tailcalling
   *  computation. */
  protected case class Done[A](value: A) extends TailRec[A]

  /** Internal class representing a continuation with function A => TailRec[B].
   *  It is needed for the flatMap to be implemented. */
  protected case class Cont[A, B](a: TailRec[A], f: A => TailRec[B]) extends TailRec[B]

  /** Perform a tailcall.
   *  @param rest  the expression to be evaluated in the tailcall
   *  @return a `TailRec` object representing the expression `rest`
   */
  def tailcall[A](rest: => TailRec[A]): TailRec[A] = Call(() => rest)

  /** Return the final result from a tailcalling computation.
   *  @param  `result` the result value
   *  @return a `TailRec` object representing a computation which immediately
   *          returns `result`
   */
  def done[A](result: A): TailRec[A] = Done(result)

}
