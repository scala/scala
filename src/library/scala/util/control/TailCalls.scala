/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package util.control

import collection.mutable.ArrayStack

/** Methods exported by this object implement tail calls via trampolining.
 *  Tail calling methods have to return their result using `done` or call the
 *  next method using `tailcall`. Both return a `TailRec` object. The result
 *  of evaluating a tailcalling function can be retrieved from a `Tailrec`
 *  value using method `result`.
 *  Implemented as described in "Stackless Scala with Free Monads"
 *  http://blog.higher-order.com/assets/trampolines.pdf
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
 *    } yield (x + y)
 *
 *  fib(40).result
 *  }}}
 */
object TailCalls {

  /** This class represents a tailcalling computation
   */
  abstract class TailRec[+A] {
    def map[B](f: A => B): TailRec[B] =
      flatMap(a => Call(() => Done(f(a))))
    def flatMap[B](f: A => TailRec[B]): TailRec[B] =
      Cont(this, f)
    /** Returns the result of the tailcalling computation.
     */
    def result: A = {
      var cur: TailRec[_] = this
      val stack: ArrayStack[Any => TailRec[A]] = new ArrayStack()
      var result: A = null.asInstanceOf[A]
      while (result == null) {
        cur match {
          case Done(a) =>
            if(!stack.isEmpty) {
              val fun = stack.pop
              cur = fun(a)
            } else result = a.asInstanceOf[A]
          case Call(t) => cur = t()
          case Cont(a, f) => {
            cur = a
            stack.push(f.asInstanceOf[Any => TailRec[A]])
          }
        }
      }
      result
    }
  }

  /** Internal class representing a tailcall */
  protected case class Call[A](rest: () => TailRec[A]) extends TailRec[A]

  /** Internal class representing the final result returned from a tailcalling
    * computation */
  protected case class Done[A](override val result: A) extends TailRec[A]

  /** Internal class representing a continuation with function A => TailRec[B].
    * It is needed for the flatMap to be implemented. */
  protected case class Cont[A, B](a: TailRec[A], f: A => TailRec[B]) extends TailRec[B]

  /** Performs a tailcall
   *  @param rest  the expression to be evaluated in the tailcall
   *  @return a `TailRec` object representing the expression `rest`
   */
  def tailcall[A](rest: => TailRec[A]): TailRec[A] = Call(() => rest)

  /** Used to return final result from tailcalling computation
   *  @param  `result` the result value
   *  @return a `TailRec` object representing a computation which immediately
   *          returns `result`
   */
  def done[A](result: A): TailRec[A] = Done(result)

}
