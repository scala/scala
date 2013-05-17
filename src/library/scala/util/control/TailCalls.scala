/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package util.control

/** Methods exported by this object implement tail calls via trampolining.
 *  Tail calling methods have to return their result using `done` or call the
 *  next method using `tailcall`. Both return a `TailRec` object. The result
 *  of evaluating a tailcalling function can be retrieved from a `Tailrec`
 *  value using method `result`. Here's a usage example:
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
 *  }}}
 */
object TailCalls {

  @annotation.tailrec
  private def loop(tailRec: TailRec[_]): Any = tailRec.underlying match {
    case Done(result) => result
    case underlying => loop(underlying().asInstanceOf[TailRec[_]])
  }

  /** This class represents a tailcalling computation
   */
  class TailRec[+A] private[TailCalls](val underlying: () => _) extends AnyVal {
    /** Returns the result of the tailcalling computation.
     */
    def result:A = loop(this).asInstanceOf[A]
  }

  /** Internal class representing the final result returned from a tailcalling
    * computation */
  private case class Done[+A](override val apply:A) extends (() => A)

  /** Performs a tailcall
   *  @param rest  the expression to be evaluated in the tailcall
   *  @return a `TailRec` object representing the expression `rest`
   */
  def tailcall[A](rest: => TailRec[A]): TailRec[A] = new TailRec(rest _)

  /** Used to return final result from tailcalling computation
   *  @param  `result` the result value
   *  @return a `TailRec` object representing a computation which immediately
   *          returns `result`
   */
  def done[A](result: A): TailRec[A] = new TailRec(Done(result))

}
