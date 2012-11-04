/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.control

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

  /** This class represents a tailcalling computation
   */
  abstract class TailRec[+A] {
    /** Returns the result of the tailcalling computation.
     */
    def result: A = {
      def loop(body: TailRec[A]): A = body match {
        case Call(rest) => loop(rest())
        case Done(result) => result
      }
      loop(this)
    }
  }

  /** Internal class representing a tailcall */
  protected case class Call[A](rest: () => TailRec[A]) extends TailRec[A]

  /** Internal class representing the final result returned from a tailcalling
    * computation */
  protected case class Done[A](override val result: A) extends TailRec[A]

  /** Performs a tailcall
   *  @param rest  the expression to be evaluated in the tailcall
   *  @return a `TailRec` object representing the expression `rest`
   */
  def tailcall[A](rest: => TailRec[A]): TailRec[A] = new Call(() => rest)

  /** Used to return final result from tailcalling computation
   *  @param  `result` the result value
   *  @return a `TailRec` object representing a computation which immediately
   *          returns `result`
   */
  def done[A](result: A): TailRec[A] = new Done(result)

}
