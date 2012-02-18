/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import java.lang.Thread
import scala.util.control.Exception.allCatch

/** The object `ops` ...
 *
 *  @author  Martin Odersky, Stepan Koltsov, Philipp Haller
 */
@deprecated("Use `future` instead.", "2.10.0")
object ops
{
  val defaultRunner: FutureTaskRunner = TaskRunners.threadRunner

  /**
   *  If expression computed successfully return it in `Right`,
   *  otherwise return exception in `Left`.
   */
  private def tryCatch[A](body: => A): Either[Throwable, A] =
    allCatch[A] either body

  private def getOrThrow[T <: Throwable, A](x: Either[T, A]): A =
    x.fold[A](throw _, identity _)

  /** Evaluates an expression asynchronously.
   *
   *  @param  p the expression to evaluate
   */
  def spawn(p: => Unit)(implicit runner: TaskRunner = defaultRunner): Unit = {
    runner execute runner.functionAsTask(() => p)
  }

  /** Evaluates an expression asynchronously, and returns a closure for
   *  retrieving the result.
   *
   *  @param  p the expression to evaluate
   *  @return   a closure which returns the result once it has been computed
   */
  def future[A](p: => A)(implicit runner: FutureTaskRunner = defaultRunner): () => A = {
    runner.futureAsFunction(runner submit runner.functionAsTask(() => p))
  }

  /** Evaluates two expressions in parallel. Invoking `par` blocks the current
   *  thread until both expressions have been evaluated.
   *
   *  @param  xp the first expression to evaluate
   *  @param  yp the second expression to evaluate
   *
   *  @return    a pair holding the evaluation results
   */
  def par[A, B](xp: => A, yp: => B)(implicit runner: TaskRunner = defaultRunner): (A, B) = {
    val y = new SyncVar[Either[Throwable, B]]
    spawn { y set tryCatch(yp) }
    (xp, getOrThrow(y.get))
  }

/*
  def parMap[a,b](f: a => b, xs: Array[a]): Array[b] = {
    val results = new Array[b](xs.length);
    replicate(0, xs.length) { i => results(i) = f(xs(i)) }
    results
  }
*/

}
