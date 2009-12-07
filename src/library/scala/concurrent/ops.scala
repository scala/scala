/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.concurrent

import java.lang.Thread
import scala.util.control.Exception.allCatch

/** The object <code>ops</code> ...
 *
 *  @author  Martin Odersky, Stepan Koltsov, Philipp Haller
 */
object ops
{
  val defaultRunner: FutureTaskRunner = TaskRunners.threadRunner

  /**
   *  If expression computed successfully return it in <code>Right</code>,
   *  otherwise return exception in <code>Left</code>.
   */
  //TODO: make private
  def tryCatch[A](body: => A): Either[Throwable, A] =
    allCatch[A] either body

  //TODO: make private
  def tryCatchEx[A](body: => A): Either[Exception, A] =
    try Right(body) catch {
      case ex: Exception  => Left(ex)
    }

  //TODO: make private
  def getOrThrow[T <: Throwable, A](x: Either[T, A]): A =
    x.fold[A](throw _, identity _)

  /** Evaluates an expression asynchronously.
   *
   *  @param  p the expression to evaluate
   */
  def spawn(p: => Unit)(implicit runner: TaskRunner = defaultRunner): Unit = {
    runner execute runner.functionAsTask(() => p)
  }

  /**
   *  @param p ...
   *  @return  ...
   */
  def future[A](p: => A)(implicit runner: FutureTaskRunner = defaultRunner): () => A = {
    runner.futureAsFunction(runner submit runner.functionAsTask(() => p))
  }

  /**
   *  @param xp ...
   *  @param yp ...
   *  @return   ...
   */
  def par[A, B](xp: => A, yp: => B): (A, B) = {
    val y = new SyncVar[Either[Throwable, B]]
    spawn { y set tryCatch(yp) }
    (xp, getOrThrow(y.get))
  }

  /**
   *  @param start ...
   *  @param end   ...
   *  @param p     ...
   */
  def replicate(start: Int, end: Int)(p: Int => Unit) {
    if (start == end)
      ()
    else if (start + 1 == end)
      p(start)
    else {
      val mid = (start + end) / 2
      spawn { replicate(start, mid)(p) }
      replicate(mid, end)(p)
    }
  }

/*
  def parMap[a,b](f: a => b, xs: Array[a]): Array[b] = {
    val results = new Array[b](xs.length);
    replicate(0, xs.length) { i => results(i) = f(xs(i)) }
    results
  }
*/

}
