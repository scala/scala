/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
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
  // !!! I don't think this should be implicit, but it does need to be
  // made available as a default argument (difficult at present, see spawn.)
  // If it is merely implicit without being specified as a default, then it
  // will not be in scope for callers unless ops._ is first imported.
  implicit val defaultRunner: FutureTaskRunner =
    TaskRunners.threadRunner

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
  // !!! this should have a signature like:
  //   def spawn(p: => Unit)(implicit runner: TaskRunner = defaultRunner): Unit
  // but at present the mixture of by-name argument and default implicit causes a crash.

  def spawn(p: => Unit): Unit = spawn(p, defaultRunner)
  def spawn(p: => Unit, runner: TaskRunner): Unit = {
    runner execute runner.functionAsTask(() => p)
  }

  /**
   *  @param p ...
   *  @return  ...
   */
  // See spawn above, this should have a signature like
  // def future[A](p: => A)(implicit runner: FutureTaskRunner = defaultRunner): () => A
  def future[A](p: => A): () => A = future[A](p, defaultRunner)
  def future[A](p: => A, runner: FutureTaskRunner): () => A = {
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
