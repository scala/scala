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

/** The object <code>ops</code> ...
 *
 *  @author  Martin Odersky, Stepan Koltsov, Philipp Haller
 */
object ops {

  implicit val defaultRunner: TaskRunner[Unit] =
    TaskRunners.threadRunner

  /**
   *  If expression computed successfully return it in <code>Left</code>,
   *  otherwise return exception in <code>Right</code>.
   */
  private def tryCatch[A](left: => A): Either[A, Throwable] = {
    try {
      Left(left)
    } catch {
      case t => Right(t)
    }
  }

  /** Evaluates an expression asynchronously.
   *
   *  @param  p the expression to evaluate
   */
  def spawn(p: => Unit)(implicit runner: TaskRunner[Unit] = defaultRunner): Unit = {
    runner submit (() => p)
  }

  /**
   *  @param p ...
   *  @return  ...
   */
  def future[A](p: => A)(implicit runner: TaskRunner[Unit] = defaultRunner): () => A = {
    val result = new SyncVar[Either[A, Throwable]]
    spawn({ result set tryCatch(p) })(runner)
    () => result.get match {
    	case Left(a) => a
    	case Right(t) => throw t
    }
  }

  /**
   *  @param xp ...
   *  @param yp ...
   *  @return   ...
   */
  def par[A, B](xp: => A, yp: => B): (A, B) = {
    val y = new SyncVar[Either[B, Throwable]]
    spawn { y set tryCatch(yp) }
    (xp, y.get match {
    	case Left(b) => b
    	case Right(t) => throw t
    })
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
