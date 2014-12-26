/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.impl



import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import scala.util.{ Success, Failure }


private[concurrent] object Future {
  class PromiseCompletingRunnable[T](body: => T) extends Runnable {
    val promise = new Promise.DefaultPromise[T]()

    override def run() = {
      promise complete {
        try Success(body) catch {
          // Catch all throwables here, so they can be communicated back to the
          // calling thread. Throwables that aren't handled here are invisible
          // to the caller, so it doesn't make sense to let any leak, even if
          // they're fatal.
          case t: Throwable => Failure(t)
        }
      }
    }
  }

  def apply[T](body: =>T)(implicit executor: ExecutionContext): scala.concurrent.Future[T] = {
    val runnable = new PromiseCompletingRunnable(body)
    executor.prepare.execute(runnable)
    runnable.promise.future
  }
}
