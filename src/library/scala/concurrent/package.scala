/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import scala.util.{ Duration, Try, Success, Failure }

/** This package object contains primitives for concurrent and parallel programming.
 */
package object concurrent extends scala.concurrent.ConcurrentPackageObject {
  type ExecutionException =    java.util.concurrent.ExecutionException
  type CancellationException = java.util.concurrent.CancellationException
  type TimeoutException =      java.util.concurrent.TimeoutException
}

package concurrent {
  object await {
    def ready[T](atMost: Duration)(awaitable: Awaitable[T])(implicit execCtx: ExecutionContext = executionContext): Awaitable[T] = {
      try blocking(awaitable, atMost)
      catch { case _ => }
      awaitable
    }

    def result[T](atMost: Duration)(awaitable: Awaitable[T])(implicit execCtx: ExecutionContext = executionContext): T = {
      blocking(awaitable, atMost)
    }
  }

  /** Importing this object allows using some concurrency primitives
   *  on futures and promises that can yield nondeterministic programs.
   *
   *  While program determinism is broken when using these primitives,
   *  some programs cannot be written without them (e.g. multiple client threads
   *  cannot send requests to a server thread through regular promises and futures).
   */
  object nondeterministic { }

  /** A timeout exception.
   *
   *  Futures are failed with a timeout exception when their timeout expires.
   *
   *  Each timeout exception contains an origin future which originally timed out.
   */
  class FutureTimeoutException(origin: Future[_], message: String) extends TimeoutException(message) {
    def this(origin: Future[_]) = this(origin, "Future timed out.")
  }

  final class DurationOps private[concurrent] (x: Int) {
    // TODO ADD OTHERS
    def ns = util.Duration.fromNanos(0)
  }
}
