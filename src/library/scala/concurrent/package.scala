/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import scala.util.{ Try, Success, Failure }
import scala.concurrent.util.Duration

/** This package object contains primitives for concurrent and parallel programming.
 */
package object concurrent extends scala.concurrent.ConcurrentPackageObject {
  type ExecutionException =    java.util.concurrent.ExecutionException
  type CancellationException = java.util.concurrent.CancellationException
  type TimeoutException =      java.util.concurrent.TimeoutException
}

package concurrent {
  
  sealed trait CanAwait
  
  object Await {
    private[concurrent] implicit val canAwaitEvidence = new CanAwait {}
    
    def ready[T](awaitable: Awaitable[T], atMost: Duration): Awaitable[T] = awaitable.ready(atMost)
    
    def result[T](awaitable: Awaitable[T], atMost: Duration): T = awaitable.result(atMost)
  }

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
