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
    
    def ready[T](awaitable: Awaitable[T], atMost: Duration): awaitable.type = {
      blocking(awaitable, atMost)
      awaitable
    }
    
    def result[T](awaitable: Awaitable[T], atMost: Duration): T = {
      blocking(awaitable, atMost)
    }
    
  }

  final class DurationOps private[concurrent] (x: Int) {
    // TODO ADD OTHERS
    def ns = util.Duration.fromNanos(0)
  }
}
