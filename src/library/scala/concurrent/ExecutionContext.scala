/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent



import java.util.concurrent.atomic.{ AtomicInteger }
import java.util.concurrent.{ Executors, Future => JFuture, Callable, ExecutorService, Executor }
import scala.concurrent.util.Duration
import scala.concurrent.forkjoin.{ ForkJoinPool, RecursiveTask => FJTask, RecursiveAction, ForkJoinWorkerThread }
import scala.collection.generic.CanBuildFrom
import collection._



trait ExecutionContext {

  def execute(runnable: Runnable): Unit

  def execute[U](body: () => U): Unit

  def internalBlockingCall[T](awaitable: Awaitable[T], atMost: Duration): T
  
  def reportFailure(t: Throwable): Unit

  /* implementations follow */

  private implicit val executionContext = this

}


/** Contains factory methods for creating execution contexts.
 */
object ExecutionContext {
  
  implicit def defaultExecutionContext: ExecutionContext = scala.concurrent.defaultExecutionContext
  
  /** Creates an `ExecutionContext` from the given `ExecutorService`.
   */
  def fromExecutorService(e: ExecutorService): ExecutionContext with Executor = new impl.ExecutionContextImpl(e)
  
  /** Creates an `ExecutionContext` from the given `Executor`.
   */
  def fromExecutor(e: Executor): ExecutionContext with Executor = new impl.ExecutionContextImpl(e)
  
}


