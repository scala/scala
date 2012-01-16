/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent



import java.util.concurrent.{ Executors, Future => JFuture, Callable }
import scala.util.{ Duration, Timeout }
import scala.concurrent.forkjoin.{ ForkJoinPool, RecursiveTask => FJTask, RecursiveAction, ForkJoinWorkerThread }



trait ExecutionContext {
  
  protected implicit object CanAwaitEvidence extends CanAwait
  
  def execute(runnable: Runnable): Unit
  
  def execute[U](body: () => U): Unit
  
  def promise[T]: Promise[T]
  
  def future[T](body: Callable[T]): Future[T] = future(body.call())
  
  def future[T](body: => T): Future[T]
  
  def blocking[T](atMost: Duration)(body: =>T): T
  
  def blocking[T](atMost: Duration)(awaitable: Awaitable[T]): T
  
}


sealed trait CanAwait


