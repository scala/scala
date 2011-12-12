package scala.concurrent


import java.util.concurrent.{ Executors, Future => JFuture, Callable }
import scala.util.{ Duration, Timeout }
import scala.concurrent.forkjoin.{ ForkJoinPool, RecursiveTask => FJTask, RecursiveAction, ForkJoinWorkerThread }


trait ExecutionContext {
  
  protected implicit object CanBlockEvidence extends CanBlock
  
  def execute(runnable: Runnable): Unit
  
  def execute[U](body: () => U): Unit
  
  def promise[T]: Promise[T]
  
  def future[T](body: Callable[T]): Future[T] = future(body.call())
  
  def future[T](body: => T): Future[T]

  /** Only callable from the tasks running on the same execution context. */
  def blockingCall[T](timeout: Timeout, body: Blockable[T]): T
  
}


sealed trait CanBlock


