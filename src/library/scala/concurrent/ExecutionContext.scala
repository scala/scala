package scala.concurrent


import java.util.concurrent.{ Executors, Future => JFuture }
import scala.util.{ Duration, Timeout }
import scala.concurrent.forkjoin.{ ForkJoinPool, RecursiveTask => FJTask, RecursiveAction, ForkJoinWorkerThread }


trait ExecutionContext {
  
  protected implicit object CanBlockEvidence extends CanBlock
  
  def execute(task: Runnable): Unit
  
  def task[T](task: () => T): Task[T]
  
  def promise[T]: Promise[T]
  
  /** Only callable from the tasks running on the same execution context. */
  def blockingCall[T](body: Blockable[T]): T
  
}


object ExecutionContext {

  lazy val forNonBlocking = new ForkJoinExecutionContext

  //lazy val forBlocking = new BlockingExecutionContext

}


sealed trait CanBlock


