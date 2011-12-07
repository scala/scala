package scala.concurrent


import java.util.concurrent.{ Executors, Future => JFuture }
import scala.util.{ Duration, Timeout }
import scala.concurrent.forkjoin.{ ForkJoinPool, RecursiveTask => FJTask, RecursiveAction, ForkJoinWorkerThread }


trait ExecutionContext {
  
  protected implicit object CanBlockEvidence extends CanBlock
  
  def execute(task: Runnable): Unit
  
  def task[T](task: () => T)(implicit timeout: Timeout): Task[T]
  
  def promise[T](implicit timeout: Timeout): Promise[T]
  
  def blockingCall[T](body: Blockable[T]): T
  
}


object ExecutionContext {

  lazy val forNonBlocking = new ForkJoinExecutionContext

  //lazy val forBlocking = new BlockingExecutionContext

}
