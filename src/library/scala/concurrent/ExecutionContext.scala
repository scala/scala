package scala.concurrent

import java.util.concurrent.{ Executors, Future => JFuture }
import scala.util.{ Duration, Timeout }
import scala.concurrent.forkjoin.{ ForkJoinPool, RecursiveTask => FJTask, RecursiveAction, ForkJoinWorkerThread }

trait ExecutionContext {

  protected implicit object CanBlockEvidence extends CanBlock
  
  def execute(task: Runnable): Unit
  
  def makeTask[T](task: () => T)(implicit timeout: Timeout): Task[T]
  
  def makePromise[T](timeout: Timeout): Promise[T]
  
  def blockingCall[T](body: Blockable[T]): T

}

trait Task[T] {

  def start(): Unit
  def future: Future[T]

}

/* DONE: The challenge is to make ForkJoinPromise inherit from RecursiveAction
 * to avoid an object allocation per promise. This requires turning DefaultPromise
 * into a trait, i.e., removing its constructor parameters.
 */
private[concurrent] class ForkJoinTaskImpl[T](context: ForkJoinExecutionContext, body: () => T, within: Timeout) extends FJTask[T] with Task[T] {

  val timeout = within
  implicit val dispatcher = context

  // body of RecursiveTask
  def compute(): T =
    body()

  def start(): Unit =
    fork()

  def future: Future[T] = {
    null
  }

  // TODO FIXME: handle timeouts
  def await(atMost: Duration): this.type =
    await

  def await: this.type = {
    this.join()
    this
  }

  def tryCancel(): Unit =
    tryUnfork()
}

private[concurrent] final class ForkJoinExecutionContext extends ExecutionContext {
  val pool = new ForkJoinPool

  @inline
  private def executeForkJoinTask(task: RecursiveAction) {
    if (Thread.currentThread.isInstanceOf[ForkJoinWorkerThread])
      task.fork()
    else
      pool execute task
  }

  def execute(task: Runnable) {
    val action = new RecursiveAction { def compute() { task.run() } }
    executeForkJoinTask(action)
  }
  
  def makeTask[T](body: () => T)(implicit timeout: Timeout): Task[T] = {
    new ForkJoinTaskImpl(this, body, timeout)
  }
  
  def makePromise[T](timeout: Timeout): Promise[T] =
    null
  
  def blockingCall[T](body: Blockable[T]): T =
    body.block()(CanBlockEvidence)

}

/**
 * Implements a blocking execution context
 */
/*
private[concurrent] class BlockingExecutionContext extends ExecutionContext {
  //val pool = makeCachedThreadPool // TODO FIXME: need to merge thread pool factory methods from Heather's parcolls repo

  def execute(task: Runnable) {
    /* TODO
    val p = newPromise(task.run())
    p.start()
    pool execute p
    */
  }

  // TODO FIXME: implement
  def newPromise[T](body: => T): Promise[T] = {
    throw new Exception("not yet implemented")
  }
}
*/

object ExecutionContext {

  lazy val forNonBlocking = new ForkJoinExecutionContext

  //lazy val forBlocking = new BlockingExecutionContext

}
