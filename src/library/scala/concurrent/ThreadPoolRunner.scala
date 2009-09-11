package scala.concurrent

import java.util.concurrent.{ExecutorService, Callable, TimeUnit}

/** The <code>ThreadPoolRunner</code> trait...
 *
 *  @author Philipp Haller
 */
trait ThreadPoolRunner extends FutureTaskRunner {

  type Task[T] = Callable[T] with Runnable
  type Future[T] = RichFuture[T]

  private class RunCallable[S](fun: () => S) extends Runnable with Callable[S] {
    def run() = fun()
    def call() = fun()
  }

  implicit def functionAsTask[S](fun: () => S): Task[S] =
    new RunCallable(fun)

  implicit def futureAsFunction[S](x: Future[S]): () => S =
    () => x.get()

  trait RichFuture[S] extends java.util.concurrent.Future[S]
                         with (() => S)

  protected def executor: ExecutorService

  def submit[S](task: Task[S]): Future[S] = {
    toRichFuture(executor.submit[S](task))
  }

  def execute[S](task: Task[S]): Unit =
    executor execute task

  def managedBlock(blocker: ManagedBlocker) {
    blocker.block()
  }

  private def toRichFuture[S](future: java.util.concurrent.Future[S]) =
    new RichFuture[S] {
      def cancel(mayInterrupt: Boolean) = future cancel mayInterrupt
      def get() = future.get()
      def get(timeout: Long, unit: TimeUnit) = future.get(timeout, unit)
      def isCancelled() = future.isCancelled()
      def isDone() = future.isDone()
      def apply() = future.get()
    }

}
