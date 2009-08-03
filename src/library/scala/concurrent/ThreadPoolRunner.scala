package scala.concurrent

import java.util.concurrent.{ExecutorService, Callable, TimeUnit}

import scala.annotation.unchecked.uncheckedVariance

/** The <code>ThreadPoolRunner</code> trait...
 *
 *  @author Philipp Haller
 */
trait ThreadPoolRunner[T] extends TaskRunner[T] {

  type Future[+R] = RichFuture[R]

  trait RichFuture[+S] extends java.util.concurrent.Future[S @uncheckedVariance]
                          with (() => S)

  protected def executor: ExecutorService

  def submit(task: () => T): this.Future[T] = {
    val callable = new Callable[T] {
      def call() = task()
    }
    toRichFuture(executor.submit[T](callable))
  }

  def execute(task: Runnable): Unit =
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
