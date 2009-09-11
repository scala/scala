package scala.concurrent

import java.lang.Thread

/** The <code>ThreadRunner</code> trait...
 *
 *  @author Philipp Haller
 */
class ThreadRunner extends FutureTaskRunner {

  type Task[T] = () => T
  type Future[T] = () => T

  implicit def functionAsTask[S](fun: () => S): Task[S] = fun
  implicit def futureAsFunction[S](x: Future[S]): () => S = x

  def execute[S](task: Task[S]) {
    val runnable = new Runnable {
      def run() { tryCatch(task()) }
    }
    (new Thread(runnable)).start()
  }

  def submit[S](task: Task[S]): Future[S] = {
    val result = new SyncVar[Either[Exception, S]]
    val runnable = new Runnable {
      def run() { result set tryCatch(task()) }
    }
    (new Thread(runnable)).start()
    () => ops getOrThrow result.get
  }

  def managedBlock(blocker: ManagedBlocker) {
    blocker.block()
  }

  def shutdown() {
    // do nothing
  }

}
