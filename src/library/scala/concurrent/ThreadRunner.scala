package scala.concurrent

import java.lang.Thread

/** The <code>ThreadRunner</code> trait...
 *
 *  @author Philipp Haller
 */
class ThreadRunner[T] extends TaskRunner[T] {

  type Future[+S] = () => S

  def submit(task: () => T): this.Future[T] = {
    val result = new SyncVar[Either[T, Exception]]
    val runnable = new Runnable {
      def run() { result set tryCatch(task()) }
    }
    (new Thread(runnable)).start()
    () => result.get match {
      case Left(a) => a
      case Right(t) => throw t
    }
  }

  def managedBlock(blocker: ManagedBlocker) {
    blocker.block()
  }

  def shutdown() {
    // do nothing
  }

}
