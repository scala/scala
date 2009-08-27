package scala.concurrent

import java.lang.Thread

/** The <code>ThreadRunner</code> trait...
 *
 *  @author Philipp Haller
 */
class ThreadRunner[T] extends TaskRunner[T] {

  type Future[+S] = () => S

  def submit(task: () => T): this.Future[T] = {
    val result = new SyncVar[Either[Exception, T]]
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
