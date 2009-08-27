package scala.concurrent

import java.util.concurrent.{ExecutorService, Executor}

/** The <code>JavaConversions</code> object...
 *
 *  @author Philipp Haller
 */
object JavaConversions {

  implicit def asTaskRunner(exec: ExecutorService): TaskRunner[Unit] =
    new ThreadPoolRunner[Unit] {
      override protected def executor =
        exec

      def shutdown() =
        exec.shutdown()
    }

  implicit def asTaskRunner(exec: Executor): TaskRunner[Unit] =
    new TaskRunner[Unit] {
      type Future[+R] = () => R

      def submit(task: () => Unit): this.Future[Unit] = {
        val result = new SyncVar[Either[Throwable, Unit]]
        val runnable = new Runnable {
          def run() { result set tryCatch(task()) }
        }
        exec.execute(runnable)
        () => ops getOrThrow result.get
      }

      def managedBlock(blocker: ManagedBlocker) {
        blocker.block()
      }

      def shutdown() {
        // do nothing
      }
    }
}
