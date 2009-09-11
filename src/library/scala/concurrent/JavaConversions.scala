package scala.concurrent

import java.util.concurrent.{ExecutorService, Executor}

/** The <code>JavaConversions</code> object...
 *
 *  @author Philipp Haller
 */
object JavaConversions {

  implicit def asTaskRunner(exec: ExecutorService): FutureTaskRunner =
    new ThreadPoolRunner {
      override protected def executor =
        exec

      def shutdown() =
        exec.shutdown()
    }

  implicit def asTaskRunner(exec: Executor): TaskRunner =
    new TaskRunner {
      type Task[T] = Runnable

      implicit def functionAsTask[T](fun: () => T): Task[T] = new Runnable {
        def run() { fun() }
      }

      def execute[S](task: Task[S]) {
        exec.execute(task)
      }

      def managedBlock(blocker: ManagedBlocker) {
        blocker.block()
      }

      def shutdown() {
        // do nothing
      }
    }
}
