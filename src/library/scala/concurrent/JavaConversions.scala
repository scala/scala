/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



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
