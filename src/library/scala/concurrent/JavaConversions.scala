/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import java.util.concurrent.{ExecutorService, Executor}
import language.implicitConversions

/** The `JavaConversions` object provides implicit converstions supporting
 *  interoperability between Scala and Java concurrency classes.
 *
 *  @author Philipp Haller
 */
object JavaConversions {

  @deprecated("Use `asExecutionContext` instead.", "2.10.0")
  implicit def asTaskRunner(exec: ExecutorService): FutureTaskRunner =
    new ThreadPoolRunner {
      override protected def executor =
        exec

      def shutdown() =
        exec.shutdown()
    }

  @deprecated("Use `asExecutionContext` instead.", "2.10.0")
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

  /**
   * Creates a new `ExecutionContext` which uses the provided `ExecutorService`.
   */
  implicit def asExecutionContext(exec: ExecutorService): ExecutionContextExecutorService =
    ExecutionContext.fromExecutorService(exec)

  /**
   * Creates a new `ExecutionContext` which uses the provided `Executor`.
   */
  implicit def asExecutionContext(exec: Executor): ExecutionContextExecutor =
    ExecutionContext.fromExecutor(exec)

}
