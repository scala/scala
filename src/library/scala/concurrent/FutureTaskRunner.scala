/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import language.{implicitConversions, higherKinds}

/** The `FutureTaskRunner</code> trait is a base trait of task runners
 *  that provide some sort of future abstraction.
 *
 *  @author Philipp Haller
 */
@deprecated("Use `ExecutionContext` instead.", "2.10.0")
trait FutureTaskRunner extends TaskRunner {

  /** The type of the futures that the underlying task runner supports.
   */
  type Future[T]

  /** An implicit conversion from futures to zero-parameter functions.
   */
  implicit def futureAsFunction[S](x: Future[S]): () => S

  /** Submits a task to run which returns its result in a future.
   */
  def submit[S](task: Task[S]): Future[S]

   /* Possibly blocks the current thread, for example, waiting for
    * a lock or condition.
    */
  @deprecated("Use `blocking` instead.", "2.10.0")
  def managedBlock(blocker: ManagedBlocker): Unit

}
