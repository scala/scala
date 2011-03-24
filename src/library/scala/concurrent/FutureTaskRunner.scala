package scala.concurrent

/** The <code>FutureTaskRunner</code> trait is a base trait of task runners
 *  that provide some sort of future abstraction.
 *
 *  @author Philipp Haller
 */
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
  def managedBlock(blocker: ManagedBlocker): Unit

}
