package scala.concurrent

/** The <code>FutureTaskRunner</code> trait...
 *
 *  @author Philipp Haller
 */
trait FutureTaskRunner extends TaskRunner {

  type Future[T]

  implicit def futureAsFunction[S](x: Future[S]): () => S

  def submit[S](task: Task[S]): Future[S]

  def managedBlock(blocker: ManagedBlocker): Unit

}
