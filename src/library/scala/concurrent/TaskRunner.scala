package scala.concurrent

/** The <code>TaskRunner</code> trait...
 *
 *  @author Philipp Haller
 */
trait TaskRunner[T] extends AsyncInvokable[() => T, T] {

  def submit(task: () => T): Future[T]

  def shutdown(): Unit

  def !!(task: () => T): Future[T] =
    submit(task)

  def managedBlock(blocker: ManagedBlocker): Unit

  /** If expression computed successfully return it in <code>Right</code>,
   *  otherwise return exception in <code>Left</code>.
   */
  protected def tryCatch[A](body: => A): Either[Exception, A] =
    ops tryCatchEx body
}
