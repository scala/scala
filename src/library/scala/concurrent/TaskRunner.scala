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

  /** If expression computed successfully return it in <code>Left</code>,
   *  otherwise return exception in <code>Right</code>.
   */
  protected def tryCatch[A](left: => A): Either[A, Exception] = {
    try {
      Left(left)
    } catch {
      case e: Exception => Right(e)
    }
  }

}
