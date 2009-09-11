package scala.concurrent

/** The <code>TaskRunner</code> trait...
 *
 *  @author Philipp Haller
 */
trait TaskRunner {

  type Task[T]

  implicit def functionAsTask[S](fun: () => S): Task[S]

  def execute[S](task: Task[S]): Unit

  def shutdown(): Unit

  /** If expression computed successfully return it in <code>Right</code>,
   *  otherwise return exception in <code>Left</code>.
   */
  protected def tryCatch[A](body: => A): Either[Exception, A] =
    ops tryCatchEx body
}
