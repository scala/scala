package scala.concurrent

/** The <code>AsyncInvokable</code> trait...
 *
 *  @author Philipp Haller
 */
trait AsyncInvokable[-T, +R] {

  type Future[+S] <: () => S

  def !!(task: T): Future[R]

}
