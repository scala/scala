/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.control

/** A trait for exceptions which, for efficiency reasons, do not
 *  fill in the stack trace.  Stack trace suppression can be disabled
 *  on a global basis by setting the system property named at
 *  NoStackTrace.DisableProperty.
 *
 *  @author   Paul Phillips
 *  @since    2.8
 */
trait NoStackTrace extends Throwable {
  override def fillInStackTrace(): Throwable =
    if (sys.props contains NoStackTrace.DisableProperty) super.fillInStackTrace()
    else this
}

object NoStackTrace {
  // TODO: systematic naming scheme.
  final val DisableProperty = "scala.control.no-trace-suppression"
}
