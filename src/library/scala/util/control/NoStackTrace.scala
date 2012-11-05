/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package util.control

/** A trait for exceptions which, for efficiency reasons, do not
 *  fill in the stack trace.  Stack trace suppression can be disabled
 *  on a global basis via a system property wrapper in
 *  [[scala.sys.SystemProperties]].
 *
 *  @author   Paul Phillips
 *  @since    2.8
 */
trait NoStackTrace extends Throwable {
  override def fillInStackTrace(): Throwable =
    if (NoStackTrace.noSuppression) super.fillInStackTrace()
    else this
}

object NoStackTrace {
  final def noSuppression = _noSuppression

  // two-stage init to make checkinit happy, since sys.SystemProperties.noTraceSupression.value calls back into NoStackTrace.noSuppression
  final private var _noSuppression = false
  _noSuppression = sys.SystemProperties.noTraceSupression.value
}
