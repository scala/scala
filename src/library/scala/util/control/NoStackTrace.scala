/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.control

/** A trait for exceptions which, for efficiency reasons, do not
 *  fill in the stack trace.
 *
 *  @author   Paul Phillips
 *  @since    2.8
 */
trait NoStackTrace extends Throwable
{
  override def fillInStackTrace(): Throwable = this
}
