/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package runtime

import scala.compat.Platform.EOL

@deprecated("use Throwable#getStackTrace", "2.11.0")
final class RichException(exc: Throwable) {
  def getStackTraceString = exc.getStackTrace().mkString("", EOL, EOL)
}
