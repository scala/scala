/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.net

import java.net.{ URL, MalformedURLException }
import scala.util.control.Exception._

/** Skeleton in anticipation of more convenience methods. */
object Utility
{
  def parseURL(s: String): Option[URL] =
    catching(classOf[MalformedURLException]) opt new URL(s)
}