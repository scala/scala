/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.partest.utils

import scala.util.PropertiesTrait

/** A utility to load the library properties from a Java properties file
 *  included in the jar.
 *
 *  @author Stephane Micheloud
 */
object Properties extends PropertiesTrait {
  protected def propCategory    = "partest"
  protected def pickJarBasedOn  = classOf[Application]

  // XXX unlikely it's intentional that only partest uses ISO-8859-1
  override val encodingString = prop("file.encoding", "ISO-8859-1")
}
