/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.tools.partest
package utils

/** Loads scala-partest.properties from the jar. */
object Properties extends scala.util.PropertiesTrait {
  protected def propCategory    = "scala-partest"
  protected def pickJarBasedOn  = classOf[nest.Runner]
  override def isAvian = super.isAvian
}
