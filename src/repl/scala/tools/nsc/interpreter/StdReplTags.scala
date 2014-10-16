/* NSC -- new Scala compiler
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package interpreter

import scala.tools.reflect.StdTags
import scala.reflect.runtime.{ universe => ru }

trait StdReplTags extends StdTags {
  lazy val tagOfStdReplVals = tagOfStaticClass[StdReplVals]
  lazy val tagOfIMain = tagOfStaticClass[IMain]
}

object StdReplTags extends StdTags with StdReplTags {
  val u: ru.type = ru
  val m = u.runtimeMirror(getClass.getClassLoader)
}
