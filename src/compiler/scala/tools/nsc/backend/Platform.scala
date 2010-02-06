/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package backend

import util.{ ClassPath }

/** The platform dependent pieces of Global.
 */

trait Platform[T] {
  val global: Global
  import global._

  // Unless inlining is on, we can exclude $class.class files from the classpath.
  protected def isInlinerOn = settings.inline.value

  def classPath: ClassPath[T]
  def rootLoader: global.LazyType
  def platformPhases: List[SubComponent]
}
