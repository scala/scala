/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package backend

import util.ClassPath

/** The platform dependent pieces of Global.
 */
trait Platform[T] {
  val global: Global
  import global._

  /** The compiler classpath. */
  def classPath: ClassPath[T]

  /** The root symbol loader. */
  def rootLoader: LazyType

  /** Any platform-specific phases. */
  def platformPhases: List[SubComponent]

  /** Symbol for a method which compares two objects. */
  def externalEquals: Symbol

  /** The various ways a boxed primitive might materialize at runtime. */
  def isMaybeBoxed(sym: Symbol): Boolean
}

