/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import settings.MutableSettings

/** A compatibility stub.
 */
class Settings(errorFn: String => Unit) extends MutableSettings(errorFn) {
  def this() = this(Console.println)
}
