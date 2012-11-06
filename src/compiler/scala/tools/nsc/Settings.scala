/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import settings.MutableSettings

/** A compatibility stub.
 */
class Settings(errorFn: String => Unit) extends MutableSettings(errorFn) {
  def this() = this(Console.println)

  override def withErrorFn(errorFn: String => Unit): Settings = {
    val settings = new Settings(errorFn)
    copyInto(settings)
    settings
  }
}
