/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package nsc
package settings

import util.ClassPath

class FscSettings(error: String => Unit) extends Settings(error) {
  outer =>

  def this() = this(Console.println)

  val reset    = BooleanSetting("-reset",    "Reset compile server caches")
  val shutdown = BooleanSetting("-shutdown", "Shutdown compile server")
  val server   = StringSetting ("-server",   "hostname:portnumber", "Specify compile server socket", "")

  disable(prompt)
  disable(resident)

  // Make all paths absolute since we may be going from client to server
  userSetSettings foreach {
    case x: PathSetting => x.value = ClassPath.makeAbsolute(x.value)
    case _              => ()
  }
}
