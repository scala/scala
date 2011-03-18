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

  val reset        = BooleanSetting("-reset",    "Reset compile server caches")
  val shutdown     = BooleanSetting("-shutdown", "Shutdown compile server")
  val server       = StringSetting ("-server",   "hostname:portnumber", "Specify compile server socket", "")
  val preferIPv4   = BooleanSetting("-ipv4", "Use IPv4 rather than IPv6 for the server socket")
  val absClasspath = BooleanSetting("-absolute-cp", "Make classpath elements absolute paths before sending to server") .
      withPostSetHook (_ => absolutizeClasspath())
  val idleMins     = IntSetting    ("-max-idle", "Set idle timeout in minutes for fsc (use 0 for no timeout)",
                                              30, Some(0, Int.MaxValue), (_: String) => None)

  disable(prompt)
  disable(resident)

  // Make the classpath absolute: for going from client to server.
  private def absolutizeClasspath() {
    userSetSettings collect {
      case x: PathSetting => x.value = ClassPath.makeAbsolute(x.value)
    }
  }
}
