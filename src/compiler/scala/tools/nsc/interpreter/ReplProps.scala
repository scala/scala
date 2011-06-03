/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import scala.sys._

class ReplProps {
  private def bool(name: String) = BooleanProp.keyExists(name)

  val jlineDebug = bool("scala.tools.jline.internal.Log.debug")
  val jlineTrace = bool("scala.tools.jline.internal.Log.trace")

  val info  = bool("scala.repl.info")
  val debug = bool("scala.repl.debug")
  val trace = bool("scala.repl.trace")
  val power = bool("scala.repl.power")

  val replInitCode  = Prop[JFile]("scala.repl.initcode")
  val powerInitCode = Prop[JFile]("scala.repl.power.initcode")
  val powerBanner   = Prop[JFile]("scala.repl.power.banner")
}
