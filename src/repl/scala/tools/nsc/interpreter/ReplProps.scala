/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import Properties.shellPromptString
import scala.sys._
import Prop._

class ReplProps {
  private def bool(name: String) = BooleanProp.keyExists(name)
  private def int(name: String)  = Prop[Int](name)

  // This property is used in TypeDebugging. Let's recycle it.
  val colorOk = bool("scala.color")

  val info  = bool("scala.repl.info")
  val debug = bool("scala.repl.debug")
  val trace = bool("scala.repl.trace")
  val power = bool("scala.repl.power")

  // Handy system prop for shell prompt, or else pick it up from compiler.properties
  val promptString = Prop[String]("scala.repl.prompt").option getOrElse (if (info) "%nscala %s> " else shellPromptString)
  val prompt = {
    import scala.io.AnsiColor.{ MAGENTA, RESET }
    val p = promptString format Properties.versionNumberString
    if (colorOk) s"$MAGENTA$p$RESET" else p
  }

  /** CSV of paged,across to enable pagination or `-x` style
   *  columns, "across" instead of down the column.  Since
   *  pagination turns off columnar output, these flags are
   *  currently mutually exclusive.
   */
  val format = Prop[String]("scala.repl.format")

  val replAutorunCode = Prop[JFile]("scala.repl.autoruncode")
  val powerInitCode   = Prop[JFile]("scala.repl.power.initcode")
  val powerBanner     = Prop[JFile]("scala.repl.power.banner")

  val vids = bool("scala.repl.vids")
  val maxPrintString = int("scala.repl.maxprintstring")
}
