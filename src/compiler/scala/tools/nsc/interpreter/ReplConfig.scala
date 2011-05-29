/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import sys.{ Prop, BooleanProp }

trait ReplConfig {
  class ReplProps {
    private def bool(name: String) = BooleanProp.keyExists(name)

    val jlineDebug = bool("scala.tools.jline.internal.Log.debug")
    val jlineTrace = bool("scala.tools.jline.internal.Log.trace")

    val debug = bool("scala.repl.debug")
    val trace = bool("scala.repl.trace")
    val power = bool("scala.repl.power")

    val replInitCode  = Prop[JFile]("scala.repl.initcode")
    val powerInitCode = Prop[JFile]("scala.repl.power.initcode")
    val powerBanner   = Prop[JFile]("scala.repl.power.banner")
  }
  lazy val replProps = new ReplProps

  /** Debug output */
  private[nsc] def repldbg(msg: => String)   =
    if (isReplDebug) {
      try Console println msg
      catch { case x: AssertionError => Console.println("Assertion error in repldbg:\n  " + x) }
    }

  private[nsc] def repltrace(msg: => String) =
    if (isReplTrace) {
      try Console println msg
      catch { case x: AssertionError => Console.println("Assertion error in repltrace:\n  " + x) }
    }

  def isReplTrace: Boolean = replProps.trace
  def isReplDebug: Boolean = replProps.debug || isReplTrace
  def isReplPower: Boolean = replProps.power
}
