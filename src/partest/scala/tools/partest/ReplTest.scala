/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.partest

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.ILoop
import java.lang.reflect.{ Method => JMethod, Field => JField }

/** A trait for testing repl code.  It drops the first line
 *  of output because the real repl prints a version number.
 */
abstract class ReplTest extends App {
  def code: String
  // override to add additional settings with strings
  def extraSettings: String = ""
  // override to transform Settings object immediately before the finish
  def transformSettings(s: Settings): Settings = s

  // final because we need to enforce the existence of a couple settings.
  final def settings: Settings = {
    val s = new Settings
    s.Yreplsync.value = true
    s.Xnojline.value = true
    val settingString = sys.props("scala.partest.debug.repl-args") match {
      case null   => extraSettings
      case s      => extraSettings + " " + s
    }
    s processArgumentString settingString
    transformSettings(s)
  }
  def eval() = ILoop.runForTranscript(code, settings).lines drop 1
  def show() = eval() foreach println

  try show()
  catch { case t => println(t) ; sys.exit(1) }
}
