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
  // override to add additional settings
  def extraSettings: String = ""
  def settings: Settings = {
    val s = new Settings
    s processArgumentString extraSettings
    s
  }
  def eval() = ILoop.runForTranscript(code, settings).lines drop 1
  def show() = eval() foreach println

  show()
}
