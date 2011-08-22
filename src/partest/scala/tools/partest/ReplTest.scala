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
abstract class ReplTest extends DirectTest {
  // override to transform Settings object immediately before the finish
  def transformSettings(s: Settings): Settings = s
  // final because we need to enforce the existence of a couple settings.
  final override def settings: Settings = {
    val s = super.settings
    s.Yreplsync.value = true
    s.Xnojline.value = true
    transformSettings(s)
  }
  def eval() = ILoop.runForTranscript(code, settings).lines drop 1
  def show() = eval() foreach println
}
