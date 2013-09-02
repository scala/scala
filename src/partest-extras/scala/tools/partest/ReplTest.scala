/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
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
    // s.Yreplsync.value = true
    s.Xnojline.value = true
    transformSettings(s)
  }
  def eval() = {
    val s = settings
    log("eval(): settings = " + s)
    ILoop.runForTranscript(code, s).lines drop 1
  }
  def show() = eval() foreach println
}

/** Run a REPL test from a session transcript.
 *  The `session` should be a triple-quoted String starting
 *  with the `Type in expressions` message and ending
 *  after the final `prompt`, including the last space.
 */
abstract class SessionTest extends ReplTest  {
  def session: String
  override final def code = expected filter (_.startsWith(prompt)) map (_.drop(prompt.length)) mkString "\n"
  def expected = session.stripMargin.lines.toList
  final def prompt = "scala> "
  override def show() = {
    val out = eval().toList
    if (out.size != expected.size) Console println s"Expected ${expected.size} lines, got ${out.size}"
    if (out != expected) Console print nest.FileManager.compareContents(expected, out, "expected", "actual")
  }
}
