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
  /** Session transcript, as a triple-quoted, multiline, marginalized string. */
  def session: String

  /** Expected output, as an iterator. */
  def expected = session.stripMargin.lines

  /** Code is the command list culled from the session (or the expected session output).
   *  Would be nicer if code were lazy lines.
   */
  override final def code = expected filter (_ startsWith prompt) map (_ drop prompt.length) mkString "\n"

  final def prompt = "scala> "

  /** Default test is to compare expected and actual output and emit the diff on a failed comparison. */
  override def show() = {
    val evaled = eval().toList
    val wanted = expected.toList
    if (evaled.size != wanted.size) Console println s"Expected ${wanted.size} lines, got ${evaled.size}"
    if (evaled != wanted) Console print nest.FileManager.compareContents(wanted, evaled, "expected", "actual")
  }
}
