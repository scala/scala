/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.partest

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.shell.ILoop
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

/** A class for testing repl code.
 *  It filters the line of output that mentions a version number.
 */
abstract class ReplTest extends DirectTest {
  // override to transform Settings object immediately before the finish
  def transformSettings(s: Settings): Settings = s
  // final because we need to enforce the existence of a couple settings.
  final override def settings: Settings = {
    val s = super.settings
    s.Xnojline.value = true
    if (getClass.getClassLoader.getParent != null) {
      s.classpath.value = s.classpath.value match {
        case "" => testOutput.toString
        case s => s + ":" + testOutput.toString
      }
      s.usejavacp.value = true
    }
    transformSettings(s)
  }
  def normalize(s: String) = s
  /** True for SessionTest to preserve session text. */
  def inSession: Boolean = false
  def eval() = {
    val s = settings
    log("eval(): settings = " + s)
    val transcript = ILoop.runForTranscript(code, s, inSession = inSession)
    log(s"transcript[[$transcript]]")
    transcript.lines.map(normalize)
  }
  def show() = eval() foreach println
}


/** Run a REPL test from a session transcript.
 *  The `session` is read from the `.check` file.
 */
abstract class SessionTest extends ReplTest  {
  /** Session transcript. */
  def session: String = testPath.changeExtension("check").toFile.slurp

  /** Expected output, as an iterator, optionally marginally stripped. */
  def expected = if (stripMargins) session.stripMargin.lines else session.lines

  /** Override with true if session is a """string""" with margin indent. */
  def stripMargins: Boolean = false

  /** Analogous to stripMargins, don't mangle continuation lines on echo. */
  override def inSession: Boolean = true

  /** Code is the command list culled from the session (or the expected session output).
   *  Would be nicer if code were lazy lines so you could generate arbitrarily long text.
   *  Retain user input: prompt lines and continuations, without the prefix; or pasted text plus ctrl-D.
   */
  import SessionTest._
  lazy val pasted = input(prompt)
  override final def code = pasted.findAllMatchIn(expected.mkString("", "\n", "\n")).map {
    case pasted(null, null, prompted) =>
      def continued(m: Match): Option[String] = m match {
        case margin(text) => Some(Regex.quoteReplacement(text))
        case _            => None
      }
      margin.replaceSomeIn(prompted, continued)
    case pasted(cmd, pasted, null) =>
      cmd + pasted + "\u0004"
  }.mkString

  // Just the last line of the interactive prompt
  def prompt = "scala> "

  /** When overriding show, facilitate the usual check, comparing session to eval result. */
  def checkSession(): Unit = {
    val evaled = eval().toList
    val wanted = expected.toList
    if (evaled.size != wanted.size) Console.println(s"Expected ${wanted.size} lines, got ${evaled.size}")
    if (evaled != wanted) Console.print(nest.FileManager.compareContents(wanted, evaled, "expected", "actual"))
  }
}
object SessionTest {
  // \R for line break is Java 8, \v for vertical space might suffice
  def input(prompt: String) = s"""(?m)^$prompt(:pa.*\u000A)// Entering paste mode.*\u000A\u000A((?:.*\u000A)*)\u000A// Exiting paste mode.*\u000A|^scala> (.*\u000A(?:\\s*\\| .*\u000A)*)""".r

  val margin = """(?m)^\s*\| (.*)$""".r
}
