/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.partest

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.{ ILoop, replProps }
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
    transformSettings(s)
  }
  def normalize(s: String) = s
  /** True for SessionTest to preserve session text. */
  def inSession: Boolean = false
  /** True to preserve welcome header, eliding version number. */
  def welcoming: Boolean = false
  lazy val header = replProps.welcome
  def eval() = {
    val s = settings
    log("eval(): settings = " + s)
    val lines = ILoop.runForTranscript(code, s, inSession = inSession).lines
    (if (welcoming) {
      val welcome = "(Welcome to Scala).*".r
      //val welcome = Regex.quote(header.lines.next).r
      //val version = "(.*version).*".r   // version on separate line?
      //var inHead  = false
      lines map {
        //case s @ welcome()        => inHead = true  ; s
        //case version(s) if inHead => inHead = false ; s
        case welcome(s) => s
        case s          => s
      }
    } else {
      lines drop header.lines.size
    }) map normalize
  }
  def show() = eval() foreach println
}

/** Retain and normalize the welcome message. */
trait Welcoming { this: ReplTest =>
  override def welcoming = true
}

/** Run a REPL test from a session transcript.
 *  The `session` should be a triple-quoted String starting
 *  with the `Type in expressions` message and ending
 *  after the final `prompt`, including the last space.
 */
abstract class SessionTest extends ReplTest  {
  /** Session transcript, as a triple-quoted, multiline, marginalized string. */
  def session: String

  /** Expected output, as an iterator, optionally marginally stripped. */
  def expected = if (stripMargins) session.stripMargin.lines else session.lines

  /** Override with false if we should not strip margins because of leading continuation lines. */
  def stripMargins: Boolean = true

  /** Analogous to stripMargins, don't mangle continuation lines on echo. */
  override def inSession: Boolean = true

  /** Code is the command list culled from the session (or the expected session output).
   *  Would be nicer if code were lazy lines so you could generate arbitrarily long text.
   *  Retain user input: prompt lines and continuations, without the prefix; or pasted text plus ctl-D.
   */
  import SessionTest._
  lazy val pasted = input(prompt)
  override final def code = pasted findAllMatchIn (expected mkString ("", "\n", "\n")) map {
    case pasted(null, null, prompted) =>
      def continued(m: Match): Option[String] = m match {
        case margin(text) => Some(text)
        case _            => None
      }
      margin.replaceSomeIn(prompted, continued)
    case pasted(cmd, pasted, null) =>
      cmd + pasted + "\u0004"
  } mkString

  // Just the last line of the interactive prompt
  def prompt = "scala> "

  /** Default test is to compare expected and actual output and emit the diff on a failed comparison. */
  override def show() = {
    val evaled = eval().toList
    val wanted = expected.toList
    if (evaled.size != wanted.size) Console println s"Expected ${wanted.size} lines, got ${evaled.size}"
    if (evaled != wanted) Console print nest.FileManager.compareContents(wanted, evaled, "expected", "actual")
  }
}
object SessionTest {
  // \R for line break is Java 8, \v for vertical space might suffice
  def input(prompt: String) = s"""(?m)^$prompt(:pa.*\u000A)// Entering paste mode.*\u000A\u000A((?:.*\u000A)*)\u000A// Exiting paste mode.*\u000A|^scala> (.*\u000A(?:\\s*\\| .*\u000A)*)""".r

  val margin = """(?m)^\s*\| (.*)$""".r
}
