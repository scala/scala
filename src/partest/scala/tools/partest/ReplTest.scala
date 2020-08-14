/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.partest

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.{ILoop, replProps}
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

/** Test code or commands in a REPL.
 *
 *  Just call `show` to print the result of `eval()` your `code`.
 *
 *  Optionally, `transformSettings` or redefine `normalize`.
 *
 *  The test filters the line of output that mentions a version number.
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
        case s => s + java.io.File.pathSeparator + testOutput.toString
      }
      s.usejavacp.value = true
    }
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
    val transcript = ILoop.runForTranscript(code, s, inSession = inSession)
    log(s"transcript[[$transcript]]")
    val lines = transcript.linesIterator
    val clean =
      if (welcoming) {
        val welcome = "(Welcome to Scala).*".r
        lines map {
          case welcome(s) => s
          case s          => s
        }
      } else {
        lines.drop(header.linesIterator.size)
      }
    clean.map(normalize)
  }
  def show() = eval() foreach println
}

/** Retain and normalize the welcome message. */
trait Welcoming { this: ReplTest =>
  override def welcoming = true
}

/** Strip Any.toString's id@abcdef16 hashCodes. These are generally at end of result lines. */
trait Hashless extends ReplTest {
  import Hashless._
  override def normalize(s: String) = stripIdentityHashCode(super.normalize(s))
}
object Hashless {
  private val hashless = "@[a-fA-F0-9]+".r
  private def stripIdentityHashCode(s: String): String = hashless.replaceAllIn(s, "@XXXXXXXX")
}

/** Strip dynamic parts of LambdaMetafactory synthetic class names. */
trait Lambdaless extends ReplTest {
  import Lambdaless._
  override def normalize(s: String) = stripLambdaClassName(super.normalize(s))
}
object Lambdaless {
  private val lambdaless = """\$Lambda\$\d+/(?:0x[a-f0-9]{16}|\d+)(@[a-fA-F0-9]+)?""".r
  private def stripLambdaClassName(s: String): String = lambdaless.replaceAllIn(s, Regex.quoteReplacement("<function>"))
}

/** Normalize a REPL stack trace by stripping line numbers and count of elided frames. */
trait StackCleaner extends ReplTest {
  import StackCleaner._
  override def normalize(s: String) = stripFrameCount(super.normalize(s))
}
object StackCleaner {
  private val elidedAndMore = """(\s+\.{3} )\d+( elided and )\d+( more)""".r
  private val elidedOrMore        = """(\s+\.{3} )\d+( (?:elided|more))""".r
  private val frame         = """(\s+at [^(]+\(<console>:)\d+(\))""".r
  private def stripFrameCount(line: String) = line match {
    case elidedAndMore(ellipsis, infix, suffix) => s"$ellipsis???$infix???$suffix" // must be before `elided`
    case elidedOrMore(ellipsis, suffix) => s"$ellipsis???$suffix"
    case frame(prefix, suffix)    => s"${prefix}XX${suffix}"
    case s                        => s
  }
}

/** Run a REPL test from a session transcript.
 *
 *  The `session` is read from the `.check` file,
 *  and the `code` is the session stripped of output text.
 *
 *  By default, output is compared against the check file
 *  as usual. Optionally, redefine `show` to use `checkSession()`,
 *  which compares `eval()` result to `expected`.
 *
 *  Redefine `session` if it must be constructed dynamically.
 *  Redefine `show` to use `checkSession` to postprocess
 *  the `expected` text and/or the result of `eval()`.
 *
 */
abstract class SessionTest extends ReplTest  {
  /** Session transcript. */
  def session: String = testPath.changeExtension("check").toFile.slurp

  /** Expected output, as an iterator, optionally marginally stripped. */
  def expected = if (stripMargins) session.stripMargin.linesIterator else session.linesIterator

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
  // \R for line break since Java 8
  private def input(prompt: String) = raw"""(?m)^$prompt(:pa.*\R)// Entering paste mode.*\R\R((?:.*\R)*)\R// Exiting paste mode.*\R|^scala> (.*\R(?:\s*\| .*\R)*)""".r

  private val margin = """(?m)^\s*\| (.*)$""".r
}
