/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala.tools.nsc
package interpreter

import java.io.IOException
import java.nio.channels.ClosedByInterruptException
import scala.util.control.Exception._
import session.History
import InteractiveReader._
import Properties.isMac

/** Reads lines from an input stream */
trait InteractiveReader {
  val interactive: Boolean

  def init(): Unit
  def reset(): Unit

  def history: History
  def completion: Completion
  def keyBindings: List[KeyBinding]
  def eraseLine(): Unit
  def redrawLine(): Unit
  def currentLine: String

  def readYesOrNo(prompt: String) = readOneKey(prompt) match {
    case 'y'  => true
    case 'n'  => false
  }
  def readAssumingNo(prompt: String)  = try readYesOrNo(prompt) catch { case _: MatchError  => false }
  def readAssumingYes(prompt: String) = try readYesOrNo(prompt) catch { case _: MatchError  => true }

  protected def readOneLine(prompt: String): String
  protected def readOneKey(prompt: String): Int

  def readLine(prompt: String): String =
    // hack necessary for OSX jvm suspension because read calls are not restarted after SIGTSTP
    if (isMac) restartSysCalls(readOneLine(prompt), reset())
    else readOneLine(prompt)
}

object InteractiveReader {
  val msgEINTR = "Interrupted system call"
  def restartSysCalls[R](body: => R, reset: => Unit): R =
    try body catch {
      case e: IOException if e.getMessage == msgEINTR => reset ; body
    }

  def apply(): InteractiveReader = SimpleReader()
  @deprecated("Use `apply` instead.")
  def createDefault(): InteractiveReader = apply()
}

