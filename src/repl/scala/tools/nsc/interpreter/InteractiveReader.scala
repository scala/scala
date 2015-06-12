/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala.tools.nsc
package interpreter

import java.io.IOException
import session.History
import InteractiveReader._
import Properties.isMac

/** Reads lines from an input stream */
trait InteractiveReader {
  def postInit(): Unit = {}

  val interactive: Boolean

  def reset(): Unit
  def history: History
  def completion: Completion
  def redrawLine(): Unit

  def readYesOrNo(prompt: String, alt: => Boolean): Boolean = readOneKey(prompt) match {
    case 'y'  => true
    case 'n'  => false
    case -1   => false // EOF
    case _    => alt
  }

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
  @deprecated("Use `apply` instead.", "2.9.0")
  def createDefault(): InteractiveReader = apply() // used by sbt
}

