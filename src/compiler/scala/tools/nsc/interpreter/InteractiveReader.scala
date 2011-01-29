/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala.tools.nsc
package interpreter

import java.io.IOException
import java.nio.channels.ClosedByInterruptException
import scala.util.control.Exception._
import InteractiveReader._

/** Reads lines from an input stream */
trait InteractiveReader {
  val interactive: Boolean
  protected def readOneLine(prompt: String): String

  def history: History
  def completion: Completion
  def init(): Unit
  def reset(): Unit

  def redrawLine(): Unit = ()
  def currentLine = ""    // the current buffer contents, if available

  def readLine(prompt: String): String = {
    def handler: Catcher[String] = {
      case e: ClosedByInterruptException          => sys.error("Reader closed by interrupt.")
      // Terminal has to be re-initialized after SIGSTP or up arrow etc. stop working.
      case e: IOException if restartSystemCall(e) => reset() ; readLine(prompt)
    }
    catching(handler) { readOneLine(prompt) }
  }

  // hack necessary for OSX jvm suspension because read calls are not restarted after SIGTSTP
  private def restartSystemCall(e: Exception): Boolean =
    Properties.isMac && (e.getMessage == msgEINTR)
}

object InteractiveReader {
  val msgEINTR = "Interrupted system call"
  def apply(): InteractiveReader = new SimpleReader

  // @deprecated("Use `apply` instead") def createDefault(intp: IMain): InteractiveReader = apply(intp)
  // @deprecated("Use `apply` instead") def createDefault(comp: Completion): InteractiveReader = apply(comp)
}
