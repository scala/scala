/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
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

  protected def readOneLine(prompt: String): String
  val interactive: Boolean
  def init(): Unit = ()
  def currentLine = ""    // the current buffer contents, if available

  def readLine(prompt: String): String = {
    def handler: Catcher[String] = {
      case e: ClosedByInterruptException          => system.error("Reader closed by interrupt.")
      // Terminal has to be re-initialized after SIGSTP or up arrow etc. stop working.
      case e: IOException if restartSystemCall(e) => init() ; readLine(prompt)
    }
    catching(handler) { readOneLine(prompt) }
  }

  // override if history is available
  def history: Option[History] = None
  def historyList = history map (_.asList) getOrElse Nil

  // override if completion is available
  def completion: Option[Completion] = None

  // hack necessary for OSX jvm suspension because read calls are not restarted after SIGTSTP
  private def restartSystemCall(e: Exception): Boolean =
    Properties.isMac && (e.getMessage == msgEINTR)
}

object InteractiveReader {
  val msgEINTR = "Interrupted system call"
  def createDefault(): InteractiveReader = createDefault(null)

  /** Create an interactive reader.  Uses <code>JLineReader</code> if the
   *  library is available, but otherwise uses a <code>SimpleReader</code>.
   */
  def createDefault(interpreter: Interpreter): InteractiveReader =
    try new JLineReader(interpreter)
    catch {
      case e @ (_: Exception | _: NoClassDefFoundError) =>
        // println("Failed to create JLineReader(%s): %s".format(interpreter, e))
        new SimpleReader
    }
}

