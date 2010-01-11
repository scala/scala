/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Stepan Koltsov
 */
// $Id$

package scala.tools.nsc
package interpreter
import scala.util.control.Exception._

/** Reads lines from an input stream */
trait InteractiveReader {
  import InteractiveReader._
  import java.io.IOException

  protected def readOneLine(prompt: String): String
  val interactive: Boolean

  def readLine(prompt: String): String = {
    def handler: Catcher[String] = {
      case e: IOException if restartSystemCall(e) => readLine(prompt)
    }
    catching(handler) { readOneLine(prompt) }
  }

  // hack necessary for OSX jvm suspension because read calls are not restarted after SIGTSTP
  private def restartSystemCall(e: Exception): Boolean =
    Properties.isMac && (e.getMessage == msgEINTR)
}


object InteractiveReader {
  val msgEINTR = "Interrupted system call"
  private val exes = List(classOf[Exception], classOf[NoClassDefFoundError])

  def createDefault(): InteractiveReader = createDefault(null)

  /** Create an interactive reader.  Uses <code>JLineReader</code> if the
   *  library is available, but otherwise uses a <code>SimpleReader</code>.
   */
  def createDefault(interpreter: Interpreter, intLoop: InterpreterLoop = null): InteractiveReader =
    catching(exes: _*)
      . opt (new JLineReader(interpreter, intLoop))
      . getOrElse (new SimpleReader)
}

