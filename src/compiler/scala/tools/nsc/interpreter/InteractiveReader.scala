/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Stepan Koltsov
 */
// $Id$

package scala.tools.nsc.interpreter

/** Reads lines from an input stream */
trait InteractiveReader {
  import InteractiveReader._
  import java.io.IOException

  protected def readOneLine(prompt: String): String
  val interactive: Boolean

  def readLine(prompt: String): String =
    try {
      readOneLine(prompt)
    }
    catch {
      case e: IOException if restartSystemCall(e) => readLine(prompt)
      case e => throw e
    }

  private def restartSystemCall(e: Exception): Boolean =
    Properties.isMac && (e.getMessage == msgEINTR)
}


object InteractiveReader {
  // hacks necessary for OSX jvm suspension because read calls are not restarted after SIGTSTP
  val vendor = System.getProperty("java.vendor", "")
  val msgEINTR = "Interrupted system call"

  def createDefault(): InteractiveReader = createDefault(null)

  /** Create an interactive reader.  Uses <code>JLineReader</code> if the
   *  library is available, but otherwise uses a <code>SimpleReader</code>.
   */
  def createDefault(interpreter: Interpreter): InteractiveReader =
    try {
      new JLineReader(interpreter)
    } catch {
      case _: Exception | _: NoClassDefFoundError => new SimpleReader
    }
}

