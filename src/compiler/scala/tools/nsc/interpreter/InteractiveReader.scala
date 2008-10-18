/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
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
    (vendor startsWith "Apple") && (e.getMessage == msgEINTR)
}



object InteractiveReader {
  // hacks necessary for OSX jvm suspension because read calls are not restarted after SIGTSTP
  val vendor = { val v = System.getProperty("java.vendor") ; if (v == null) "" else v }
  val msgEINTR = "Interrupted system call"

  /** Create an interactive reader.  Uses JLine if the
   *  library is available, but otherwise uses a
   *  SimpleReader. */
   def createDefault(): InteractiveReader = {
     try {
       new JLineReader
     } catch {
       case e =>
         //out.println("jline is not available: " + e) //debug
  new SimpleReader()
     }
   }
}
