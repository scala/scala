/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author Stepan Koltsov
 */
// $Id$

package scala.tools.nsc.interpreter

/** Reads lines from an input stream */
trait InteractiveReader {
  def readLine(prompt: String): String
  val interactive: Boolean
}



object InteractiveReader {
  /** Create an interactive reader.  Uses JLine if the
   *  library is available, but otherwise uses a
   *  SimpleReader. */
  def createDefault(): InteractiveReader = {
    try {
      new JlineReader
    } catch {
      case e =>
        //out.println("jline is not available: " + e) //debug
	new SimpleReader()
    }
  }


}
