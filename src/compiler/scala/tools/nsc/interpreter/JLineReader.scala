/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala.tools.nsc
package interpreter

import java.io.File
import jline.{ ConsoleReader, ArgumentCompletor, History => JHistory }

/** Reads from the console using JLine */
class JLineReader(interpreter: Interpreter) extends InteractiveReader {
  def this() = this(null)

  override lazy val history    = Some(History(consoleReader))
  override lazy val completion = Option(interpreter) map (x => new Completion(x))
  override def init()          = consoleReader.getTerminal().initializeTerminal()
  override def redrawLine()    = {
    consoleReader.flushConsole()
    consoleReader.drawLine()
    consoleReader.flushConsole()
  }

  val consoleReader = {
    val r = new jline.ConsoleReader()
    r setHistory (History().jhistory)
    r setBellEnabled false
    completion foreach { c =>
      r addCompletor c.jline
      r setAutoprintThreshhold 250
    }

    r
  }

  override def currentLine: String = consoleReader.getCursorBuffer.getBuffer.toString
  def readOneLine(prompt: String) = consoleReader readLine prompt
  val interactive = true
}

