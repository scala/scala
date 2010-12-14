/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala.tools.nsc
package interpreter

import java.io.File
import jline.console.ConsoleReader
import jline.console.completer._
import jline.console.history.{ History => JHistory }

/** Reads from the console using JLine */
class JLineReader(interpreter: Interpreter) extends InteractiveReader {
  def this() = this(null)

  override lazy val history    = {
    val h = History()
    system addShutdownHook {
      repldbg("Flushing history")
      h.flush()
    }
    Some(h)
  }
  override lazy val completion = Option(interpreter) map (x => new Completion(x))
  override def reset()         = consoleReader.getTerminal().reset()
  override def init()          = consoleReader.getTerminal().init()
  override def redrawLine()    = {
    consoleReader.flush()
    consoleReader.drawLine()
    consoleReader.flush()
  }

  val consoleReader = {
    val r = new ConsoleReader()
    r setBellEnabled false
    history foreach { r setHistory _.jhistory }
    completion foreach { c =>
      r addCompleter c.jline
      r setAutoprintThreshold 250 // max completion candidates without warning
    }

    r
  }

  override def currentLine: String = consoleReader.getCursorBuffer.buffer.toString
  def readOneLine(prompt: String) = consoleReader readLine prompt
  val interactive = true
}

