/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala.tools.nsc
package interpreter

import java.io.File
import scala.tools.jline.console.ConsoleReader
import scala.tools.jline.console.completer._

/** Reads from the console using JLine */
class JLineReader(val completion: Completion) extends InteractiveReader {
  lazy val history = History()

  def reset() = consoleReader.getTerminal().reset()
  def init()  = consoleReader.getTerminal().init()

  override def redrawLine()    = {
    consoleReader.flush()
    consoleReader.drawLine()
    consoleReader.flush()
  }

  def argCompletor: ArgumentCompleter = {
    val c = new ArgumentCompleter(new JLineDelimiter, completion.completer())
    c setStrict false
    c
  }

  val consoleReader = {
    val r = new ConsoleReader()
    r setBellEnabled false
    if (history ne History.Empty)
      r setHistory history.jhistory

    if (completion ne Completion.Empty) {
      r addCompleter argCompletor
      r setAutoprintThreshold 250 // max completion candidates without warning
    }

    r
  }

  override def currentLine: String = consoleReader.getCursorBuffer.buffer.toString
  def readOneLine(prompt: String) = consoleReader readLine prompt
  val interactive = true
}

