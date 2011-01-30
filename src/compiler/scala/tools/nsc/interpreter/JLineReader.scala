/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala.tools.nsc
package interpreter

import java.io.File
import java.util.{ List => JList }
import scala.tools.jline.console.ConsoleReader
import scala.tools.jline.console.completer._
import scala.tools.jline.console.history._
import scala.tools.jline.console.history.{ FileHistory, PersistentHistory, History => JHistory }
import scala.tools.jline.console.history.History.{ Entry => JEntry }
import scala.tools.jline.console.ConsoleReader
import scala.collection.JavaConverters._
import Properties.userHome
import Completion._
import io.Streamable.slurp

/** Reads from the console using JLine */
class JLineReader(val completion: Completion) extends InteractiveReader {
  val interactive = true
  lazy val history = JLineHistory()
  lazy val keyBindings =
    try KeyBinding parse slurp(term.getDefaultBindings)
    catch { case _: Exception => Nil }

  private def term = consoleReader.getTerminal()
  def reset() = term.reset()
  def init()  = term.init()

  def scalaToJline(tc: ScalaCompleter): Completer = new Completer {
    def complete(_buf: String, cursor: Int, candidates: JList[CharSequence]): Int = {
      val buf   = if (_buf == null) "" else _buf
      val Candidates(newCursor, newCandidates) = tc.complete(buf, cursor)
      newCandidates foreach (candidates add _)
      newCursor
    }
  }

  def argCompletor: ArgumentCompleter = {
    val c = new ArgumentCompleter(new JLineDelimiter, scalaToJline(completion.completer()))
    c setStrict false
    c
  }

  val consoleReader = {
    val r = new ConsoleReader()
    r setBellEnabled false
    if (history ne NoHistory)
      r setHistory history.jhistory

    if (completion ne NoCompletion) {
      r addCompleter argCompletor
      r setAutoprintThreshold 400 // max completion candidates without warning
    }

    r
  }

  def currentLine: String = consoleReader.getCursorBuffer.buffer.toString
  def redrawLine() = {
    consoleReader.flush()
    consoleReader.drawLine()
    consoleReader.flush()
  }
  def readOneLine(prompt: String) = consoleReader readLine prompt
}

object JLineReader {
  def apply(intp: IMain): JLineReader = apply(new JLineCompletion(intp))
  def apply(comp: Completion): JLineReader = new JLineReader(comp)
}
