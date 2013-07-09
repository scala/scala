/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala.tools.nsc
package interpreter

import jline.console.ConsoleReader
import jline.console.completer._
import session._
import Completion._

/**
 *  Reads from the console using JLine.
 */
class JLineReader(_completion: => Completion) extends InteractiveReader {
  val interactive = true
  val consoleReader = new JLineConsoleReader()

  lazy val completion = _completion
  lazy val history: JLineHistory = JLineHistory()

  private def term = consoleReader.getTerminal()
  def reset() = term.reset()

  def scalaToJline(tc: ScalaCompleter): Completer = new Completer {
    def complete(_buf: String, cursor: Int, candidates: JList[CharSequence]): Int = {
      val buf   = if (_buf == null) "" else _buf
      val Candidates(newCursor, newCandidates) = tc.complete(buf, cursor)
      newCandidates foreach (candidates add _)
      newCursor
    }
  }

  class JLineConsoleReader extends ConsoleReader with ConsoleReaderHelper {
    // working around protected/trait/java insufficiencies.
    def goBack(num: Int): Unit = back(num)
    if ((history: History) ne NoHistory)
      this setHistory history

    def readOneKey(prompt: String) = {
      this.print(prompt)
      this.flush()
      this.readCharacter()
    }
    def eraseLine() = consoleReader.resetPromptLine("", "", 0)
    def redrawLineAndFlush(): Unit = { flush() ; drawLine() ; flush() }

    // A hook for running code after the repl is done initializing.
    lazy val postInit: Unit = {
      this setBellEnabled false

      if (completion ne NoCompletion) {
        val argCompletor: ArgumentCompleter =
          new ArgumentCompleter(new JLineDelimiter, scalaToJline(completion.completer()))
        argCompletor setStrict false

        this addCompleter argCompletor
        this setAutoprintThreshold 400 // max completion candidates without warning
      }
    }
  }

  def redrawLine() = consoleReader.redrawLineAndFlush()
  def readOneLine(prompt: String) = consoleReader readLine prompt
  def readOneKey(prompt: String)  = consoleReader readOneKey prompt
}
