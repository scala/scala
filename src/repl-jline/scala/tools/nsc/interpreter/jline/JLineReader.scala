/** NSC -- new Scala compiler
  *
  * Copyright 2005-2015 LAMP/EPFL
  * @author Stepan Koltsov
  * @author Adriaan Moors
  */

package scala.tools.nsc.interpreter.jline

import java.util.{Collection => JCollection, List => JList}

import _root_.jline.{console => jconsole}
import jline.console.ConsoleReader
import jline.console.completer.{CandidateListCompletionHandler, CompletionHandler, Completer, ArgumentCompleter}
import jconsole.history.{History => JHistory}

import scala.tools.nsc.interpreter
import scala.tools.nsc.interpreter.{Completion, JLineCompletion, NoCompletion}
import scala.tools.nsc.interpreter.Completion.Candidates
import scala.tools.nsc.interpreter.session.History

/**
 * Reads from the console using JLine.
 *
 * Eagerly instantiates all relevant JLine classes, so that we can detect linkage errors on `new JLineReader` and retry.
 */
class InteractiveReader(completer: () => Completion) extends interpreter.InteractiveReader {
  val interactive = true

  val history: History = new JLineHistory.JLineFileHistory()

  private val consoleReader = {
    val reader = new JLineConsoleReader()

    reader setPaginationEnabled interpreter.isPaged

    // turn off magic !
    reader setExpandEvents false

    // enable detecting pasted tab char (when next char is immediately available) which is taken raw, not completion
    reader setCopyPasteDetection true

    reader setHistory history.asInstanceOf[JHistory]

    reader
  }

  private[this] var _completion: Completion = interpreter.NoCompletion
  def completion: Completion = _completion

  override def postInit() = {
    _completion = completer()

    consoleReader.initCompletion(completion)
  }

  def reset()                     = consoleReader.getTerminal().reset()
  def redrawLine()                = consoleReader.redrawLineAndFlush()
  def readOneLine(prompt: String) = consoleReader.readLine(prompt)
  def readOneKey(prompt: String)  = consoleReader.readOneKey(prompt)
}

// implements a jline interface
private class JLineConsoleReader extends jconsole.ConsoleReader with interpreter.VariColumnTabulator {
  val isAcross   = interpreter.`package`.isAcross
  val marginSize = 3

  def width  = getTerminal.getWidth()
  def height = getTerminal.getHeight()

  private def morePrompt = "--More--"

  private def emulateMore(): Int = {
    val key = readOneKey(morePrompt)
    try key match {
      case '\r' | '\n' => 1
      case 'q' => -1
      case _ => height - 1
    }
    finally {
      eraseLine()
      // TODO: still not quite managing to erase --More-- and get
      // back to a scala prompt without another keypress.
      if (key == 'q') {
        putString(getPrompt())
        redrawLine()
        flush()
      }
    }
  }

  override def printColumns(items: JCollection[_ <: CharSequence]): Unit = {
    import scala.tools.nsc.interpreter.javaCharSeqCollectionToScala
    printColumns_(items: List[String])
  }

  private def printColumns_(items: List[String]): Unit = if (items exists (_ != "")) {
    val grouped = tabulate(items)
    var linesLeft = if (isPaginationEnabled()) height - 1 else Int.MaxValue
    grouped foreach { xs =>
      println(xs.mkString)
      linesLeft -= 1
      if (linesLeft <= 0) {
        linesLeft = emulateMore()
        if (linesLeft < 0)
          return
      }
    }
  }

  def readOneKey(prompt: String) = {
    this.print(prompt)
    this.flush()
    this.readCharacter()
  }

  def eraseLine() = resetPromptLine("", "", 0)

  def redrawLineAndFlush(): Unit = {
    flush(); drawLine(); flush()
  }

  // A hook for running code after the repl is done initializing.
  def initCompletion(completion: Completion): Unit = {
    this setBellEnabled false

    // adapt the JLine completion interface
    def completer =
      new Completer {
        val tc = completion.completer()
        def complete(_buf: String, cursor: Int, candidates: JList[CharSequence]): Int = {
          val buf = if (_buf == null) "" else _buf
          val Candidates(newCursor, newCandidates) = tc.complete(buf, cursor)
          newCandidates foreach (candidates add _)
          newCursor
        }
      }
    getCompletionHandler match {
      case clch: CandidateListCompletionHandler => clch.setPrintSpaceAfterFullCompletion(false)
    }

    // a last bit of nastiness: parsing help depending on the flavor of completer (fixme)
    completion match {
      case _: JLineCompletion =>
        val jlineCompleter = new ArgumentCompleter(new JLineDelimiter, completer)
        jlineCompleter setStrict false
        this addCompleter jlineCompleter
      case NoCompletion       => ()
      case _                  => this addCompleter completer
    }

    setAutoprintThreshold(400) // max completion candidates without warning
  }
}
