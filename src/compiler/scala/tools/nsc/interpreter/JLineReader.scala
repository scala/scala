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

/** A wrapper for JLine's History.
 */
class JLineHistory(val jhistory: JHistory) extends History {
  def asJavaList = jhistory.entries()
  def asStrings = asList map (_.value.toString)
  def asList: List[JEntry] = asJavaList.asScala.toList
  def index = jhistory.index()
  def size = jhistory.size()

  def grep(s: String) = asStrings filter (_ contains s)
  def flush() = jhistory match {
    case x: PersistentHistory => x.flush()
    case _                    => ()
  }
}

object JLineHistory {
  val ScalaHistoryFile = ".scala_history"

  def apply() = new JLineHistory(
    try newFile()
    catch { case x : Exception =>
      Console.println("Error creating file history: memory history only. " + x)
      newMemory()
    }
  )

  def newMemory() = new MemoryHistory()
  def newFile() = new FileHistory(new File(userHome, ScalaHistoryFile)) {
    // flush after every add to avoid installing a shutdown hook.
    // (The shutdown hook approach also loses history when they aren't run.)
    override def add(item: CharSequence): Unit = {
      super.add(item)
      flush()
    }
  }
}

/** Reads from the console using JLine */
class JLineReader(val completion: Completion) extends InteractiveReader {
  lazy val history = JLineHistory()

  def reset() = consoleReader.getTerminal().reset()
  def init()  = consoleReader.getTerminal().init()

  override def redrawLine()    = {
    consoleReader.flush()
    consoleReader.drawLine()
    consoleReader.flush()
  }

  def argCompletor: ArgumentCompleter = {
    val wrapped = new Completer {
      val cc = completion.completer()
      def complete(buffer: String, cursor: Int, candidates: JList[CharSequence]): Int =
        cc.complete(buffer, cursor, candidates)
    }
    val c = new ArgumentCompleter(new JLineDelimiter, wrapped)
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
      r setAutoprintThreshold 400 // max completion candidates without warning
    }

    r
  }

  override def currentLine: String = consoleReader.getCursorBuffer.buffer.toString
  def readOneLine(prompt: String) = consoleReader readLine prompt
  val interactive = true
}

object JLineReader {
  def apply(intp: IMain): InteractiveReader = apply(new JLineCompletion(intp))
  def apply(comp: Completion): InteractiveReader = {
    try new JLineReader(comp)
    catch { case e @ (_: Exception | _: NoClassDefFoundError) => new SimpleReader }
  }
}
