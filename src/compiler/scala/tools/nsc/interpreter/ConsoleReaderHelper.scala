/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import scala.tools.jline.console.{ ConsoleReader, CursorBuffer }
import scala.tools.jline.console.completer.CompletionHandler
import Completion._

trait ConsoleReaderHelper extends ConsoleReader {
  def currentLine = "" + getCursorBuffer.buffer
  def currentPos  = getCursorBuffer.cursor
  def terminal    = getTerminal()
  def width       = terminal.getWidth()
  def height      = terminal.getHeight()
  def paginate    = isPaginationEnabled()
  def paginate_=(value: Boolean) = setPaginationEnabled(value)

  def goBack(num: Int): Unit
  def readOneKey(prompt: String): Int
  def eraseLine(): Unit

  private val marginSize = 3
  private def morePrompt = "--More--"
  private def emulateMore(): Int = {
    val key = readOneKey(morePrompt)
    try key match {
      case '\r' | '\n'  => 1
      case 'q'          => -1
      case _            => height - 1
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

  override def printColumns(items: JCollection[_ <: CharSequence]): Unit =
    printColumns(items: List[String])

  def printColumns(items: List[String]): Unit = {
    if (items forall (_ == ""))
      return

    val longest    = items map (_.length) max
    var linesLeft  = if (isPaginationEnabled()) height - 1 else Int.MaxValue
    val columnSize = longest + marginSize
    val padded     = items map ("%-" + columnSize + "s" format _)
    val groupSize  = 1 max (width / columnSize)   // make sure it doesn't divide to 0

    padded grouped groupSize foreach { xs =>
      println(xs.mkString)
      linesLeft -= 1
      if (linesLeft <= 0) {
        linesLeft = emulateMore()
        if (linesLeft < 0)
          return
      }
    }
  }
}
