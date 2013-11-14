/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import jline.console.{ ConsoleReader, CursorBuffer }

trait ConsoleReaderHelper { _: ConsoleReader with Tabulator =>
  def isAcross: Boolean

  def terminal    = getTerminal()
  def width       = terminal.getWidth()
  def height      = terminal.getHeight()

  def readOneKey(prompt: String): Int
  def eraseLine(): Unit

  val marginSize = 3

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
    printColumns_(items: List[String])

  private def printColumns_(items: List[String]): Unit = if (items exists (_ != "")) {
    val grouped = tabulate(items)
    var linesLeft  = if (isPaginationEnabled()) height - 1 else Int.MaxValue
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
}

trait Tabulator {
  def isAcross: Boolean
  def width: Int
  def marginSize: Int

  private def fits(items: List[String], width: Int): Boolean = (
    (items map (_.length)).sum + (items.length - 1) * marginSize < width
  )
  def tabulate(items: List[String]): Seq[Seq[String]] = {
    if (fits(items, width)) Seq(Seq(items mkString " " * marginSize))
    else printMultiLineColumns(items)
  }
  private def printMultiLineColumns(items: List[String]): Seq[Seq[String]] = {
    import SimpleMath._
    val longest     = (items map (_.length)).max
    //val shortest   = (items map (_.length)).min
    val columnWidth = longest + marginSize
    val maxcols = {
      if (columnWidth >= width) 1
      else 1 max (width / columnWidth)   // make sure it doesn't divide to 0
    }
    val nrows       = items.size /% maxcols
    val ncols       = items.size /% nrows
    val groupSize   = ncols
    val padded      = items map (s"%-${columnWidth}s" format _)
    val xwise       = isAcross || ncols > items.length
    val grouped: Seq[Seq[String]]    =
      if (groupSize == 1) Seq(items)
      else if (xwise) (padded grouped groupSize).toSeq
      else {
        val h       = 1 max padded.size /% groupSize
        val cols    = (padded grouped h).toList
        for (i <- 0 until h) yield
          for (j <- 0 until groupSize) yield
            if (i < cols(j).size) cols(j)(i) else ""
      }
    grouped
  }
}

private[interpreter] object SimpleMath {
  implicit class DivRem(private val i: Int) extends AnyVal {
    /** i/n + if (i % n != 0) 1 else 0 */
    def /%(n: Int): Int = (i + n - 1) / n
  }
}
