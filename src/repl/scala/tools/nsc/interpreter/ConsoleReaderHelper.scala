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

  protected def fits(items: Seq[String], width: Int): Boolean = (
    (items map (_.length)).sum + (items.length - 1) * marginSize < width
  )
  def tabulate(items: Seq[String]): Seq[Seq[String]] = (
    if (fits(items, width)) Seq(Seq(items mkString " " * marginSize))
    else printMultiLineColumns(items)
  )
  protected def columnize(ss: Seq[String]): Seq[Seq[String]] = ss map (s => Seq(s))
  protected def printMultiLineColumns(items: Seq[String]): Seq[Seq[String]] = {
    import SimpleMath._
    val longest     = (items map (_.length)).max
    val columnWidth = longest + marginSize
    val maxcols = (
      if (columnWidth >= width) 1
      else 1 max (width / columnWidth)   // make sure it doesn't divide to 0
    )
    val nrows       = items.size /% maxcols
    val ncols       = items.size /% nrows
    val groupSize   = ncols
    val padded      = items map (s"%-${columnWidth}s" format _)
    val xwise       = isAcross || ncols >= items.length
    val grouped: Seq[Seq[String]]    =
      if (groupSize == 1) columnize(items)
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

/** Adjust the column width and number of columns to minimize the row count. */
trait VariColumnTabulator extends Tabulator {
  override protected def printMultiLineColumns(items: Seq[String]): Seq[Seq[String]] = {
    import SimpleMath._
    val longest  = (items map (_.length)).max
    val shortest = (items map (_.length)).min
    val fattest  = longest + marginSize
    val skinny   = shortest + marginSize

    // given ncols, calculate nrows and a list of column widths, or none if not possible
    // if ncols > items.size, then columnWidths.size == items.size
    def layout(ncols: Int): Option[(Int, Seq[Int], Seq[Seq[String]])] = {
      val nrows = items.size /% ncols
      val xwise = isAcross || ncols >= items.length
      def maxima(sss: Seq[Seq[String]]) =
        (0 until (ncols min items.size)) map (i => (sss map (ss => ss(i).length)).max)
      def resulting(rows: Seq[Seq[String]]) = {
        val columnWidths = maxima(rows) map (_ + marginSize)
        val linelen      = columnWidths.sum
        if (linelen <= width) Some((nrows, columnWidths, rows))
        else None
      }
      if (ncols == 1) resulting(columnize(items))
      else if (xwise) resulting((items grouped ncols).toSeq)
      else {
        val cols = (items grouped nrows).toList
        val rows = for (i <- 0 until nrows) yield
          for (j <- 0 until ncols) yield
            if (j < cols.size && i < cols(j).size) cols(j)(i) else ""
        resulting(rows)
      }
    }

    if (fattest >= width) {
      columnize(items)
    } else {
      // if every col is widest, we have at least this many cols
      val mincols = 1 max (width / fattest)
      // if every other col is skinniest, we have at most this many cols
      val maxcols = 1 + ((width - fattest) / skinny)
      val possibles = (mincols to maxcols).map(n => layout(n)).flatten
      val minrows = (possibles map (_._1)).min

      // select the min ncols that results in minrows
      val (_, columnWidths, sss) = (possibles find (_._1 == minrows)).get

      // format to column width
      sss map (ss => ss.zipWithIndex map {
        case (s, i) => s"%-${columnWidths(i)}s" format s
      })
    }
  }
}

private[interpreter] object SimpleMath {
  implicit class DivRem(private val i: Int) extends AnyVal {
    /** i/n + if (i % n != 0) 1 else 0 */
    def /%(n: Int): Int = (i + n - 1) / n
  }
}
