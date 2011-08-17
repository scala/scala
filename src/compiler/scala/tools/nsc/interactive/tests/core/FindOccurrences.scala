package scala.tools.nsc
package interactive
package tests.core

import scala.tools.nsc.util.Position
import scala.tools.nsc.util.SourceFile

/** Find occurrences of `text` in the passed `sources`. */
private[core] object FindOccurrences {

  def apply(sources: Seq[SourceFile])(text: String): Map[SourceFile, Seq[Position]] =
    allPositionsOf(sources, text)

  /** All positions of the given string in all source files. */
  private def allPositionsOf(sources: Seq[SourceFile], str: String): Map[SourceFile, Seq[Position]] =
    (for (s <- sources; p <- positionsOf(s, str)) yield p).groupBy(_.source)

  /** Return all positions of the given str in the given source file. */
  private def positionsOf(source: SourceFile, str: String): Seq[Position] = {
    val buf = new collection.mutable.ListBuffer[Position]
    var pos = source.content.indexOfSlice(str)
    while (pos >= 0) {
      buf += source.position(pos - 1) // we need the position before the first character of this marker
      pos = source.content.indexOfSlice(str, pos + 1)
    }
    buf.toList
  }
}