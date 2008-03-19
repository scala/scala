/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.util.parsing.input

import java.io.BufferedReader

/** An object to create a StreamReader from a <code>java.io.Reader</code>.
 *
 * @param in the <code>java.io.Reader</code> that provides the underlying
 *           stream of characters for this Reader.
 *
 * @author Miles Sabin
 */
object StreamReader {
  final val EofCh = '\032'

  def apply(in: java.io.Reader): StreamReader = {
    new StreamReader(new LazyCharSequence(in), 0, 1)
  }
}

/** A character array reader reads a stream of characters (keeping track of
 *  their positions) from an array.
 *
 *  NOTE:
 *  StreamReaders do not really fulfill the new contract for readers, which
 *  requires a `source' CharSequence representing the full input.
 *  Instead source is treated line by line.
 *  As a consequence, regex matching cannot extend beyond a single lines
 *  when a StreamReader are used for input.
 *
 * @param bin the underlying java.io.BufferedReader
 * @param sourceLine  the line at column `col' in the stream
 * @param line   the 1-based line number of the character returned by `first'
 * @param column the 1-based column number of the character returned by `first'
 *
 * @author Miles Sabin
 */
sealed class StreamReader(source: CharSequence, offset: Int, lnum: Int) extends CharSequenceReader(source, offset) {
  import StreamReader._

  override def rest: CharSequenceReader =
    if (offset == source.length) this
    else if (source(offset) == '\n') new StreamReader(source.subSequence(offset + 1), 0, lnum + 1)
    else new StreamReader(source, offset + 1, lnum)

  private def nextEol = {
    var i = offset
    while (i < source.length && source(i) != '\n' && source(i) != EofCh) i += 1
    i
  }

  override def drop(n: Int): StreamReader = {
    val eolPos = nextEol
    if (eolPos < offset + n && eolPos < source.length)
      new StreamReader(source.subSequence(eolPos + 1), 0, lnum + 1).drop(offset + n - (eolPos + 1))
    else
      new StreamReader(source, offset + n, lnum)
  }

  override def pos: Position = new Position {
    def line = lnum
    def column = offset + 1
    def lineContents = source.subSequence(0, nextEol).toString
  }
}
