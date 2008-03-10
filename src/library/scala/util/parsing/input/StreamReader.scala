/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: $

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
  final val CR = '\015'

  def apply(in: java.io.Reader): StreamReader = {
    val bin = new BufferedReader(in)
    new StreamReader(bin, bin.readLine, 1, 1)
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
sealed class StreamReader private (bin: BufferedReader, sourceLine: String, ln: Int, col: Int)
extends Reader[Char] {
  import StreamReader._

  def source: CharSequence = sourceLine
  def offset: Int = col-1

  def first =
    if (sourceLine == null)
      EofCh
    else if (col > sourceLine.length)
      CR
    else
      sourceLine(col-1)

  def rest: StreamReader =
    if (sourceLine == null)
      this
    else if (col > sourceLine.length)
      new StreamReader(bin, bin.readLine, ln+1, 1)
    else
      new StreamReader(bin, sourceLine, ln, col+1)

  def pos: Position = new Position {
    	def line = ln
      def column = col
      def lineContents(lnum: Int) = sourceLine
    }

  def atEnd = (sourceLine == null)
}
