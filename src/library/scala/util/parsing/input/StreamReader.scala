/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.parsing.input

import java.io.BufferedReader

/** An object to create a StreamReader from a java.io.Reader.
 *
 * @param in the java.io.Reader that provides the underlying stream of characters for this Reader
 *
 * @author Miles Sabin
 */
object StreamReader
{
  final val EofCh = '\032'
  final val CR = '\015'

  def apply(in: java.io.Reader) = {
   	val bin = new BufferedReader(in)
    new StreamReader(bin, bin.readLine, 1, 1)
  }
}

/** A character array reader reads a stream of characters (keeping track of their positions)
 * from an array.
 *
 * @param bin the underlying java.io.BufferedReader
 * @param sourceLine  the line at column `col' in the stream
 * @param line   the 1-based line number of the character returned by `first'
 * @param column the 1-based column number of the character returned by `first'
 *
 * @author Miles Sabin
 */
sealed class StreamReader private (bin: java.io.BufferedReader, sourceLine: String, ln: int, col: int) extends Reader[char]
{
  import StreamReader._

  def first =
    if(sourceLine == null)
      EofCh
    else if(col > sourceLine.length)
      CR
    else
      sourceLine(col-1)

  def rest: StreamReader =
    if(sourceLine == null)
      this
    else if(col > sourceLine.length)
      new StreamReader(bin, bin.readLine, ln+1, 1)
    else
      new StreamReader(bin, sourceLine, ln, col+1)

  def pos: Position = new Position {
    	def line = ln
      def column = col
      def lineContents(lnum: int) = sourceLine
    }

  def atEnd = (sourceLine == null)
}
