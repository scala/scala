/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.parsing.input

import java.io.BufferedReader
import scala.collection.immutable.PagedSeq

/** An object to create a `StreamReader` from a `java.io.Reader`.
 *
 * @author Miles Sabin
 */
object StreamReader {
  final val EofCh = '\032'

  /** Create a `StreamReader` from a `java.io.Reader`.
   *
   * @param in the `java.io.Reader` that provides the underlying
   *           stream of characters for this Reader.
   */  
  def apply(in: java.io.Reader): StreamReader = {
    new StreamReader(PagedSeq.fromReader(in), 0, 1)
  }
}

/** A StreamReader reads from a character sequence, typically created as a PagedSeq
 *  from a java.io.Reader
 *
 *  NOTE:
 *  StreamReaders do not really fulfill the new contract for readers, which
 *  requires a `source` CharSequence representing the full input.
 *  Instead source is treated line by line.
 *  As a consequence, regex matching cannot extend beyond a single line
 *  when a StreamReader are used for input.
 *
 *  If you need to match regexes spanning several lines you should consider
 *  class `PagedSeqReader` instead.
 *
 *  @author Miles Sabin
 *  @author Martin Odersky
 */
sealed class StreamReader(seq: PagedSeq[Char], off: Int, lnum: Int) extends PagedSeqReader(seq, off) {
  import StreamReader._

  override def rest: StreamReader =
    if (off == seq.length) this
    else if (seq(off) == '\n')
      new StreamReader(seq.slice(off + 1), 0, lnum + 1)
    else new StreamReader(seq, off + 1, lnum)

  private def nextEol = {
    var i = off
    while (i < seq.length && seq(i) != '\n' && seq(i) != EofCh) i += 1
    i
  }

  override def drop(n: Int): StreamReader = {
    val eolPos = nextEol
    if (eolPos < off + n && eolPos < seq.length)
      new StreamReader(seq.slice(eolPos + 1), 0, lnum + 1).drop(off + n - (eolPos + 1))
    else
      new StreamReader(seq, off + n, lnum)
  }

  override def pos: Position = new Position {
    def line = lnum
    def column = off + 1
    def lineContents = seq.slice(0, nextEol).toString
  }
}
