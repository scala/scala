/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.util.parsing.input

import scala.collection.immutable.PagedSeq

/** An object encapsulating basic character constants
 *
 * @author Martin Odersky, Adriaan Moors
 */
object PagedSeqReader {
  final val EofCh = '\032'
}

/** A character array reader reads a stream of characters (keeping track of their positions)
 * from an array.
 *
 * @param source the source sequence
 * @param offset  starting offset.
 *
 * @author Martin Odersky
 */
class PagedSeqReader(seq: PagedSeq[Char],
                     override val offset: Int) extends Reader[Char] {
  import PagedSeqReader._

  override lazy val source: java.lang.CharSequence = seq

  /** Construct a <code>PagedSeqReader</code> with its first element at
   *  <code>source(0)</code> and position <code>(1,1)</code>.
   */
  def this(seq: PagedSeq[Char]) = this(seq, 0)

  /** Returns the first element of the reader, or EofCh if reader is at its end
   */
  def first =
    if (seq.isDefinedAt(offset)) seq(offset) else EofCh

  /** Returns a PagedSeqReader consisting of all elements except the first
   *
   * @return If <code>atEnd</code> is <code>true</code>, the result will be
   *         <code>this'; otherwise, it's a <code>PagedSeqReader</code> containing
   *         the rest of input.
   */
  def rest: PagedSeqReader =
    if (seq.isDefinedAt(offset)) new PagedSeqReader(seq, offset + 1)
    else this

  /** The position of the first element in the reader
   */
  def pos: Position = new OffsetPosition(source, offset)

  /** true iff there are no more elements in this reader (except for trailing
   *  EofCh's)
   */
  def atEnd = !seq.isDefinedAt(offset)

  /** Returns an abstract reader consisting of all elements except the first
   *  <code>n</code> elements.
   */
  override def drop(n: Int): PagedSeqReader =
    new PagedSeqReader(seq, offset + n)
}
