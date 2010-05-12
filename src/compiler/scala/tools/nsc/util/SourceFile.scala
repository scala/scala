/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package util

import scala.tools.nsc.io.{AbstractFile, VirtualFile}
import scala.collection.mutable.ArrayBuffer
import annotation.{ tailrec, switch }
import Chars._

/** abstract base class of a source file used in the compiler */
abstract class SourceFile {
  def content : Array[Char]         // normalized, must end in SU
  def file    : AbstractFile
  def isLineBreak(idx : Int) : Boolean
  def length : Int
  def position(offset: Int) : Position = {
    assert(offset < length)
    new OffsetPosition(this, offset)
  }
  def position(line: Int, column: Int) : Position = new OffsetPosition(this, lineToOffset(line) + column)
  def offsetToLine(offset: Int): Int
  def lineToOffset(index : Int): Int
  /** Map a position to a position in the underlying source file.
   *  For regular source files, simply return the argument.
   */
  def positionInUltimateSource(position: Position) = position
  override def toString(): String = file.name /* + ":" + content.length */
  def dbg(offset: Int) = (new OffsetPosition(this, offset)).dbgString
  def path = file.path

  def beginsWith(offset: Int, text: String): Boolean =
    (content drop offset) startsWith text

  def lineToString(index: Int): String =
    content drop lineToOffset(index) takeWhile (c => !isLineBreakChar(c.toChar)) mkString

  @tailrec
  final def skipWhitespace(offset: Int): Int =
    if (content(offset).isWhitespace) skipWhitespace(offset + 1) else offset

  def identifier(pos: Position, compiler: Global): Option[String] = None
}

/** a file whose contents do not change over time */
class BatchSourceFile(val file : AbstractFile, val content: Array[Char]) extends SourceFile {

  def this(_file: AbstractFile)                 = this(_file, _file.toCharArray)
  def this(sourceName: String, cs: Seq[Char])   = this(new VirtualFile(sourceName), cs.toArray)
  def this(file: AbstractFile, cs: Seq[Char])   = this(file, cs.toArray)

  override def equals(that : Any) = that match {
    case that : BatchSourceFile => file.path == that.file.path
    case _ => false
  }
  override def hashCode = file.path.hashCode
  val length = content.length

  // in SourceFileFragments, these are overridden to compensate during offset calculation
  // Invariant: length + start = underlyingLength
  def underlyingLength = length
  def start = 0

  override def identifier(pos: Position, compiler: Global) =
    if (pos.isDefined && pos.source == this && pos.point != -1) {
      def isOK(c: Char) = isIdentifierPart(c) || isOperatorPart(c)
      Some(new String(content drop pos.point takeWhile isOK))
    } else {
      super.identifier(pos, compiler)
    }

  def isLineBreak(idx: Int) =
    if (idx >= length) false else {
      val ch = content(idx)
      // don't identify the CR in CR LF as a line break, since LF will do.
      if (ch == CR) (idx + 1 == length) || (content(idx + 1) != LF)
      else isLineBreakChar(ch)
    }

  private lazy val lineIndices: Array[Int] = {
    val buf = new ArrayBuffer[Int]
    buf += 0
    for (i <- 0 until content.length) if (isLineBreak(i)) buf += i + 1
    buf += content.length // sentinel, so that findLine below works smoother
    buf.toArray
  }

  def lineToOffset(index : Int): Int = lineIndices(index)

  private var lastLine = 0

  /** Convert offset to line in this source file
   *  Lines are numbered from 0
   */
  def offsetToLine(offset: Int): Int = {
    val lines = lineIndices
    def findLine(lo: Int, hi: Int, mid: Int): Int =
      if (offset < lines(mid)) findLine(lo, mid - 1, (lo + mid - 1) / 2)
      else if (offset >= lines(mid + 1)) findLine(mid + 1, hi, (mid + 1 + hi) / 2)
      else mid
    lastLine = findLine(0, lines.length, lastLine)
    lastLine
  }

/**

  // An array which maps line numbers (counting from 0) to char offset into content
  private lazy val lineIndices: Array[Int] = {

    val xs = content.indices filter isLineBreak map (_ + 1) toArray
    val arr = new Array[Int](xs.length + 1)
    arr(0) = 0
    System.arraycopy(xs, 0, arr, 1, xs.length)

    arr
  }
  // A reverse map which also hunts down the right answer on non-exact lookups
  private class SparseReverser() {
    val revMap = Map(lineIndices.zipWithIndex: _*)

    def apply(x: Int): Int = revMap.get(x) match {
      case Some(res)  => res
      case _          =>
        var candidate = x - 1
        while (!revMap.contains(candidate))
          candidate -= 1

        revMap(candidate)
    }
  }
  private lazy val lineIndicesRev = new SparseReverser()

  def lineToOffset(index : Int): Int = lineIndices(index)
  def offsetToLine(offset: Int): Int = lineIndicesRev(offset)

  */
}

/** A source file composed of multiple other source files.
 *
 *  @version 1.0
 */
class CompoundSourceFile(
    name: String,
    components: List[BatchSourceFile],
    contents: Array[Char])
extends BatchSourceFile(name, contents)
{
  /** The usual constructor.  Specify a name for the compound file and
   *  a list of component sources.
   */
  def this(name: String, components: BatchSourceFile*) =
    this(name, components.toList, components flatMap (CompoundSourceFile stripSU _.content) toArray)

  /** Create an instance with the specified components and a generic name. */
  def this(components: BatchSourceFile*) = this("(virtual file)", components: _*)

  override def positionInUltimateSource(position: Position) = {
    if (!position.isDefined) super.positionInUltimateSource(position)
    else {
      var off = position.point
      var compsLeft = components
      // the search here has to be against the length of the files underlying the
      // components, not their advertised length (which in the case of a fragment is
      // less than the underlying length.) Otherwise we can and will overshoot the
      // correct component and return a garbage position.
      while (compsLeft.head.underlyingLength-1 <= off && !compsLeft.tail.isEmpty) {
        off = off - compsLeft.head.underlyingLength + 1
        compsLeft = compsLeft.tail
      }
      // now that we've identified the correct component, we have to adjust the
      // position we report since it is expected relative to the fragment, not the
      // underlying file.  Thus, off - comp.start.
      val comp = compsLeft.head
      comp.positionInUltimateSource(new OffsetPosition(this, off - comp.start))
    }
  }
}

object CompoundSourceFile {
  private[util] def stripSU(chars: Array[Char]) =
    if (chars.length > 0 && chars.last == SU)
      chars dropRight 1
    else
      chars
}


/** One portion of an underlying file.  The fragment includes
  * the indices from the specified start (inclusively) to stop
  * (not inclusively).
  */
class SourceFileFragment private (
    name: String,
    underlyingFile: BatchSourceFile,
    override val start: Int,
    stop: Int,
    contents: Array[Char])
extends BatchSourceFile(name, contents) {
  override def underlyingLength = underlyingFile.length
  def this(name: String, underlyingFile: BatchSourceFile, start: Int, stop: Int) =
    this(
      name,
      underlyingFile,
      start,
      stop,
      { assert(start >= 0)
        assert(start <= stop)
        assert(start <= underlyingFile.length)
        assert(stop <= underlyingFile.length)
        underlyingFile.content.slice(start, stop).toArray })

  def this(underlyingFile: BatchSourceFile, start: Int, stop: Int) =
    this(
      "(fragment of " + underlyingFile.file.name + ")",
      underlyingFile,
      start,
      stop)

  override def positionInUltimateSource(position: Position) =
    super.positionInUltimateSource(
      if (position.isDefined) new OffsetPosition(this, position.point)
      else position
    )
}
