/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.reflect.internal.util

import scala.tools.nsc.io.{ AbstractFile, VirtualFile }
import scala.collection.mutable.ArrayBuffer
import annotation.tailrec
import java.util.regex.Pattern
import java.io.IOException
import scala.reflect.internal.Chars._

/** abstract base class of a source file used in the compiler */
abstract class SourceFile {
  def content : Array[Char]         // normalized, must end in SU
  def file    : AbstractFile
  def isLineBreak(idx : Int) : Boolean
  def isSelfContained: Boolean
  def length : Int
  def position(offset: Int) : Position = {
    assert(offset < length, file + ": " + offset + " >= " + length)
    new OffsetPosition(this, offset)
  }
  def position(line: Int, column: Int) : Position = new OffsetPosition(this, lineToOffset(line) + column)

  def offsetToLine(offset: Int): Int
  def lineToOffset(index : Int): Int

  /** Map a position to a position in the underlying source file.
   *  For regular source files, simply return the argument.
   */
  def positionInUltimateSource(position: Position) = position
  override def toString() = file.name
  def dbg(offset: Int) = (new OffsetPosition(this, offset)).dbgString
  def path = file.path

  def beginsWith(offset: Int, text: String): Boolean =
    (content drop offset) startsWith text

  def lineToString(index: Int): String =
    content drop lineToOffset(index) takeWhile (c => !isLineBreakChar(c.toChar)) mkString

  @tailrec
  final def skipWhitespace(offset: Int): Int =
    if (content(offset).isWhitespace) skipWhitespace(offset + 1) else offset

  def identifier(pos: Position): Option[String] = None
}

/** An object representing a missing source file.
 */
object NoSourceFile extends SourceFile {
  def content                   = Array()
  def file                      = NoFile
  def isLineBreak(idx: Int)     = false
  def isSelfContained           = true
  def length                    = -1
  def offsetToLine(offset: Int) = -1
  def lineToOffset(index : Int) = -1
  override def toString = "<no source file>"
}

object NoFile extends VirtualFile("<no file>", "<no file>")

object ScriptSourceFile {
  /** Length of the script header from the given content, if there is one.
   *  The header begins with "#!" or "::#!" and ends with a line starting
   *  with "!#" or "::!#".
   */
  def headerLength(cs: Array[Char]): Int = {
    val headerPattern = Pattern.compile("""((?m)^(::)?!#.*|^.*/env .*)(\r|\n|\r\n)""")
    val headerStarts  = List("#!", "::#!")

    if (headerStarts exists (cs startsWith _)) {
      val matcher = headerPattern matcher cs.mkString
      if (matcher.find) matcher.end
      else throw new IOException("script file does not close its header with !# or ::!#")
    }
    else 0
  }
  def stripHeader(cs: Array[Char]): Array[Char] = cs drop headerLength(cs)

  def apply(file: AbstractFile, content: Array[Char]) = {
    val underlying = new BatchSourceFile(file, content)
    val headerLen = headerLength(content)
    val stripped = new ScriptSourceFile(underlying, content drop headerLen, headerLen)

    stripped
  }
}
import ScriptSourceFile._

class ScriptSourceFile(underlying: BatchSourceFile, content: Array[Char], override val start: Int) extends BatchSourceFile(underlying.file, content) {
  override def isSelfContained = false

  override def positionInUltimateSource(pos: Position) =
    if (!pos.isDefined) super.positionInUltimateSource(pos)
    else new OffsetPosition(underlying, pos.point + start)
}

/** a file whose contents do not change over time */
class BatchSourceFile(val file : AbstractFile, val content: Array[Char]) extends SourceFile {

  def this(_file: AbstractFile)                 = this(_file, _file.toCharArray)
  def this(sourceName: String, cs: Seq[Char])   = this(new VirtualFile(sourceName), cs.toArray)
  def this(file: AbstractFile, cs: Seq[Char])   = this(file, cs.toArray)

  override def equals(that : Any) = that match {
    case that : BatchSourceFile => file.path == that.file.path && start == that.start
    case _ => false
  }
  override def hashCode = file.path.## + start.##
  val length = content.length
  def start = 0
  def isSelfContained = true

  override def identifier(pos: Position) =
    if (pos.isDefined && pos.source == this && pos.point != -1) {
      def isOK(c: Char) = isIdentifierPart(c) || isOperatorPart(c)
      Some(new String(content drop pos.point takeWhile isOK))
    } else {
      super.identifier(pos)
    }

  def isLineBreak(idx: Int) =
    if (idx >= length) false else {
      val ch = content(idx)
      // don't identify the CR in CR LF as a line break, since LF will do.
      if (ch == CR) (idx + 1 == length) || (content(idx + 1) != LF)
      else isLineBreakChar(ch)
    }

  def calculateLineIndices(cs: Array[Char]) = {
    val buf = new ArrayBuffer[Int]
    buf += 0
    for (i <- 0 until cs.length) if (isLineBreak(i)) buf += i + 1
    buf += cs.length // sentinel, so that findLine below works smoother
    buf.toArray
  }
  private lazy val lineIndices: Array[Int] = calculateLineIndices(content)

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
}
