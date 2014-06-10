/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala
package reflect.internal.util

import scala.reflect.io.{ AbstractFile, VirtualFile }
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec
import java.util.regex.Pattern
import java.io.IOException
import scala.reflect.internal.Chars._

/** abstract base class of a source file used in the compiler */
abstract class SourceFile {
  def content: Array[Char]         // normalized, must end in SU
  def file   : AbstractFile
  def isLineBreak(idx: Int): Boolean
  def isEndOfLine(idx: Int): Boolean
  def isSelfContained: Boolean
  def length : Int
  def position(offset: Int): Position = {
    assert(offset < length, file + ": " + offset + " >= " + length)
    Position.offset(this, offset)
  }

  def offsetToLine(offset: Int): Int
  def lineToOffset(index : Int): Int

  /** Map a position to a position in the underlying source file.
   *  For regular source files, simply return the argument.
   */
  def positionInUltimateSource(position: Position) = position
  override def toString() = file.name
  def path = file.path

  def lineToString(index: Int): String = {
    val start = lineToOffset(index)
    var end = start
    while (end < length && !isEndOfLine(end)) end += 1
    new String(content, start, end - start)
  }

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
  def isEndOfLine(idx: Int)     = false
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

  def apply(file: AbstractFile, content: Array[Char]) = {
    val underlying = new BatchSourceFile(file, content)
    val headerLen = headerLength(content)
    val stripped = new ScriptSourceFile(underlying, content drop headerLen, headerLen)

    stripped
  }

  def apply(underlying: BatchSourceFile) = {
    val headerLen = headerLength(underlying.content)
    new ScriptSourceFile(underlying, underlying.content drop headerLen, headerLen)
  }
}

class ScriptSourceFile(underlying: BatchSourceFile, content: Array[Char], override val start: Int) extends BatchSourceFile(underlying.file, content) {
  override def isSelfContained = false

  override def positionInUltimateSource(pos: Position) =
    if (!pos.isDefined) super.positionInUltimateSource(pos)
    else pos withSource underlying withShift start
}

/** a file whose contents do not change over time */
class BatchSourceFile(val file : AbstractFile, content0: Array[Char]) extends SourceFile {
  def this(_file: AbstractFile)                 = this(_file, _file.toCharArray)
  def this(sourceName: String, cs: Seq[Char])   = this(new VirtualFile(sourceName), cs.toArray)
  def this(file: AbstractFile, cs: Seq[Char])   = this(file, cs.toArray)

  // If non-whitespace tokens run all the way up to EOF,
  // positions go wrong because the correct end of the last
  // token cannot be used as an index into the char array.
  // The least painful way to address this was to add a
  // newline to the array.
  val content = (
    if (content0.length == 0 || !content0.last.isWhitespace)
      content0 :+ '\n'
    else content0
  )
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

  private def charAtIsEOL(idx: Int)(p: Char => Boolean) = {
    // don't identify the CR in CR LF as a line break, since LF will do.
    def notCRLF0 = content(idx) != CR || !content.isDefinedAt(idx + 1) || content(idx + 1) != LF

    idx < length && notCRLF0 && p(content(idx))
  }

  def isLineBreak(idx: Int) = charAtIsEOL(idx)(isLineBreakChar)

  /** True if the index is included by an EOL sequence. */
  def isEndOfLine(idx: Int) = (content isDefinedAt idx) && PartialFunction.cond(content(idx)) {
    case CR | LF => true
  }

  /** True if the index is end of an EOL sequence. */
  def isAtEndOfLine(idx: Int) = charAtIsEOL(idx) {
    case CR | LF => true
    case _       => false
  }

  def calculateLineIndices(cs: Array[Char]) = {
    val buf = new ArrayBuffer[Int]
    buf += 0
    for (i <- 0 until cs.length) if (isAtEndOfLine(i)) buf += i + 1
    buf += cs.length // sentinel, so that findLine below works smoother
    buf.toArray
  }
  private lazy val lineIndices: Array[Int] = calculateLineIndices(content)

  def lineToOffset(index : Int): Int = lineIndices(index)

  private var lastLine = 0

  /** Convert offset to line in this source file.
   *  Lines are numbered from 0.
   */
  def offsetToLine(offset: Int): Int = {
    val lines = lineIndices
    def findLine(lo: Int, hi: Int, mid: Int): Int = (
      if (mid < lo || hi < mid) mid // minimal sanity check - as written this easily went into infinite loopyland
      else if (offset < lines(mid)) findLine(lo, mid - 1, (lo + mid - 1) / 2)
      else if (offset >= lines(mid + 1)) findLine(mid + 1, hi, (mid + 1 + hi) / 2)
      else mid
    )
    lastLine = findLine(0, lines.length, lastLine)
    lastLine
  }

  override def equals(that : Any) = that match {
    case that : BatchSourceFile => file.path == that.file.path && start == that.start
    case _ => false
  }
  override def hashCode = file.path.## + start.##
}
