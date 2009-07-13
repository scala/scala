/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.util
import scala.tools.nsc.io.{AbstractFile, VirtualFile}
import annotation.tailrec

object SourceFile {
  // Be very careful touching these.
  // Apparently trivial changes to the way you write these constants
  // will cause Scanners.scala to go from a nice efficient switch to
  // a ghastly nested if statement which will bring the type checker
  // to its knees. See ticket #1456
  final val LF = '\u000A'
  final val FF = '\u000C'
  final val CR = '\u000D'
  final val SU = '\u001A'

  def isLineBreakChar(c: Int) = c match {
    case LF|FF|CR|SU  => true
    case _            => false
  }
}
/** abstract base class of a source file used in the compiler */
abstract class SourceFile {
  import SourceFile._
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
    content drop lineToOffset(index) takeWhile (c => !isLineBreakChar(c)) mkString

  @tailrec
  final def skipWhitespace(offset: Int): Int =
    if (content(offset).isWhitespace) skipWhitespace(offset + 1) else offset

  def identifier(pos: Position, compiler: Global): Option[String] = None
}

/** a file whose contents do not change over time */
class BatchSourceFile(val file : AbstractFile, val content: Array[Char]) extends SourceFile {
  import SourceFile._

  def this(_file: AbstractFile)                 = this(_file, _file.toCharArray)
  def this(sourceName: String, cs: Seq[Char])   = this(new VirtualFile(sourceName), cs.toArray)
  def this(file: AbstractFile, cs: Seq[Char])   = this(file, cs.toArray)

  override def equals(that : Any) = that match {
    case that : BatchSourceFile => file == that.file
    case _ => false
  }
  override def hashCode = file.hashCode
  val length = content.length

  // in SourceFileFragments, these are overridden to compensate during offset calculation
  // Invariant: length + start = underlyingLength
  def underlyingLength = length
  def start = 0

  override def identifier(pos: Position, compiler: Global) = pos match {
    case OffsetPosition(source, offset) if source == this && offset != -1 =>
      def isOK(c: Char) = {
        import compiler.syntaxAnalyzer.{ isOperatorPart, isIdentifierPart }
        isIdentifierPart(c) || isOperatorPart(c)
      }
      Some(new String(content drop offset takeWhile isOK))
    case _ =>
      super.identifier(pos, compiler)
  }

  def isLineBreak(idx: Int) =
    if (idx >= length) false else content(idx) match {
      // don't identify the CR in CR LF as a line break, since LF will do.
      case CR => (idx + 1 == length) || (content(idx + 1) != LF)
      case x  => isLineBreakChar(x)
    }

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
  def this(name: String, components: BatchSourceFile*) = {
    this(
      name,
      components.toList,
      Array.concat(components.toList.map(comp =>
        CompoundSourceFile.stripSU(comp.content).toArray):_*))
  }

  /** Create an instance with the specified components and a generic name. */
  def this(components: BatchSourceFile*) =
    this("(virtual file)", components.toList:_*)

  override def positionInUltimateSource(position: Position) = {
    if (position.offset.isEmpty) super.positionInUltimateSource(position)
    else {
      println("!!!")
      var off = position.offset.get
      var compsLeft = components
      // the search here has to be against the length of the files underlying the
      // components, not their advertised length (which in the case of a fragment is
      // less than the underlying length.) Otherwise we can and will overshoot the
      // correct component and return a garbage position.
      while (compsLeft.head.underlyingLength-1 <= off && !compsLeft.tail.isEmpty) {
        println("discarding "+compsLeft.head)
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
    if (chars.length > 0 && chars.last == SourceFile.SU)
      chars.slice(0, chars.length-1)
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

  override def positionInUltimateSource(position: Position) = {
    if (position.offset.isEmpty)
      super.positionInUltimateSource(position)
    else {
      super.positionInUltimateSource(
      new OffsetPosition(this, position.offset.get))
    }
  }
}
