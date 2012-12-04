/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 *
 */

package scala.reflect.internal.util

import scala.reflect.ClassTag
import scala.reflect.internal.FatalError
import scala.reflect.macros.Attachments

object Position {
  val tabInc = 8

  /** Prints the message with the given position indication. */
  def formatMessage(posIn: Position, msg: String, shortenFile: Boolean): String = {
    val pos = (
      if (posIn eq null) NoPosition
      else if (posIn.isDefined) posIn.inUltimateSource(posIn.source)
      else posIn
    )
    def file   = pos.source.file
    def prefix = if (shortenFile) file.name else file.path

    pos match {
      case FakePos(fmsg) => fmsg+" "+msg
      case NoPosition    => msg
      case _             =>
        List(
          "%s:%s: %s".format(prefix, pos.line, msg),
          pos.lineContent.stripLineEnd,
          " " * (pos.column - 1) + "^"
        ) mkString "\n"
    }
  }
}

/** The Position class and its subclasses represent positions of ASTs and symbols.
 *  Except for NoPosition and FakePos, every position refers to a SourceFile
 *  and to an offset in the sourcefile (its `point`). For batch compilation,
 *  that's all. For interactive IDE's there are also RangePositions
 *  and TransparentPositions. A RangePosition indicates a start and an end
 *  in addition to its point. TransparentPositions are a subclass of RangePositions.
 *  Range positions that are not transparent are called opaque.
 *  Trees with RangePositions need to satisfy the following invariants.
 *
 *  INV1: A tree with an offset position never contains a child
 *        with a range position
 *  INV2: If the child of a tree with a range position also has a range position,
 *        then the child's range is contained in the parent's range.
 *  INV3: Opaque range positions of children of the same node are non-overlapping
 *        (this means their overlap is at most a single point).
 *
 *  The following tests are useful on positions:
 *
 *  pos.isDefined     true if position is not a NoPosition nor a FakePosition
 *  pos.isRange       true if position is a range
 *  pos.isOpaqueRange true if position is an opaque range
 *
 *  The following accessor methods are provided:
 *
 *  pos.source        The source file of the position, which must be defined
 *  pos.point         The offset of the position's point, which must be defined
 *  pos.start         The start of the position, which must be a range
 *  pos.end           The end of the position, which must be a range
 *
 *  There are also convenience methods, such as
 *
 *  pos.startOrPoint
 *  pos.endOrPoint
 *  pos.pointOrElse(default)
 *
 *  These are less strict about the kind of position on which they can be applied.
 *
 *  The following conversion methods are often used:
 *
 *  pos.focus           converts a range position to an offset position, keeping its point;
 *                      returns all other positions unchanged.
 *  pos.makeTransparent converts an opaque range position into a transparent one.
 *                      returns all other positions unchanged.
 */
abstract class Position extends scala.reflect.api.Position { self =>

  type Pos = Position

  def pos: Position = this

  def withPos(newPos: Position): Attachments { type Pos = self.Pos } = newPos

  /** An optional value containing the source file referred to by this position, or
   *  None if not defined.
   */
  def source: SourceFile = throw new UnsupportedOperationException("Position.source")

  /** Is this position neither a NoPosition nor a FakePosition?
   *  If isDefined is true, offset and source are both defined.
   */
  def isDefined: Boolean = false

  /** Is this position a transparent position? */
  def isTransparent: Boolean = false

  /** Is this position a range position? */
  def isRange: Boolean = false

  /** Is this position a non-transparent range position? */
  def isOpaqueRange: Boolean = false

  /** if opaque range, make this position transparent */
  def makeTransparent: Position = this

  /** The start of the position's range, error if not a range position */
  def start: Int = throw new UnsupportedOperationException("Position.start")

  /** The start of the position's range, or point if not a range position */
  def startOrPoint: Int = point

  /**  The point (where the ^ is) of the position */
  def point: Int = throw new UnsupportedOperationException("Position.point")

  /**  The point (where the ^ is) of the position, or else `default` if undefined */
  def pointOrElse(default: Int): Int = default

  /** The end of the position's range, error if not a range position */
  def end: Int = throw new UnsupportedOperationException("Position.end")

  /** The end of the position's range, or point if not a range position */
  def endOrPoint: Int = point

  @deprecated("use point instead", "2.9.0")
  def offset: Option[Int] = if (isDefined) Some(point) else None

  /** The same position with a different start value (if a range) */
  def withStart(off: Int): Position = this

  /** The same position with a different end value (if a range) */
  def withEnd(off: Int): Position = this

  /** The same position with a different point value (if a range or offset) */
  def withPoint(off: Int): Position = this

  /** The same position with a different source value, and its values shifted by given offset */
  def withSource(source: SourceFile, shift: Int): Position = this

  /** If this is a range, the union with the other range, with the point of this position.
   *  Otherwise, this position
   */
  def union(pos: Position): Position = this

  /** If this is a range position, the offset position of its start.
   *  Otherwise the position itself
   */
  def focusStart: Position = this

  /** If this is a range position, the offset position of its point.
   *  Otherwise the position itself
   */
  def focus: Position = this

  /** If this is a range position, the offset position of its end.
   *  Otherwise the position itself
   */
  def focusEnd: Position = this

  /** Does this position include the given position `pos`.
   *  This holds if `this` is a range position and its range [start..end]
   *  is the same or covers the range of the given position, which may or may not be a range position.
   */
  def includes(pos: Position): Boolean = false

  /** Does this position properly include the given position `pos` ("properly" meaning their
   *  ranges are not the same)?
   */
  def properlyIncludes(pos: Position): Boolean =
    includes(pos) && (start < pos.startOrPoint || pos.endOrPoint < end)

  /** Does this position precede that position?
   *  This holds if both positions are defined and the end point of this position
   *  is not larger than the start point of the given position.
   */
  def precedes(pos: Position): Boolean =
    isDefined && pos.isDefined && endOrPoint <= pos.startOrPoint

  /** Does this position properly precede the given position `pos` ("properly" meaning their ranges
   *  do not share a common point).
   */
  def properlyPrecedes(pos: Position): Boolean =
    isDefined && pos.isDefined && endOrPoint < pos.startOrPoint

  /** Does this position overlap with that position?
   *  This holds if both positions are ranges and there is an interval of
   *  non-zero length that is shared by both position ranges.
   */
  def overlaps(pos: Position): Boolean =
    isRange && pos.isRange &&
    ((pos.start < end && start < pos.end) || (start < pos.end && pos.start < end))

  /** Does this position cover the same range as that position?
   *  Holds only if both position are ranges
   */
  def sameRange(pos: Position): Boolean =
    isRange && pos.isRange && start == pos.start && end == pos.end

  def line: Int = throw new UnsupportedOperationException("Position.line")

  def column: Int = throw new UnsupportedOperationException("Position.column")

  /** Convert this to a position around `point` that spans a single source line */
  def toSingleLine: Position = this

  def lineContent: String =
    if (isDefined) source.lineToString(line - 1)
    else "NO_LINE"

  /** Map this position to a position in an original source
   * file.  If the SourceFile is a normal SourceFile, simply
   * return this.
   */
  def inUltimateSource(source : SourceFile): Position =
    if (source == null) this else source.positionInUltimateSource(this)

  def dbgString: String = toString
  def safeLine: Int = try line catch { case _: UnsupportedOperationException => -1 }

  def show: String = "["+toString+"]"
}

case object NoPosition extends Position {
  override def dbgString = toString
}

case class FakePos(msg: String) extends Position {
  override def toString = msg
}

class OffsetPosition(override val source: SourceFile, override val point: Int) extends Position {
  override def isDefined = true
  override def pointOrElse(default: Int): Int = point
  override def withPoint(off: Int) = new OffsetPosition(source, off)
  override def withSource(source: SourceFile, shift: Int) = new OffsetPosition(source, point + shift)

  override def line: Int = source.offsetToLine(point) + 1

  override def column: Int = {
    var idx = source.lineToOffset(source.offsetToLine(point))
    var col = 0
    while (idx != point) {
      col += (if (source.content(idx) == '\t') Position.tabInc - col % Position.tabInc else 1)
      idx += 1
    }
    col + 1
  }

  override def union(pos: Position) = if (pos.isRange) pos else this

  override def equals(that : Any) = that match {
    case that : OffsetPosition => point == that.point && source.file == that.source.file
    case that => false
  }
  override def hashCode = point * 37 + source.file.hashCode

  override def toString = {
    val pointmsg = if (point > source.length) "out-of-bounds-" else "offset="
    "source-%s,line-%s,%s%s".format(source.file.canonicalPath, line, pointmsg, point)
  }
  override def show = "["+point+"]"
}

/** new for position ranges */
class RangePosition(source: SourceFile, override val start: Int, point: Int, override val end: Int)
extends OffsetPosition(source, point) {
  if (start > end) sys.error("bad position: "+show)
  override def isRange: Boolean = true
  override def isOpaqueRange: Boolean = true
  override def startOrPoint: Int = start
  override def endOrPoint: Int = end
  override def withStart(off: Int) = new RangePosition(source, off, point, end)
  override def withEnd(off: Int) = new RangePosition(source, start, point, off)
  override def withPoint(off: Int) = new RangePosition(source, start, off, end)
  override def withSource(source: SourceFile, shift: Int) = new RangePosition(source, start + shift, point + shift, end + shift)
  override def focusStart = new OffsetPosition(source, start)
  override def focus = {
    if (focusCache eq NoPosition) focusCache = new OffsetPosition(source, point)
    focusCache
  }
  override def focusEnd = new OffsetPosition(source, end)
  override def makeTransparent = new TransparentPosition(source, start, point, end)
  override def includes(pos: Position) = pos.isDefined && start <= pos.startOrPoint && pos.endOrPoint <= end
  override def union(pos: Position): Position =
    if (pos.isRange) new RangePosition(source, start min pos.start, point, end max pos.end) else this

  override def toSingleLine: Position = source match {
    case bs: BatchSourceFile
    if end > 0 && bs.offsetToLine(start) < bs.offsetToLine(end - 1) =>
      val pointLine = bs.offsetToLine(point)
      new RangePosition(source, bs.lineToOffset(pointLine), point, bs.lineToOffset(pointLine + 1))
    case _ => this
  }

  override def toString = "RangePosition("+source.file.canonicalPath+", "+start+", "+point+", "+end+")"
  override def show = "["+start+":"+end+"]"
  private var focusCache: Position = NoPosition
}

class TransparentPosition(source: SourceFile, start: Int, point: Int, end: Int) extends RangePosition(source, start, point, end) {
  override def isOpaqueRange: Boolean = false
  override def isTransparent = true
  override def makeTransparent = this
  override def show = "<"+start+":"+end+">"
}
