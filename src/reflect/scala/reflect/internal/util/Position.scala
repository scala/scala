/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package internal
package util

/** @inheritdoc */
class Position extends macros.EmptyAttachments with api.Position with InternalPositionImpl with DeprecatedPosition {
  type Pos = Position
  def pos: Position = this
  def withPos(newPos: Position): macros.Attachments { type Pos = Position.this.Pos } = newPos

  protected def fail(what: String) = throw new UnsupportedOperationException(s"Position.$what on $this")

  // If scala-refactoring extends Position directly it seems I have no
  // choice but to offer all the concrete methods.
  def isDefined          = false
  def isRange            = false
  def source: SourceFile = NoSourceFile
  def start: Int         = fail("start")
  def point: Int         = fail("point")
  def end: Int           = fail("end")
}

object Position {
  final val tabInc = 8

  private def validate[T <: Position](pos: T): T = {
    if (pos.isRange) {
      import pos.{pos => _, _}
      assert(start <= end, s"bad position: ${pos.show}")
      //assert(start <= point && point <= end, s"bad position: point $point out of range $start..$end: ${pos.show}")
      //assert(start <= point && point <= end, s"bad position: point $point out of range $start..$end: ${pos.show}\n${pos.lineContent}\n${pos.lineCaret}")
    }
    pos
  }

  /** Prints the message with the given position indication. */
  def formatMessage(posIn: Position, msg: String, shortenFile: Boolean): String = {
    val pos    = if (posIn eq null) NoPosition else posIn
    val prefix = pos.source match {
      case NoSourceFile     => ""
      case s if shortenFile => s.file.name + ":"
      case s                => s.file.path + ":"
    }
    prefix + (pos showError msg)
  }

  def offset(source: SourceFile, point: Int): Position                            = validate(new OffsetPosition(source, point))
  def range(source: SourceFile, start: Int, point: Int, end: Int): Position       = validate(new RangePosition(source, start, point, end))
  def transparent(source: SourceFile, start: Int, point: Int, end: Int): Position = validate(new TransparentPosition(source, start, point, end))
}

class OffsetPosition(sourceIn: SourceFile, pointIn: Int) extends DefinedPosition {
  override def isRange = false
  override def source  = sourceIn
  override def point   = pointIn
  override def start   = point
  override def end     = point
}
class RangePosition(sourceIn: SourceFile, startIn: Int, pointIn: Int, endIn: Int) extends DefinedPosition {
  override def isRange = true
  override def source  = sourceIn
  override def point   = pointIn
  override def start   = startIn
  override def end     = endIn
}
class TransparentPosition(sourceIn: SourceFile, startIn: Int, pointIn: Int, endIn: Int) extends RangePosition(sourceIn, startIn, pointIn, endIn) {
  override def isTransparent = true
}
case object NoPosition extends UndefinedPosition
case class FakePos(msg: String) extends UndefinedPosition {
  override def toString = msg
}

sealed abstract class DefinedPosition extends Position {
  final override def isDefined = true
  override def equals(that: Any) = that match {
    case that: DefinedPosition => source.file == that.source.file && start == that.start && point == that.point && end == that.end
    case _                     => false
  }
  override def hashCode = Seq[Any](source.file, start, point, end).##
  override def toString = (
    if (isRange) s"RangePosition($canonicalPath, $start, $point, $end)"
    else s"source-$canonicalPath,line-$line,$pointMessage$point"
  )
  private def pointMessage  = if (point > source.length) "out-of-bounds-" else "offset="
  private def canonicalPath = source.file.canonicalPath
}

sealed abstract class UndefinedPosition extends Position {
  final override def isDefined = false
  override def isRange         = false
  override def source          = NoSourceFile
  override def start           = fail("start")
  override def point           = fail("point")
  override def end             = fail("end")

  override def samePointAs(that: Position) = false
}

private[util] trait InternalPositionImpl {
  self: Position =>

  // The methods which would be abstract in Position if it were
  // possible to change Position.
  def isDefined: Boolean
  def isRange: Boolean
  def source: SourceFile
  def start: Int
  def point: Int
  def end: Int

  /** Map this position to its position in the original source file
   *  (which may be this position unchanged.)
   */
  def finalPosition: Pos = source positionInUltimateSource this

  def isTransparent              = false
  def isOffset                   = isDefined && !isRange
  def isOpaqueRange              = isRange && !isTransparent
  def pointOrElse(alt: Int): Int = if (isDefined) point else alt
  def makeTransparent: Position  = if (isOpaqueRange) Position.transparent(source, start, point, end) else this
  final def makeTransparentIf(cond: Boolean): Position =
    if (cond && isOpaqueRange) Position.transparent(source, start, point, end) else this

  /* Copy a range position with a changed value. */
  /* Note: the result is validated (start <= end), use `copyRange` to update both at the same time. */
  /** If start differs, copy a range position or promote an offset. */
  def withStart(start: Int): Position = if (isDefined && this.start != start) copyRange(start = start) else this
  /** If point differs, copy a range position or return an offset. */
  def withPoint(point: Int): Position =
    if (!isDefined || this.point == point) this else if (isRange) copyRange(point = point) else asOffset(point)
  /** If end differs, copy a range position or promote an offset. */
  def withEnd(end: Int): Position     = if (isDefined && this.end != end) copyRange(end = end) else this
  def withSource(source: SourceFile): Position =
    if (isRange) copyRange(source = source)
    else if (isDefined) Position.offset(source, point)
    else this
  def withShift(shift: Int): Position =
    if (isRange) Position.range(source, start + shift, point + shift, end + shift)
    else if (isDefined) asOffset(point + shift)
    else this

  def copyRange(start: Int = start, point: Int = point, end: Int = end, source: SourceFile = source) =
    Position.range(source, start, point, end)

  /** Convert a range position to a simple offset.
   */
  def focusStart: Position = if (this.isRange) asOffset(start) else this
  def focus: Position      = if (this.isRange) asOffset(point) else this
  def focusEnd: Position   = if (this.isRange) asOffset(end) else this

  /** Convert an offset position to a degenerate range.
   *
   *  Note that withPoint does not promote to range, but withStart and withEnd may do so.
   *  It would be more disciplined to require explicit promotion with toRange.
   */
  def toRange: Position = if (this.isRange) this else copyRange()

  /** If you have it in for punctuation you might not like these methods.
   *  However I think they're aptly named.
   *
   *    |   means union
   *    ^   means "the point" (look, it's a caret)
   *    |^  means union, taking the point of the rhs
   *    ^|  means union, taking the point of the lhs
   */
  //def |(that: Position, poses: Position*): Position = poses.foldLeft(this | that)(_ | _)
  def |(that: Position): Position                   = this union that
  def ^(point: Int): Position                       = this withPoint point
  def |^(that: Position): Position                  = (this | that) ^ that.point
  def ^|(that: Position): Position                  = (this | that) ^ this.point

  /** Widen a range to include the other operand.
   *  If this position is a range, preserve its point; otherwise, the point of the other operand.
   *  Note that NoPosition | offset is not promoted to an offset position.
   *  Nor is offset | offset promoted to range.
   */
  def union(pos: Position): Position = {
    def ranged(point: Int) = Position.range(source, start = start.min(pos.start), point = point, end = end.max(pos.end))
    if (pos.isRange) {
      if (this.isRange) ranged(point)
      else if (this.isDefined) ranged(pos.point)
      else pos
    }
    else if (this.isRange && pos.isDefined && !this.includes(pos)) ranged(point)
    else this
  }

  def includes(pos: Position): Boolean         = isRange && pos.isDefined && start <= pos.start && pos.end <= end
  def properlyIncludes(pos: Position): Boolean = includes(pos) && (start < pos.start || pos.end < end)
  def precedes(pos: Position): Boolean         = bothDefined(pos) && end <= pos.start
  def properlyPrecedes(pos: Position): Boolean = bothDefined(pos) && end < pos.start
  def sameRange(pos: Position): Boolean        = bothRanges(pos) && start == pos.start && end == pos.end
  // This works because it's a range position invariant that S1 < E1 and S2 < E2.
  // So if S1 < E2 and S2 < E1, then both starts precede both ends, which is the
  // necessary condition to establish that there is overlap.
  def overlaps(pos: Position): Boolean         = bothRanges(pos) && start < pos.end && pos.start < end

  private def line0       = source.offsetToLine(point)
  private def lineOffset  = source.lineToOffset(line0)
  def line: Int           = if (hasSource) line0 + 1 else 0
  def column: Int         = if (!hasSource) 0 else {
    var idx = lineOffset
    var col = 0
    while (idx != point) {
      col += (if (source.content(idx) == '\t') Position.tabInc - col % Position.tabInc else 1)
      idx += 1
    }
    col + 1
  }
  def lineContent: String = if (hasSource) source.lineToString(line0) else ""
  def lineCaret: String   = if (!hasSource) "" else {
    val buf = new StringBuilder
    var idx = lineOffset
    while (idx < point) {
      buf.append(if (source.content(idx) == '\t') '\t' else ' ')
      idx += 1
    }
    buf.append('^')
    buf.toString
  }
  @deprecated("use `lineCaret`", since="2.11.0")
  def lineCarat: String = lineCaret

  def showError(msg: String): String = {
    def escaped(s: String) = {
      def u(c: Int) = f"\\u$c%04x"
      def uable(c: Int) = (c < 0x20 && c != '\t') || c == 0x7F
      if (s exists (c => uable(c))) {
        val sb = new StringBuilder
        s foreach (c => sb append (if (uable(c)) u(c) else c))
        sb.toString
      } else s
    }
    import java.lang.System.{lineSeparator => NL}
    finalPosition match {
      case FakePos(fmsg) => s"$fmsg $msg"
      case NoPosition    => msg
      case pos           => s"${pos.line}: ${msg}${NL}${escaped(pos.lineContent)}${NL}${pos.lineCaret}"
    }
  }
  def showDebug: String = toString
  def show = (
    if (isOpaqueRange) s"[$start:$end]"
    else if (isTransparent) s"<$start:$end>"
    else if (isDefined) s"[$point]"
    else "[NoPosition]"
  )

  /* Same as `this.focus == that.focus`, but less allocation-y. */
  def samePointAs(that: Position): Boolean =
    that.isDefined && this.point == that.point && this.source.file == that.source.file

  private def asOffset(point: Int): Position = Position.offset(source, point)
  private def hasSource                      = source ne NoSourceFile
  private def bothRanges(that: Position)     = isRange && that.isRange
  private def bothDefined(that: Position)    = isDefined && that.isDefined
}

/** Holding cell for methods unused and/or unnecessary. */
private[util] trait DeprecatedPosition {
  self: Position =>

  @deprecated("use `point`", "2.9.0") // Used in sbt 0.12.4
  def offset: Option[Int] = if (isDefined) Some(point) else None

  @deprecated("use `focus`", "2.11.0")
  def toSingleLine: Position = this

  @deprecated("use `line`", "2.11.0")
  def safeLine: Int = line

  @deprecated("use `showDebug`", "2.11.0")
  def dbgString: String = showDebug

  @deprecated("use `finalPosition`", "2.11.0")
  def inUltimateSource(source: SourceFile): Position = source positionInUltimateSource this

  @deprecated("use `lineCaret`", since="2.11.0")
  def lineWithCarat(maxWidth: Int): (String, String) = ("", "")

  @deprecated("use `withSource(source)` and `withShift`", "2.11.0")
  def withSource(source: SourceFile, shift: Int): Position = this withSource source withShift shift

  @deprecated("use `start` instead", "2.11.0")
  def startOrPoint: Int = if (isRange) start else point

  @deprecated("use `end` instead", "2.11.0")
  def endOrPoint: Int = if (isRange) end else point
}
