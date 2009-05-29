/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.util

object Position {
  // a static field
  private val tabInc = 8
}

trait Position {
  import Position.tabInc
  def offset: Option[Int] = None
  def source: Option[SourceFile] = None
  def isDefined: Boolean = false
  def isSynthetic: Boolean = false
  def toSynthetic: Position = this

  def start: Int = point
  def point: Int = offset.get
  def end: Int = point

  def startOrElse(d: Int) = offset.getOrElse(d)
  def pointOrElse(d: Int) = offset.getOrElse(d)
  def endOrElse(d: Int) = offset.getOrElse(d)

  def withStart(off: Int) = this
  def withEnd(off: Int) = this
  def withPoint(off: Int) = this

  /** If this is a range, the union with the other range, with the point of this position.
   *  Otherwise, this position
   */
  def union(pos: Position) = this

  def underlying = this

  /** If this is a range position, the offset position of its start.
   *  Otherwise the position itself
   */
  def focusStart = this

  /** If this is a range position, the offset position of its point.
   *  Otherwise the position itself
   */
  def focusPoint = this

  /** If this is a range position, the offset position of its end.
   *  Otherwise the position itself
   */
  def focusEnd = this

  def includes(pos: Position) =
    isDefined && pos.isDefined && start <= pos.start && pos.end <= end

  def properlyIncludes(pos: Position) =
    includes(pos) && (start < pos.start || pos.end < end)

  /** Does this position precede that position?
   */
  def precedes(pos: Position) =
    isDefined && pos.isDefined && end <= pos.start

  def properlyPrecedes(pos: Position) =
    precedes(pos) && start < pos.end

  def overlaps(pos: Position) =
    isDefined && pos.isDefined &&
    (pos.start <= start && start < pos.end) || (start <= pos.start && pos.start < end)

  def sameRange(pos: Position) =
    isDefined && pos.isDefined && start == pos.start && end == pos.end

  def line: Option[Int] =
    if (offset.isEmpty || source.isEmpty) None
    else Some(source.get.offsetToLine(offset.get) + 1)

  def column: Option[Int] =
    if (offset.isEmpty || source.isEmpty)
      None
    else {
      var column = 1
      // find beginning offset for line
      val line = source.get.offsetToLine(offset.get)
      var coffset = source.get.lineToOffset(line)
      var continue = true
      while (continue) {
        if (coffset == offset.getOrElse(-1)) continue = false
        else if (source.get.asInstanceOf[BatchSourceFile].content(coffset) == '\t')
          column = ((column - 1) / tabInc * tabInc) + tabInc + 1
        else column += 1
        coffset += 1
      }
      Some(column)
    }

  def lineContent: String = {
    val line = this.line
    if (!line.isEmpty) source.get.lineToString(line.get - 1)
    else "NO_LINE"
  }

  /** Map this position to a position in an original source
   * file.  If the SourceFile is a normal SourceFile, simply
   * return this.
   */
  def inUltimateSource(source : SourceFile) =
    if (source == null) this else source.positionInUltimateSource(this)

  def dbgString = {
    (if (source.isEmpty) "" else "source-" + source.get.path) +
      (if (line.isEmpty) "" else "line-" + line.get) +
        (if (offset.isEmpty) ""
         else if (offset.get >= source.get.length) "out-of-bounds-" + offset.get
         else {
           val ret = "offset=" + offset.get;
           var add = "";
           /*
           while (offset.get + add.length < source.get.length &&
                  add.length < 10) add = add + source.get.content(offset.get + add.length());
           */
           ret + " c[0..9]=\"" + add + "\"";
         })
  }

  def show: String = "["+toString+"]"
}

case object NoPosition extends Position
case class FakePos(msg: String) extends Position {
  override def toString=msg
}

case class OffsetPosition(source0: SourceFile, offset0: Int) extends Position {
  override def source = Some(source0)
  override def offset = Some(offset0)
  override def withPoint(off: Int) = new OffsetPosition(source0, off)
  override def isDefined = true
  override def toSynthetic: Position = new SyntheticOffsetPosition(source0, offset0)
  override def equals(that : Any) = that match {
  case that : OffsetPosition => offset0 == that.offset0 && source0.file == that.source0.file
  case that => false
  }
  override def hashCode = offset0 * 37 + source0.file.hashCode
  override def show = "["+point+"]"
}

class SyntheticOffsetPosition(source0: SourceFile, offset0: Int) extends OffsetPosition(source0, offset0) {
  override def isSynthetic = true
  override def toSynthetic = this
  override def withPoint(off: Int) = new SyntheticOffsetPosition(source0, off)
}

/** new for position ranges */
class RangePosition(source0: SourceFile, override val start: Int, override val point: Int, override val end: Int)
extends OffsetPosition(source0, point) {
  override def isDefined = true
  override def startOrElse(d: Int) = start
  override def pointOrElse(d: Int) = point
  override def withStart(off: Int) = new RangePosition(source0, off, point, end)
  override def withEnd(off: Int) = new RangePosition(source0, start, off, end)
  override def withPoint(off: Int) = new RangePosition(source0, start, point, off)
  override def union(pos: Position) = new RangePosition(source0, start min pos.start, point, end max pos.end)
  override def endOrElse(d: Int) = end
  override def focusStart = OffsetPosition(source0, start)
  override def focusPoint = OffsetPosition(source0, point)
  override def focusEnd = OffsetPosition(source0, end)
  override def toSynthetic = new SyntheticRangePosition(source0, start, point, end)
  override def toString = "RangePosition("+source0+", "+start+", "+point+", "+end+")"
  override def show = "["+start+":"+end+"]"
}

/** A position to be used for synthetic trees that do not correspond to some original tree
 *  @note Trees with synthetic positions may not contain trees with real positions inside them!
 */
class SyntheticRangePosition(source0: SourceFile, start: Int, point: Int, end: Int) extends RangePosition(source0, start, point, end) {
  override def isSynthetic = true
  override def toSynthetic = this
  override def withStart(off: Int) = new SyntheticRangePosition(source0, off, point, end)
  override def withEnd(off: Int) = new SyntheticRangePosition(source0, start, off, end)
  override def withPoint(off: Int) = new SyntheticRangePosition(source0, start, point, off)
  override def union(pos: Position) = new SyntheticRangePosition(source0, start min pos.start, point, end max pos.end)
  override def focusStart = new SyntheticOffsetPosition(source0, start)
  override def focusPoint = new SyntheticOffsetPosition(source0, point)
  override def focusEnd = new SyntheticOffsetPosition(source0, end)
}




