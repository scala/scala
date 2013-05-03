/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala
package reflect.internal.util

/** new for position ranges */
class RangePosition(source: SourceFile, override val start: Int, point: Int, override val end: Int)
extends OffsetPosition(source, point) {
  if (start > end) scala.sys.error("bad position: "+show)
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
