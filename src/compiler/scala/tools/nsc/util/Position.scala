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

  def start: Int = mid
  def mid: Int = offset.get
  def end: Int = mid

  def startOrElse(d: Int) = offset.get//OrElse(d)
  def midOrElse(d: Int) = offset.get//OrElse(d)
  def endOrElse(d: Int) = offset.get//OrElse(d)

  def includes(pos: Position) =
    isDefined && pos.isDefined && start <= pos.start && pos.end <= end

  def precedes(pos: Position) =
    isDefined && pos.isDefined && end <= pos.start

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

}

case object NoPosition extends Position
case class FakePos(msg: String) extends Position {
  override def toString=msg
}

case class OffsetPosition(source0: SourceFile, offset0: Int) extends Position {
  override def source = Some(source0)
  override def offset = Some(offset0)
  override def isDefined = true
  override def equals(that : Any) = that match {
  case that : OffsetPosition => offset0 == that.offset0 && source0.file == that.source0.file
  case that => false
  }
  override def hashCode = offset0 * 37 + source0.file.hashCode
}

/** new for position ranges */
class RangePosition(source0: SourceFile, override val start: Int, override val mid: Int, override val end: Int)
extends OffsetPosition(source0, mid) {
  override def isDefined = true
  override def startOrElse(d: Int) = start
  override def midOrElse(d: Int) = mid
  override def endOrElse(d: Int) = end
}


