/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$
package scala.tools.nsc.util

trait Position {
  def offset : Option[Int] = None
  private val tabInc = 8
  def line : Option[Int] =
    if (source.isEmpty || offset.isEmpty) None else Some(source.get.offsetToLine(offset.get) + 1)
  def column : Option[Int] = {
    if (source.isEmpty || offset.isEmpty) return None
    var column = 1
    // find beginning offset for line
    val line = source.get.offsetToLine(offset.get)
    var coffset = source.get.lineToOffset(line)
    var continue = true
    while (continue) {
      if (coffset == offset.get(-1)) continue = false
      else if (source.get.content(coffset) == '\t') column = ((column - 1) / tabInc * tabInc) + tabInc + 1
      else column = column + 1
      coffset = coffset + 1
    }
    Some(column)
  }
  def source : Option[SourceFile] = None
  def lineContent: String =
    if (!line.isEmpty && !source.isEmpty) source.get.lineToString(line.get - 1)
    else "NO_LINE"
  /** Map this position to a position in an original source
   * file.  If the SourceFile is a normal SourceFile, simply
   * return this.
   */
  def inUltimateSource = if (!source.isEmpty) source.get.positionInUltimateSource(this)
                         else this

  def dbgString = {
    (if (source.isEmpty) "" else "source-" + source.get.path) +
      (if (line.isEmpty) "" else "line-" + line.get) +
        (if (offset.isEmpty || source.isEmpty) ""
         else if (offset.get >= source.get.content.length) "out-of-bounds-" + offset.get
         else {
           val ret = "offset=" + offset.get;
           var add = "";
           while (offset.get + add.length < source.get.content.length &&
                  add.length < 10) add = add + source.get.content(offset.get + add.length());
           ret + " c[0..9]=\"" + add + "\"";
         })
  }

}

object NoPosition extends Position;
case class FakePos(msg : String) extends Position;

case class LinePosition(line0 : Int, override val source : Option[SourceFile]) extends Position {
  def this(line0 : Int) = this(line0, None)
  assert(line0 >= 1)
  override def offset = None
  override def column = None
  override def line = Some(line0)
}
case class OffsetPosition(source0 : SourceFile, offset0 : Int) extends Position {
  override def source = Some(source0)
  override def offset = Some(offset0)
}