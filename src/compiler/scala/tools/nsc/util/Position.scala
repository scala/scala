/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.tools.nsc.util


import compat.StringBuilder

/** This position uses offset in character buffer rather than line/column
 *  relationship.
 *
 *  @author  Sean McDirmid
 *  @version 1.0
 */
object Position {
  val NOPOS    = -1
  val FIRSTPOS = 0
  val NOLINE   = 0
  val FIRSTLINE = 1

  def line(source : SourceFile, offset : Int) =
    (new Position(source, offset)).line

  def lineToOffset(line : Int) = {
    assert(line >= 1)
    NOPOS - line
  }

  def offsetToLine(pos : Int) = {
    assert(pos < NOPOS)
    NOPOS - pos
  }
}

class Position( val source : SourceFile, val offset: Int) {
  import Position._

  private val tabInc = 8

  //def this(sourceName : String) = this(new SourceFile(sourceName, new Array[Char](0)), Position.NOPOS);
  //def this(sourceName : String, _offset : Int) = this(new SourceFile(sourceName, new Array[Char](0)), _offset);

  private def hasOffset = offset > NOPOS
  private def isLine    = offset < NOPOS

  def line: Int =
    if (hasOffset) source.offsetToLine(offset) + FIRSTLINE
    else if (isLine) Position.offsetToLine(offset)
    else NOLINE

  // for display purposes only.
  def column: Int = if (hasOffset) {
    var column = 1

    // find beginning offset for line
    val line = source.offsetToLine(offset)
    var coffset = source.lineToOffset(line)
    var continue = true
    while (continue) {
      if (coffset == offset) continue = false
      else if (source.content(coffset) == '\t') column = ((column - 1) / tabInc * tabInc) + tabInc + 1;
      else column = column + 1;
      coffset = coffset + 1
    }
    column
  } else 0

  /** Map this position to a position in an original source
    * file.  If the SourceFile is a normal SourceFile, simply
    * return this.
    */
  def inUltimateSource = source.positionInUltimateSource(this)

  def dbgString = {
    "source: " + (if (source eq null) source else source . path) + " " +
    (if (isLine) "line-" + line
    else if (!hasOffset) "NOP"
    else if (offset >= source.content.length) "out-of-bounds-" + offset else {
      val ret = "offset=" + offset + " line=" + line
      var add = ""
      while (offset + add.length() < source.content.length &&
             add.length() < 10) add = add + source.content(offset + add.length());
      ret + " c[0..9]=\"" + add + "\"";
    })
  }

  def lineContent: String =
    if (hasOffset) source.lineToString(line - FIRSTLINE)
    else "NO_LINE"

  /** Returns a string representation of the encoded position. */
  override def toString(): String = {
    if(inUltimateSource != this)
      return inUltimateSource.toString

    val sb = new StringBuilder()
    if (source ne null) {
      sb.append(source.file.path)
      if (hasOffset) {
        sb.append(line)
        sb.append(':')
        sb.append(column)
      }
    } else sb.append("::" + offset)
    sb.toString()
  }
}

/** this class merely serves to communicate a string to ConsoleReporter
 */
case class FakePos(msg: String) extends Position(null, Position.NOPOS) {
  override def inUltimateSource = this
}
