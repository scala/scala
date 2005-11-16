/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.tools.nsc.util;

/** This position uses offset in character buffer rather than line column relationship.
 *  @author Sean McDirmid
 */
object Position {
  val NOPOS    = -1;
  val FIRSTPOS = 0;
  val NOLINE   = 0;
  val FIRSTLINE = 1;


  def line(offset : Int) = offset;
}


class Position(_source : SourceFile,_offset: Int) {
  import Position._;

  val offset = _offset;
  val  source = _source;
  val tabInc = 8;


  def this(sourceName : String) = this(new SourceFile(sourceName, new Array[Char](0)), NOPOS);
  def this(sourceName : String, _offset : Int) = this(new SourceFile(sourceName, new Array[Char](0)), _offset);

  def hasOffset = offset != NOPOS;

  def   line: Int = if (hasOffset) source.offsetToLine(offset) + FIRSTLINE else NOLINE;
  // for display purposes only.
  def column: Int = if (hasOffset) {
    var column = 1;

    // find beginning offset for line
    val line    = source.offsetToLine  (offset);
    var coffset = source.  lineToOffset(line);
    var continue = true;
    while (continue) {
      if (coffset == offset) continue = false;
      else if (source.content(coffset) == '\t') column = ((column - 1) / tabInc * tabInc) + tabInc + 1;
      else column = column + 1;
      coffset = coffset + 1;
    }
    column;
  } else 0;


  def lineContent: String = if (hasOffset) source.lineToString(line - FIRSTLINE) else "NO_LINE";

  /** Returns a string representation of the encoded position. */
  override def toString(): String = {
    val sb = new StringBuffer();
    sb.append(source.file.getPath());
    if (hasOffset) {
      sb.append(line);
      sb.append(':');
      sb.append(column);
    }
    sb.toString();
  }
}
