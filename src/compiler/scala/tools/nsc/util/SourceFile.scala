/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */


package scala.tools.nsc.util;
import scala.tools.util.AbstractFile;
import scala.tools.util.CharArrayFile;

/** Uses positions that are offsets rather than line/column pairs.
 *
 *  @author Sean McDirmid
 */
object SourceFile {
  val LF : Char = 0x0A;
  val FF : Char = 0x0C;
  val CR : Char = 0x0D;
  val SU : Char = 0x1A;
  def isLineBreak(c : Char) = c == LF || c == FF || c == CR || c == SU;
}


class SourceFile(_file : AbstractFile, _content : Array[Char]) {
  import SourceFile._;

  def this(_file: AbstractFile) = {
    this(_file, (new String(_file.read())).toCharArray)
  }

  val file    = _file;
  val content = normalize(_content);

  def getContent() = content;

  def getFile() = file;

  def this(sourceName: String, content : Array[Char]) =
    this(new CharArrayFile(sourceName, content), content);

  def isLineBreak(idx : Int) = if (!SourceFile.isLineBreak(content(idx))) false;
			       else if (content(idx) == CR && idx + 1 < content.length && content(idx + 1) == LF) false;
			       else true;


  def position(offset : Int) = new Position(this, offset);
  def position(line : Int, column : Int) = new Position(this, lineToOffset(line) + column);

  // constants

  // NOTE: all indexes are based on zero!!!!
  override def toString(): String = file.getName() /* + ":" + content.length */ ;


  def dbg(offset : Int) = (new Position(this, offset)).dbgString;


  object line {
    var index  = 0;
    var offset = 0;

    def find(toFind : Int, isIndex : Boolean) : Int = {
      if (toFind == 0) return 0;

      if (!isIndex) assert(toFind != Position.NOPOS);
      if ( isIndex) assert(toFind > Position.NOLINE - Position.FIRSTLINE);

      if (!isIndex && (toFind >= content.length)) throw new Error(toFind + " not valid offset in " + file.getName() + ":" + content.length);

      def get(isIndex : Boolean) = if (isIndex) index else offset;

      val isBackward = toFind <= get(isIndex);
      val increment = if (isBackward) -1 else + 1;
      val oneIfBackward = if (isBackward) +1 else 0;

      // System.err.println("FIND-0: " + toFind + " " + isIndex);

      while (true) {
	// System.err.println("FIND-1: " + offset + " " + index);

	if (!isIndex && offset == toFind) return index;
	if ( isBackward && offset <= 0) throw new Error(offset + " " + index + " " + toFind + " " + isIndex);
	offset = offset + increment;
	if (!isBackward) assert(offset < content.length);

	if (isLineBreak(offset + (if (isBackward) 0 else -1))) {
	  index = index + increment;
	  if (isIndex && index + oneIfBackward == toFind)
	    return      offset + oneIfBackward;
	}
      }
      throw new Error();
    }
  }
  def offsetToLine(offset : Int) : Int = line.find(offset, false);
  def lineToOffset(index  : Int) : Int = line.find(index , true);

  def beginsWith(offset : Int, text : String): Boolean = {
    var idx = 0;
    while (idx < text.length()) {
      if (offset + idx >= content.length) return false;
      if (content(offset + idx) != text.charAt(idx)) return false;
      idx = idx + 1;
    }
    return true;
  }
  def path = getFile().getPath();

  def skipWhitespace(offset : Int): Int =
    if (Character.isWhitespace(content(offset))) skipWhitespace(offset + 1) else offset;


  def lineToString(index : Int) = {
    var offset = lineToOffset(index);
    val buf = new StringBuffer();
    while (!isLineBreak(offset) && offset < content.length) {
      buf.append(content(offset));
      offset = offset + 1;
    }
    buf.toString();
  }


  private def normalize(input : Array[char]): Array[char] =
    if (input.length > 0 && input(input.length - 1) == SU) input;
    else {
      val content = new Array[char](input.length + 1);
      System.arraycopy(input, 0, content, 0, input.length);
      content(input.length) = SU;
      content;
    }




}
