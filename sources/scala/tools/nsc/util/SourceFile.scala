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


  val file    = _file;
  val content = normalize(_content);

  def getContent() = content;

  def getFile() = file;

  def this(sourceName: String, content : Array[Char]) =
    this(new CharArrayFile(sourceName, content), content);

  def isLineBreak(idx : Int) = SourceFile.isLineBreak(content(idx));


  def position(offset : Int) = new Position(this, offset);
  def position(line : Int, column : Int) = new Position(this, lineToOffset(line) + column);

  // constants

  // NOTE: all indexes are based on zero!!!!
  override def toString(): String = file.getName() + ":" + content.length;


  object line {
    var index  = 0;
    var offset = 0;

    private def reset: Unit = {
      index = 0; offset = 0;
    }

    def find(toFind : Int, isIndex : Boolean) : Int = {
      if (!isIndex) assert(toFind != Position.NOPOS);
      if ( isIndex) assert(toFind > Position.NOLINE - Position.FIRSTLINE);

      if (!isIndex && (toFind >= content.length)) throw new Error(toFind + " not valid offset in " + file.getName() + ":" + content.length);

      if ( isIndex && toFind <  index) reset;
      if (!isIndex && toFind < offset) reset;
      try {
	var seek = 0;
	var continue = true;
	while (continue) {
	  if (false) {;}
	  else if ( isIndex && seek == 0 && toFind == index   ) continue = false;
	  else if (!isIndex &&      toFind ==  offset + seek  ) continue = false;
	  else if (!isIndex &&     (toFind  < (offset + seek))) throw new Error("HOW??? toFind=" + toFind + " offset=" + offset + " seek=" + seek);
	  else if (isLineBreak(offset + seek)) {
	    index  = index         + 1;
	    offset = offset + seek + 1;
	    seek = 0;
	  } else seek = seek + 1;
	}
	if (isIndex) offset else index;
      } catch {
	  case ex: ArrayIndexOutOfBoundsException =>
	    System.err.println("XXX: toFind=" + toFind + " isIndex=" + isIndex + " length=" + content.length);
	throw ex;
      }
    }
  }
  def offsetToLine(offset : Int) : Int = line.find(offset, false);
  def lineToOffset(index  : Int) : Int = line.find(index , true);

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
