/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */
package scala.xml ;

/** this classes is for creating text nodes with trimmed whitespace.
 *  all occurrences of one or more whitespace in strings appended (using the
 *  append method) will be replaced by a single space character
 */
class TextBuffer {

  val sb = new StringBuffer();
  var ws = true;

  def appendSpace        = if( !ws ) { ws = true;  sb.append(' ');} else {};
  def appendChar(c:char) =           { ws = false; sb.append( c );}

  /** appends this string to the text buffer, trimming whitespaces as needed */
  def append(  cs:Seq[Char] ):TextBuffer = {
    for( val c <- cs ) {
      if( Parsing.isSpace( c ) )
        appendSpace;
      else
        appendChar( c )
    }
    this
  }

  /** returns an empty sequence if text is only whitespace */
  def toText:Seq[Text] = {
    var len = sb.length(); /* invariant */
    if( len == 0 ) return Nil;

    if( Parsing.isSpace( sb.charAt( len - 1 ) )) {
      len = len - 1;
      sb.setLength( len )
    }
    if( len == 0 ) return Nil;

    List( Text( sb.toString() ) );
  }

}
