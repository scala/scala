/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml ;

import java.lang.StringBuffer ; /* Java dependency! */
import scala.collection.Map ;

/** Utility functions for processing instances of bound and not bound XML
**  classes, as well as escaping text nodes
**/

object Utility {

  def view( s:String ):Text = Text( s );

  /** representation of text in a string that is well-formed XML.
  **  the characters &lt; &gt; &amp; and &quot; are escaped
  */
  def escape( text:String ) = {
    val s = new StringBuffer();
    for (val c <- Iterator.fromString(text)) c match {
      case '<' => s.append("&lt;");
      case '>' => s.append("&gt;");
      case '&' => s.append("&amp;");
      case '"' => s.append("&quot;");
      case _   => s.append( c );
    }
    s.toString();
  }

  /** serializes an instance of Node to a string that contains well-formed XML
   *  @todo define a way to escape literal characters to &amp;xx; references
  */
  def toXML( n:Node ):String = n match {
    case Text( t ) =>
      escape( t );
    case _:EntityRef | _:Comment | _:ProcInstr =>
      n.toString();
    case _ =>
      val s = new StringBuffer();
      toXML( n, s );
      s.toString();
  }

  /** serializes an instance of Node to the given stringbuffer
  */
  def toXML( n:Node, s:StringBuffer ):Unit = n match {
    case Text( t ) =>
      s.append( escape( t ) );
    case _:EntityRef | _:Comment | _:ProcInstr =>
      s.append( n.toString() );
    case x:Node => {
      s.append('<');
      s.append( x.label );
      if( x.attribute.size != 0 ) {
        attr2xml( x.attribute.elements, s )
      }
      s.append('>');
      for( val c <- x.child.elements ) {
        toXML( c, s );
      }
      s.append("</");
      s.append( x.label );
      s.append('>');
    }
  }

  /*
  def toXML( ch:Iterator[Node] ):String = {
    ch.foldLeft ("") { (s:String,n:Node) => s + n.toString() }
  }
*/

  /** for a Node n, returns string representation of n.attributes **/
  def attr2xml( attrib:Iterator[Pair[String, String]], sb:StringBuffer ):Unit = {
    for( val x <- attrib ) {
      sb.append( " " );
      sb.append( x._1 );
      sb.append("=");
      val sep = { if( x._2.indexOf('"') != -1 ) '\'' else '"' };
      sb.append( sep );
      sb.append( x._2 );
      sb.append( sep );
    }
  }


  /** for a Node n, returns string representation of n.attributes **/
  def attr2xml( attrib:Iterator[Pair[String, String]] ):String = {
    val t = new StringBuffer();
    attr2xml( attrib, t );
    t.toString();
  }

  /** returns a hashcode for the given constituents of a node */
  def hashCode( label:String, attribHashCode:int, children:Seq[Node] ) = {
    label.hashCode() + attribHashCode + children.hashCode()
  }

  /** returns a hashcode for the given constituents of a node */
  def hashCode( label:String, attribs:scala.collection.mutable.HashMap[String,String], children:Seq[Node] ) = {
    label.hashCode() + attribs.toList.hashCode() + children.hashCode()
  }

  /** NameChar ::= Letter | Digit | '.' | '-' | '_' | ':'
   *             | CombiningChar | Extender
   *
   * see [4] and Appendix B of XML 1.0 specification
  */
  def isNameChar( ch:Char ) = isNameStart( ch ) || (ch match {
    case '.' | '-' | ':' => true;
    case _ => java.lang.Character.getType( ch ).asInstanceOf[Byte] match {
      case java.lang.Character.COMBINING_SPACING_MARK => true; // Mc
      case java.lang.Character.ENCLOSING_MARK => true;         // Me
      case java.lang.Character.NON_SPACING_MARK => true;       // Mn
      case java.lang.Character.MODIFIER_LETTER => true;        // Lm
      case java.lang.Character.DECIMAL_DIGIT_NUMBER => true;   // Nd
      case _ => false;
    }
  });

  /** NameStart ::= ( Letter | '_' )
   *  where Letter means in one of the Unicode general
   *  categories { Ll, Lu, Lo, Lt, Nl }
   *
   *  We do not allow a name to start with ':'.
   *  see [3] and Appendix B of XML 1.0 specification
   */
  def isNameStart( ch:Char ) =
    java.lang.Character.getType( ch ).asInstanceOf[Byte] match {
      case  java.lang.Character.LOWERCASE_LETTER => true;
      case  java.lang.Character.UPPERCASE_LETTER => true;
      case  java.lang.Character.OTHER_LETTER     => true;
      case  java.lang.Character.TITLECASE_LETTER => true;
      case  java.lang.Character.LETTER_NUMBER    => true;
      case _ => ch match {
        case '_' => true
        case _ => false;
      }
    }

  /** Name ::= ( Letter | '_' ) (NameChar)*
   *
   *  see  [5] of XML 1.0 specification
   */
  def isName( s:String ):boolean = {
    if( s.length() > 0 ) {
      val z:Seq[Char] = s;
      val y           = z.elements;
      if( isNameStart( y.next ) ) {
        while( y.hasNext && isNameChar( y.next ) ) {};
        !y.hasNext
      } else false;
    } else false;
  }

  def isPubIDChar( c:Char ) = c match {
    case '\u0020' | '\u000D' | '\u000A' => true;
    case _ if
      ('0' < c && c < '9')||('a' < c && c < 'z')||('A' < c && c < 'Z') => true;
    case '-' | '\''| '(' | ')' | '+' | ',' | '.' | '/' | ':'  | '=' |
         '?' | ';' | '!' | '*' | '#' | '@' | '$' | '_' | '%'           => true
    case _ => false;
  }

  def checkSysID( s:String ):boolean = {
    s.indexOf('"') == -1 || s.indexOf('\'') == -1
  }

  def checkPubID( s:String ):boolean = {
    if( s.length() > 0 ) {
      val z:Seq[Char] = s;
      val y = z.elements;
      while( y.hasNext && isPubIDChar( y.next ) ){};
      !y.hasNext
    } else true
  }

  def systemLiteralToString( s:String ) = {
    val ch = {
      if( s.indexOf('"') != -1 ) '\'' else '"'
    }
    new StringBuffer("SYSTEM ").append(ch).append(s).append(ch).toString();
  }

  def publicLiteralToString( s:String ) = {
    val ch = {
      if( s.indexOf('\'') != -1 ) '"' else '\''
    }
    new StringBuffer("PUBLIC ").append(ch).append(s).append(ch).toString();
  }

}
