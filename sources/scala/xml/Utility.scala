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
  def hashCode(uri:String, label:String, attribHashCode:int, children:Seq[Node]) = {
    41 * uri.hashCode() % 7 + label.hashCode() + attribHashCode + children.hashCode()
  }

  /** returns a hashcode for the given constituents of a node */
  def hashCode(uri:String, label:String, attribs:scala.collection.mutable.HashMap[String,String], children:Seq[Node]) = {
    41 * uri.hashCode() % 7 + label.hashCode() + attribs.toList.hashCode() + children.hashCode()
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
