package scala.xml ;

import java.lang.StringBuffer ; /* Java dependency! */
import scala.collection.Map ;
import scala.xml.nobinding.Element ;
/** Utility functions for processing instances of bound and not bound XML classes.
**/

object Utility {

  /** representation of text in a string that is well-formed XML.
  **  the characters &lt; &gt; &amp; and &quot; are escaped
  */
  def escape( text:String ) = {
    val s = new StringBuffer();
    for( val c <- new IterableString(text).elements ) c match {
      case '<' => val _ = s.append("&lt;");
      case '>' => val _ = s.append("&gt;");
      case '&' => val _ = s.append("&amp;");
      case '"' => val _ = s.append("&quot;");
      case _   => val _ = s.append( c );
    }
    s.toString();
  }

  /** serializes an instance of Node to a string that contains well-formed XML **/
  def toXML( n:Node ):String = n match {
    case Text( t ) => escape( t );
    case _ =>
    val s = new StringBuffer();
    s.append("<");
    s.append( n.label );
    if( null != n.attributes ) {
      s.append( attr2xml( n.attributes.elements ) );{}
    }
    s.append(">");
    s.append( toXML( n.children.elements ) );
    s.append("</");
    s.append( n.label );
    s.append(">");
    s.toString()
  }

  def toXML( ch:Iterator[Node] ):String = {
    ch.foldLeft ("") { (s:String,n:Node) => { val t:String = toXML( n ); s + t }}
  }

  /** for a Node n, returns string representation of n.attributes **/
  def attr2xml( attrib:Iterator[Pair[String, String]] ):String = {
    attrib.foldLeft ("") { (s:String,x:Pair[String,String]) => {
        val t = new StringBuffer(" ");
        t.append( s );
        t.append( x._1 );
        t.append("=\"");
        t.append( x._2 );
        t.append("\"");
        t.toString()
    }
    }
  }

  /** returns a hashcode for the given constituents of a node */
  def hashCode( label:String, attribHashCode:int, children:Seq[Node] ) = {
    label.hashCode() + attribHashCode + children.hashCode()
  }

}
