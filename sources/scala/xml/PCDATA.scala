// BE

package scala.xml;

import scala.xml.javaAdapter.Map ;

/** an XML representation text. Used in both generic and specific XML representation
*/

case class PCDATA( content:String ) extends Element  {

              def getName     = "PCDATA";
              def getChildren = error("PCDATA.getChildren");
	      def setChildren( l:Seq[ Element ] ):Unit = error("PCDATA.setChildren");
              def getAttribs  = error("PCDATA.getAttribs");
	      def setAttribs( m:Map[ String, String ] ):Unit = error("PCDATA.setAttribs");

  /** like toString, but escaping the characters &lt; &gt; &amp; and &quot; as demanded by XML standard.
  */

              override def toXML:String = {
                // new java.util.StringBuffer !!crashes!!
                val s = new StringBuffer();
                var i = 0;
                while( i < content.length() ) {
                  val c = content.charAt( i );
                  c match {
                        case '<' => s.append("&lt;");
                        case '>' => s.append("&gt;");
                        case '&' => s.append("&amp;");
                        case '"' => s.append("&quot;");
                        case _   => s.append( c );
                  }
                  i = i + 1;
                }
                s.toString();
              }

              override def hashCode() = content.hashCode();
	      override def toString() = "PCDATA("+content+")";

} // PCDATA

