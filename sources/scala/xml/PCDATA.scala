// BE

package scala.xml;

import scala.xml.javaAdapter.Map ;

case class PCDATA( content:String ) extends Element  {

              def getName     = "PCDATA";
              def getChildren = error("PCDATA.getChildren");
              def getAttribs  = error("PCDATA.getAttribs");
	      def setAttribs( m:Map[ String, String ] ):Unit = error("PCDATA.setAttribs");

              override def toXML:String = content;

} // PCDATA

