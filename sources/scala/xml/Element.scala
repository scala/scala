package scala.xml ;

import javaAdapter.Map;

/** interface to XML elements. used by dtd2scala tool and the generated XML parser interfaces.
 *  sorry, no attributes yet (needs working HashMap).
 *  future revisions might use sequence instead of list.
 */

abstract class Element {

    def getName:     String;                // the real element name
    def getChildren: Seq[ Element ];         // the children
    def setChildren( l:Seq[ Element ] ):Unit ;
    def getAttribs:  Map[ String, String ]; // disabled
    def setAttribs( m:Map[ String, String ] ):Unit ;

    def toXML: String = {
        "<" + getName + toXML_( getAttribs ) + ">"
        + toXML_( getChildren )
        + "</" + getName +">"
    }

    def toXML_( elems:Seq[Element] ):String = elems match {
        case head :: tail  => head.toXML + toXML_( tail );
        case Nil           => "";
    }

    def toXML_( attrib:Map[ String, String ] ):String = {
	def iterate( keys:Iterator[String] ) =
	    if( keys.hasNext )
		{
                        val key = keys.next;
			" " + key + "=\"" + attrib.get( key ) + "\" ";
		}
	    else
		{
			""
		}

	if( attrib != null ) iterate( attrib.keys.elements ) else "";
    }

    override def toString() = getName.concat("(").concat(getChildren.toString().concat(")"));

  /*
                def toXML : String = {
                        val attribs   = getAttribs;
                        var attribStr = "";
                        if( attribs != null )
                                attribStr = scala.xml.util.toXML( attribs )
                        else
                                () ;
                        "<" + getName
                            + attribStr
                            + ">"
                            + scala.xml.util.toXML( getChildren )
                            + "</" + getName + ">" ;
                } // def toXML
*/
} // abstract class
