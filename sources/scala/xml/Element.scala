package scala.xml ;

/** interface to XML elements. used by dtd2scala tool and the generated XML parser interfaces.
 *  sorry, no attributes yet (needs working HashMap).
 *  future revisions might use sequence instead of list.
 */

abstract class Element {

                def getName:     String;                // the real element name
                def getChildren: List[Element];         // the children
                def getAttribs:  java.Map[ String, String ]; // disabled

                def toXML: String = {
                        "<" + getName + " " + toXML( getAttribs ) + ">" + toXML( getChildren ) + "</" + getName +">"
		}

	        def toXML( elems:List[Element] ):String = elems match {
		    case head :: tail  => head.toString() + toXML( tail );
                    case Nil           => "";
		}

                def toXML( attrib:java.Map[ String, String ] ):String = {
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

		    iterate( attrib.keys.elements );
		}
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
