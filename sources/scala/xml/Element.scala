package scala.xml ;

import scala.collection.Map;

/** superclass for specific representation of XML elements. These are created by the dtd2scala tool, together
*  with a parsing facility.
*/
abstract class Element {

  def getName:     String;                // the real element name
  def getChildren: Seq[ Element ];         // the children
  def getAttribs:  Map[ String, String ]; // disabled updates
  def setAttribs( m:Map[ String, String ] ):Unit ;

  /** see Element.hashValue
  */

  override def hashCode() = Element.hashValue( getName, getAttribs, getChildren );

  def toXML: String = {
    "<" + getName + Generic.toXML( getAttribs ) + ">"
    + toXML_( getChildren )
    + "</" + getName +">"
  }

  def toXML_( elems:Seq[Element] ):String = elems match {
    case head :: tail  => head.toXML + toXML_( tail );
    case Nil           => "";
  }

  override def toString() = getName.concat("(").concat(getChildren.toString().concat(")"));

} // abstract class Element

/** static helper methods for specific representations of XML elemens
*/
object Element {

  /** returns a hash value computed from the element name, attributes and hash values of children.
  */

  def hashValue( name:String, attribs:Map[ String, String ], children:Seq[ Element ] ) = {
    name.hashCode() + attribs.toList.hashCode() + children.hashCode()
  }

  /** returns a hash value computed from the element name, attributes and hash values of children.

  def hashValue( name:String, attribs:java.util.Map, children:Seq[ Element ] ) = {
    name.hashCode() + attribs.hashCode() + children.hashCode()
  }
  */

}
