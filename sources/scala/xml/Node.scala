package scala.xml ;

import scala.collection.Map;

/** superclass for specific representation of XML elements. These are created by
**  a xxx2scala binding tool
**/
abstract class Node {

  def label:    String;
  def children: Seq[ Node ];

  /** returns a mapping from all present attributes to values */
  def attributes: Map[String,String];

  protected val attribHashCode = attributes.toList.hashCode();

  /** hashcode for this node*/
  override def hashCode() = Utility.hashCode( label, attribHashCode, children );

  def toXML: String = Utility.toXML( this );

  def toXML_( elems:Seq[Node] ):String = elems match {
    case head :: tail  => head.toXML + toXML_( tail );
    case Nil           => "";
  }

  override def toString() = {
    var s = new StringBuffer( label );
    s.append("(");
    s.append( children.toString() );
    s.append(")");
    s.toString();
  }

}
