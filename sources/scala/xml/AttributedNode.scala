package scala.xml ;

import scala.collection.Map;


/** Superclass for specific representation of XML elements. These are created by
 *  a xxx2scala binding tool
 */
trait AttributedNode extends Node {

    /** Returns the value for the given attribute.
     */
    final def apply(key: String): Option[String] = attributes.get(key);

    /** Returns a mapping from all present attributes to values.
     */
    def attributes: Map[String, String];

    /** Returns a new AttributedNode with updated attributes.
     */
    def %(attrs: List[Pair[String, String]]): AttributedNode;

    /** Returns a new AttributedNode with updated attribute.
     */
    def %(attr: Pair[String, String]): AttributedNode;

    /** Returns the hashcode for this node.
     */
    override def hashCode() = Utility.hashCode(label, attributes.toList.hashCode(), children);

    override def toXML:String = Utility.toXML(this);

    override def toString() = toXML /*{
        var s = new StringBuffer( "AttributedNode('"+label );
        val as = attributes;
        if( as != null )
            s.append( Utility.attr2xml( as.elements ) );
        s.append("(");
        s.append( children.toString() );
        s.append(")");
        s.toString();
    }*/
}
