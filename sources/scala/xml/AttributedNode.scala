/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */
package scala.xml ;

import scala.collection.Map;


/** an XML node that may have attributes
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

    override def toString() = Utility.toXML(this);
}
