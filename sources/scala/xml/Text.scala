/* BE */
package scala.xml;

import scala.collection.Map ;

/** a text node. Used in both unsafe and typed XML representation
**/

case class Text( text:String ) extends Node {

  def label    = "#PCDATA";

  def children = Nil;
  def attributes = null;

  protected val attribHashCode = 0;

  override def toXML = Utility.escape( text );

  override def hashCode() = text.hashCode(); /* could be done at parse time, too */
  override def toString() = "Text("+text+")";

} // Text

