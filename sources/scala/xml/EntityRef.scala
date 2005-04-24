/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml;

/** an XML node for entity references
 *
 * @author buraq
 * @param text the text contained in this node
 **/

case class EntityRef( entityName:String ) extends SpecialNode {

  final override def typeTag$:Int = -5;

  /** structural equality */
  override def equals(x: Any): Boolean = x match {
    case EntityRef(x) => x.equals(entityName);
    case _ => false
  }

  /** the constant "#ENTITY"
  */
  def label    = "#ENTITY";

  override def hashCode() = entityName.hashCode();

  /** appends "&amp; entityName;" to this stringbuffer */
  def toString(sb:StringBuffer) =
    sb.append("&").append(entityName).append(";");

}
