/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml;

import scala.collection.immutable ;
/** an XML node for text (PCDATA). Used in both non-bound and bound XML
 *  representations
 * @author Burak Emir
 * @param text the text contained in this node
**/

case class EntityRef( entityName:String ) extends SpecialNode {

  final override def typeTag$:Int = -5;

  /** the constant "#ENTITY"
  */
  def label    = "#ENTITY";

  final override def equals(x:Any) = x match {
    case EntityRef( s ) => entityName.equals( s );
    case _ => false;
  }

  override def hashCode() = entityName.hashCode();

  /** returns text, with some characters escaped according to XML spec */
  final override def toString():String = "&"+entityName+";";

}
