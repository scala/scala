/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml;

/** an XML node for text (PCDATA). Used in both non-bound and bound XML
 *  representations
 * @author Burak Emir
 * @param text the text contained in this node
**/

case class EntityRef( entityName:String ) extends Node {
  /** the constant "#ENTITY"
  */
  def label    = "#ENTITY";

  /** always empty */
  final def attribute = Nil;

  /** always empty */
  final def child = Nil;

  override def hashCode() = entityName.hashCode();

  /** returns text, with some characters escaped according to XML spec */
  final override def toString():String = "&"+entityName+";";

}
