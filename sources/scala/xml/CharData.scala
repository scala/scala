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

/** an XML node for unparsed character data (CDATA).
 * @author Burak Emir
 * @param text text contained in this node, may not contain "]]>"
**/

case class CharData( text:String ) extends Node {

  if( text.indexOf( "]]>" ) != -1 )
    throw new IllegalArgumentException(" CDATA text may not contain \"]]>\" ");

  /** the constant "#PCDATA"
  */
  def label    = "#PI";

  /** always empty */
  final def attribute = immutable.TreeMap.Empty[String,String];

  /** always empty */
  final def child = Nil;

  /** returns  "<![CDATA["+text+"]]>" */
  final override def toString() = "<![CDATA["+text+"]]>";

}
