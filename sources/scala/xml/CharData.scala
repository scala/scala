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
 * @param text text contained in this node, may not contain &quot;]]&gt;&quot;
**/

case class CharData( text:String ) extends SpecialNode {

  final override def typeTag$:Int = -4;

  if( text.indexOf( "]]>" ) != -1 )
    throw new IllegalArgumentException(" CDATA text may not contain \"]]>\" ");

  /** the constant "#CDATA"
  */
  def label    = "#CDATA";

  /** returns  &quot;&lt;![CDATA[&quot;+text+&quot;]]&gt;&quot; */
  final override def toString() = "<![CDATA["+text+"]]>";

}
