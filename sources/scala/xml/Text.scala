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

case class Text( text:String ) extends Node {

  final override def typeTag$:Int = -1;

  /** the constant "#PCDATA"
  */
  def label    = "#PCDATA";

  final override def equals(x:Any) = x match {
    case s:String => text.equals( s );
    case Text( s ) => text.equals( s );
    case _ => false;
  }

  /** always empty */
  final def attribute = immutable.TreeMap.Empty[String,String];

  /** always empty */
  final def child = Nil;

  /** hashcode for this Text */
  override def hashCode() = text.hashCode();

  /** returns text, with some characters escaped according to XML spec */
  override def toString() = Utility.escape( text );

}
