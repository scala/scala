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

/** an XML node for comments.
 *
 * @author Burak Emir
 * @param text text contained in this node, may not contain "--"
**/

case class Comment( text:String ) extends Node {

  final override def typeTag$:Int = -3;

  if( text.indexOf("--" ) != -1 )
    throw new IllegalArgumentException("text containts \"--\"");

  final override def equals(x:Any) = x match {
    case Comment( s ) => text.equals( s );
    case _ => false;
  }

  /** the constant "#REM" */
  def label    = "#REM";

  /** always empty */
  final def attribute = Node.NoAttributes;

  /** always empty */
  final def child = Nil;

  /** hashcode for this Comment */
  override def hashCode() = text.hashCode();

  /** returns "<!--"+text+"-->" */
  final override def toString() = "<!--"+text+"-->";

}
