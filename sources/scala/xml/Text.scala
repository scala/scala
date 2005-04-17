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
 * @param text the text contained in this node, may not be null.
**/


case class Text[+A]( data: A ) extends SpecialNode {

  /** @deprecated
   */
  def text = toString();

  if(null == data)
    throw new java.lang.NullPointerException("tried to construct Text with null");

  final override def typeTag$:Int = -1;

  /** the constant "#PCDATA"
  */
  def label    = "#PCDATA";

  final override def equals(x:Any) = x match {
    case s:String  => s.equals( data.toString() );
    case s:Text[A] => data == s.data ;
    case _ => false;
  }

  /** hashcode for this Text */
  override def hashCode() = data.hashCode();

  /** returns text, with some characters escaped according to XML spec */
  override def toString() = Utility.escape( data.toString() );

}
