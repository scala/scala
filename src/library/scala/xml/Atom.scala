/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml;


/** an XML node for text (PCDATA). Used in both non-bound and bound XML
 *  representations
 * @author Burak Emir
 * @param text the text contained in this node, may not be null.
 */
[serializable]
class Atom[+A]( val data: A ) extends SpecialNode {

  final override def typeTag$:Int = -1;

  /** the constant "#PCDATA"
  */
  def label = "#PCDATA";

 override def equals(x:Any) = x match {
    case s:Atom[A] => data == s.data ;
    case _ => false;
  }

  /** hashcode for this Text */
  override def hashCode() =
    data.hashCode();

  /** returns text, with some characters escaped according to XML spec */
  def toString(sb:StringBuffer) =
    Utility.escape( data.toString(), sb );

  override def text: String = data.toString();

}
