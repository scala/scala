/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml;

import scala.runtime.compat.StringBuilder;

/** an XML node for a list of data items.
 * @author buraq
 * @param list a list of data items, space separated
 */
class Molecule[+A]( val list: List[A] ) extends SpecialNode {

  final override def typeTag$:Int = -1;

  /** the constant "#PCDATA"
  */
  def label = "#PCDATA";

  final override def equals(x:Any) = x match {
    case s:Molecule[A] => list == s.list ;
    case _ => false;
  }

  /** hashcode for this Text */
  override def hashCode() =
    list.hashCode();

  override def text = list.mkString(""," ","");

  /** returns text, with some characters escaped according to XML spec */
  def toString(sb:StringBuilder) =
    sb.append(list.mkString(""," ",""))

}
