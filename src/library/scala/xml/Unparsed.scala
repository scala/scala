/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Text.scala 8097 2006-07-11 17:15:02 +0200 (Tue, 11 Jul 2006) emir $


package scala.xml

import compat.StringBuilder

/** an XML node for unparsed content. It will be output verbatim, all bets
 *   are off regarding wellformedness etc.
 * @author Burak Emir
 * @param _data content in this node, may not be null.
 */
case class Unparsed(_data: String) extends Atom[String](_data) {

  if (null == data)
    throw new java.lang.NullPointerException("tried to construct Unparsed with null")

  final override def equals(x: Any) = x match {
    case s:String   => s.equals(data)
    case s:Text     => data == s.data
    case s:Unparsed => data == s.data
    case _ => false
  }

  /** returns text, with some characters escaped according to XML spec */
  override def toString(sb: StringBuilder) = sb append data

}
