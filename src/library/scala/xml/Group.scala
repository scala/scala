/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml

import scala.runtime.compat.StringBuilder

/** A hack to group XML nodes in one node for output.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
[serializable]
case class Group(val nodes: Seq[Node]) extends Node {

  override def theSeq = nodes

  /** structural equality */
  override def equals(x: Any) = x match {
    case z:Node      => (length == 1) && z == apply(0)
    case z:Seq[Node] => sameElements(z)
    case z:String    => text == z
    case _           => false;
  }

  /** always null */
  final def label =
    error("class Group does not support method 'label'")

  /** always empty */
  final override def attributes =
    error("class Group does not support method 'attributes'")

  /** always null */
  final override def namespace =
    error("class Group does not support method 'namespace'")

  /** always empty */
  final override def child =
    error("class Group does not support method 'child'")

  /** returns text, with some characters escaped according to XML spec */
  def toString(sb: StringBuilder) =
    error("class Group does not support method toString(StringBuilder)")

  override def text = { // same impl as NodeSeq
    val sb = new StringBuilder()
    val it = elements
    while (it.hasNext)
      sb.append(it.next.text)
    sb.toString()
  }
}
