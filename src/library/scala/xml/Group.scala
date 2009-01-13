/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml

/** A hack to group XML nodes in one node for output.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
@serializable
case class Group(val nodes: Seq[Node]) extends Node {

  override def theSeq = nodes

  /** structural equality */
  override def equals(x: Any) = x match {
    case z:Group     => (length == z.length) && sameElements(z)
    case z:Node      => (length == 1) && z == apply(0)
    case z:Seq[_]    => sameElements(z)
    case z:String    => text == z
    case _           => false
  }

  /**
   * @throws Predef.UnsupportedOperationException (always)
   */
  final def label =
    throw new UnsupportedOperationException("class Group does not support method 'label'")

  /**
   * @throws Predef.UnsupportedOperationException (always)
   */
  final override def attributes =
    throw new UnsupportedOperationException("class Group does not support method 'attributes'")

  /**
   * @throws Predef.UnsupportedOperationException (always)
   */
  final override def namespace =
    throw new UnsupportedOperationException("class Group does not support method 'namespace'")

  /**
   * @throws Predef.UnsupportedOperationException (always)
   */
  final override def child =
    throw new UnsupportedOperationException("class Group does not support method 'child'")

  /**
   * @throws Predef.UnsupportedOperationException (always)
   */
  def toString(sb: StringBuilder) =
    throw new UnsupportedOperationException(
      "class Group does not support method toString(StringBuilder)")

  override def text = { // same impl as NodeSeq
    val sb = new StringBuilder()
    val it = elements
    while (it.hasNext)
      sb.append(it.next.text)
    sb.toString()
  }
}
