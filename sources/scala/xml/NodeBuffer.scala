/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml ;

/** this class acts as a Buffer for nodes. If it is used as a sequence
 *  of nodes Seq[Node], it must be ensured that no updates occur after
 *  that point, because scala.xml.Node is assumed to be immutable.
 */
class NodeBuffer extends scala.collection.mutable.ArrayBuffer[Node] {

  override def +(n:Node):NodeBuffer = { super.+( n ); this }
  def +(ns:Seq[Node]):NodeBuffer = { super.++( ns ); this }

}
