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

  /** append a single node to this buffer, returns reference on this NodeBuffer for convenience. */
  override def +(n:Node):NodeBuffer = { super.+( n ); this }

  /** append a sequence of nodes to this buffer, returns reference on this NodeBuffer for convenience. */
  def +(ns:Seq[Node]):NodeBuffer = { super.++( ns ); this }

  /** append given string as a scala.xml.Text node to this buffer, returns reference on this NodeBuffer for convenience. */
  def +(t:String):NodeBuffer = { super.+( Text( t ) ); this }

}
