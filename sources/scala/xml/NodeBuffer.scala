/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml;

/**
 * This class acts as a Buffer for nodes. If it is used as a sequence
 * of nodes <code>Seq[Node]</code>, it must be ensured that no updates
 * occur after that point, because <code>scala.xml.Node</code> is assumed
 * to be immutable.
 */
class NodeBuffer extends scala.collection.mutable.ArrayBuffer[Node] {

  /**
   * Append a single node to this buffer, returns reference on this
   * NodeBuffer for convenience.
   *
   * @param n
   */
  override def +(n: Node): NodeBuffer = { super.+(n); this }

  /**
   * Append a sequence of nodes to this buffer, returns reference on
   * this NodeBuffer for convenience.
   *
   * @param ns
   */
  def +(ns: Iterable[Node]): NodeBuffer = { super.++(ns); this }


  /**
   * Append a sequence of nodes to this buffer, returns reference on
   * this NodeBuffer for convenience.
   *
   * @param ns
   */
  def +(ns: Iterator[Node]): NodeBuffer = { ns.foreach{x => super.+(x)}; this }

  /**
   * Append given string as a <code>scala.xml.Text</code> node to this
   * buffer, returns reference on this NodeBuffer for convenience.
   *
   * @param t
   */
  def +(t :String): NodeBuffer = { super.+(Text(t)); this }

}
