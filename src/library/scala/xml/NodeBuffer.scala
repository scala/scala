/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml;


/**
 * This class acts as a Buffer for nodes. If it is used as a sequence
 * of nodes <code>Seq[Node]</code>, it must be ensured that no updates
 * occur after that point, because <code>scala.xml.Node</code> is assumed
 * to be immutable.
 *
 * Despite this being a sequence, don't use it as key in a hashtable.
 * Calling the hashcode function will result in a runtime error.
 */
class NodeBuffer extends scala.collection.mutable.ArrayBuffer[Node] {

  /**
   * Append a single node to this buffer, returns reference on this
   * NodeBuffer for convenience.
   *
   * Append an iterable object to this buffer, returns reference on
   * this NodeBuffer for convenience.
   *
   * Append given string as a <code>scala.xml.Text</code> node to this
   * buffer, returns reference on this NodeBuffer for convenience.
   *
   * @param n
   */
  def &+(o: Any): NodeBuffer = {
    o match {
      case n:Node     => super.+(n);
      case ns:Iterable[AnyRef] =>
        val it = ns.elements;
        while(it.hasNext) {
          this &+ it.next;
          if (it.hasNext)
            this &+ " ";
        }
      case _          => super.+(Text(o.toString()));
    }
    this
  }
  /*
  def +(o: AnyVal): NodeBuffer = {
    super.+(Text(o.toString()));
    this
  }
  */
}
