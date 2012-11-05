/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml

/**
 * This class acts as a Buffer for nodes. If it is used as a sequence of
 * nodes `Seq[Node]`, it must be ensured that no updates occur after that
 * point, because `scala.xml.Node` is assumed to be immutable.
 *
 * Despite this being a sequence, don't use it as key in a hashtable.
 * Calling the hashcode function will result in a runtime error.
 *
 * @author  Burak Emir
 * @version 1.0
 */
class NodeBuffer extends scala.collection.mutable.ArrayBuffer[Node] {

  /**
   * Append given object to this buffer, returns reference on this
   * `NodeBuffer` for convenience. Some rules apply:
   * - If argument `o` is `'''null'''`, it is ignored.
   * - If it is an `Iterator` or `Iterable`, its elements will be added.
   * - If `o` is a node, it is added as it is.
   * - If it is anything else, it gets wrapped in an [[scala.xml.Atom]].
   *
   * @param o converts to an xml node and adds to this node buffer
   * @return  this nodebuffer
   */
  def &+(o: Any): NodeBuffer = {
    o match {
      case null | _: Unit | Text("")  => // ignore
      case it: Iterator[_]            => it foreach &+
      case n: Node                    => super.+=(n)
      case ns: Iterable[_]            => this &+ ns.iterator
      case ns: Array[_]               => this &+ ns.iterator
      case d                          => super.+=(new Atom(d))
    }
    this
  }
}
