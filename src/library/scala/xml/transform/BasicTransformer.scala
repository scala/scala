/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.xml
package transform

/** A class for XML transformations.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
abstract class BasicTransformer extends Function1[Node,Node]
{
  protected def unchanged(n: Node, ns: Seq[Node]) =
    ns.length == 1 && (ns.head == n)

  /** Call transform(Node) for each node in ns, append results
   *  to NodeBuffer.
   */
  def transform(it: Iterator[Node], nb: NodeBuffer): Seq[Node] =
    it.foldLeft(nb)(_ ++= transform(_)).toSeq

  /** Call transform(Node) to each node in ns, yield ns if nothing changes,
   *  otherwise a new sequence of concatenated results.
   */
  def transform(ns: Seq[Node]): Seq[Node] = {
    val (xs1, xs2) = ns span (n => unchanged(n, transform(n)))

    if (xs2.isEmpty) ns
    else xs1 ++ transform(xs2.head) ++ transform(xs2.tail)
  }

  def transform(n: Node): Seq[Node] = {
    if (n.doTransform) n match {
      case Group(xs)  => Group(transform(xs)) // un-group the hack Group tag
      case _          =>
        val ch = n.child
        val nch = transform(ch)

        if (ch eq nch) n
        else           Elem(n.prefix, n.label, n.attributes, n.scope, nch: _*)
    }
    else n
  }

  def apply(n: Node): Node = {
    val seq = transform(n)
    if (seq.length > 1)
      throw new UnsupportedOperationException("transform must return single node for root");
    else seq.head
  }
}
