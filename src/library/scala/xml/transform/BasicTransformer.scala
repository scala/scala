/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml.transform

/** A class for XML transformations.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
abstract class BasicTransformer extends Function1[Node,Node] {

  protected case class NeedsCopy(result: Seq[Node]) extends Throwable

  /** Returns a new node buffer with the first <code>pos</code> elements
   *  from <code>ns</code>.
   *
   *  @param pos ..
   *  @param ns  ..
   *  @return    ..
   */
  protected def buffer(pos: Int, ns: Seq[Node]): NodeBuffer = {
    val nb = new NodeBuffer()
    var jt = ns.elements
    var j = 0; while (j < pos-1) {
      nb.append(jt.next)
      j += 1
    }
    nb
  }

  /** Turns a nodebuffer into a sequence, so hashcode works.
   *
   *  @param nb ..
   *  @return   ..
   */
  protected def freeze(nb: NodeBuffer): Seq[Node] = {
    val arr = new Array[Node](nb.length)
    var i = 0
    val it = nb.elements; while (it.hasNext) {
      arr(i) = it.next
      i += 1
    }
    val seq: Seq[Node] = arr
    seq
  }

  protected def single(ns: Seq[Node]) =
    1 == ns.length

  /**
   *  @param n  ...
   *  @param ns ...
   *  @return   ...
   */
  protected def unchanged(n: Node, ns: Seq[Node]) =
    single(ns) && (ns.elements.next.eq(n))

  /** Call transform(Node) for each node in ns, append results
   *  to NodeBuffer.
   */
  def transform(it: Iterator[Node], nb: NodeBuffer): Seq[Node] = {
    while (it.hasNext)
      nb ++= transform(it.next);
    freeze(nb)
  }

  /** Call transform(Node) to each node in ns, yield ns if nothing changes,
   *  otherwise a new sequence of concatenated results.
   */
  def transform(ns: Seq[Node]): Seq[Node] = {
    var i = 0
    val it = ns.elements
    try {
      while (it.hasNext) {
        val n = it.next
        val n2 = transform(n)
        if (!unchanged(n, n2)) {
          throw NeedsCopy(n2)
        }
        i += 1
      }
      ns
    } catch {
      case NeedsCopy(n2) =>
        val nb = buffer(i, ns)
        nb ++= (n2:Iterable[xml.Node])
        transform(it, nb)
    }
  }

  def transform(n: Node): Seq[Node] = {
    if (n.typeTag$ < 0)
      n
    else {
      val ch = n.child
      val nch = transform(ch)
      if (ch.eq(nch))
        n
      else
        Elem(n.prefix, n.label, n.attributes, n.scope, nch:_*)
    }
  }

  def apply(n: Node): Node = {
    val seq = transform(n)
    if (!single(seq))
      throw new UnsupportedOperationException("transform must return single node for root");
    else seq.elements.next
  }
}

/*
class IdentityTransformer extends BasicTransformer {
  override def transform(n: Node): Seq[Node] = n.match {
    case <world/> => <xml-world/>
    case _ => super.transform(n);
  }
}

object Foo with Application {

  val tr = new IdentityTransformer;
  val n = tr( <hello><world/></hello> );
  Console.println(n);

  val tr2 = new RewriteRule {
    final override val name = "A rule";
    override def transform(n: Node) = n.match {
      case <a/> => <b/><c/>
      case _    => n
    }
  }
  val tr3 = new RewriteRule {
    final override val name = "E rule";
    override def transform(n: Node) = n.match {
      case <e/> => <f><f/></f>
      case _    => n
    }
  }
  val tr4 = new RuleTransformer(tr2, tr3);
  val m = tr4( <hello><a/><e/></hello> );
  Console.println(m);
}
*/
