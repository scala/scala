/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml


/** This object ...
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
object NodeSeq {
  final val Empty = new NodeSeq { def theSeq = Nil }
  def fromSeq(s: Seq[Node]): NodeSeq = new NodeSeq {
    def theSeq = s
  }
  implicit def view(s: Seq[Node]): NodeSeq = fromSeq(s)
}

/** This class implements a wrapper around <code>Seq[Node]</code> that
 *  adds XPath and comprehension methods.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
abstract class NodeSeq extends Seq[Node] {
  import NodeSeq.view // import view magic for NodeSeq wrappers
  def theSeq: Seq[Node]
  def length = theSeq.length
  def elements = theSeq.elements
  def apply(i: Int): Node = theSeq.apply(i)

  def apply(f: Node => Boolean): NodeSeq = filter(f)

  /** structural equality */
  override def equals(x: Any) = x match {
    case z:Node      => (length == 1) && z == apply(0)
    case z:Seq[_]    => sameElements(z)
    case z:String    => text == z
    case _           => false;
  }

  /** Projection function. Similar to XPath, use <code>this \ "foo"</code>
   *  to get a list of all elements of this sequence that are labelled with
   *  <code>"foo"</code>. Use <code>\ "_"</code> as a wildcard.
   *  The document order is preserved.
   *
   *  @param that ...
   *  @return     ...
   */
  def \(that: String): NodeSeq = that match {
    case "_" =>
      var zs: List[Node] = Nil
      val it = this.elements
      while (it.hasNext) {
        val x = it.next
        val jt = x.child.elements
        while (jt.hasNext) {
          val y = jt.next
          if (y.typeTag$ != -1)
            zs = y::zs
        }
      }
      NodeSeq.fromSeq(zs.reverse)

    case _ if (that.charAt(0) == '@') && (this.length == 1) =>
      val k = that.substring(1)
      val y = this(0)
      y.attribute(k) match {
	case Some(x) => Group(x)
        case _       => NodeSeq.Empty
      }

    case _   =>
      var zs: List[Node] = Nil
      val it = this.elements
      while (it.hasNext) {
        val x = it.next
        val jt = x.child.elements
        while (jt.hasNext) {
          val y = jt.next
          if (y.label == that)
            zs = y::zs
        }
      }
      NodeSeq.fromSeq(zs.reverse)
  }

  /** projection function. Similar to XPath, use <code>this \\ 'foo</code>
   *  to get a list of all elements of this sequence that are labelled with
   *  <code>"foo"</code>. Use <code>\\ "_"</code> as a wildcard.
   *  The document order is preserved.
   *
   *  @param that ...
   *  @return     ...
   */
  def \\ (that: String): NodeSeq = that match {
    case "_" =>
      var zs: List[Node] = Nil
      val it = this.elements
      while (it.hasNext) {
        val x = it.next
        val jt = x.descendant_or_self.elements
        while (jt.hasNext) {
          val y = jt.next
          if (y.typeTag$ != -1)
            zs = y::zs
        }
      }
      zs.reverse

    case _ if that.charAt(0) == '@' =>
      var zs: List[Node] = Nil
      val it = this.elements
      while (it.hasNext) {
        val x = it.next
        val jt = x.descendant_or_self.elements
        while (jt.hasNext) {
          val y = jt.next
          if (y.typeTag$ != -1) {
            val kt = (y \ that).elements
            while (kt.hasNext) {
              zs = (kt.next)::zs
            }
          }
        }
      }
      zs.reverse

    case _ =>
      var zs: List[Node] = Nil
      val it = this.elements
      while (it.hasNext) {
        val x = it.next
        val jt = x.descendant_or_self.elements
        while (jt.hasNext) {
          val y = jt.next
          if (y.typeTag$ != -1 && y.label == that)
            zs = y::zs
        }
      }
    zs.reverse
  }

  override def toString(): String = theSeq.elements.foldLeft ("") {
    (s: String, x: Node) => s + x.toString()
  }
/*
  def map(f: Node => NodeSeq): NodeSeq = flatMap(f)

  def flatMap(f: Node => NodeSeq): NodeSeq = {
    val y = toList flatMap { x => f(x).toList }
    y
  }

  override def filter(f: Node => Boolean): NodeSeq = {
    val x = toList filter f
    x
  }
*/

  def text: String = {
    val sb = new compat.StringBuilder()
    val it = elements
    while (it.hasNext) {
      sb.append(it.next.text)
    }
    sb.toString()
  }
}
