/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml

import collection.immutable
import collection.generic._
import collection.mutable.ListBuffer

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
  type Coll = NodeSeq
  implicit def builderFactory: BuilderFactory[Node, NodeSeq, Coll] = new BuilderFactory[Node, NodeSeq, Coll] { def apply(from: Coll) = newBuilder }
  def newBuilder: Builder[Node, NodeSeq, Any] = new ListBuffer[Node] mapResult fromSeq
  implicit def seqToNodeSeq(s: Seq[Node]): NodeSeq = fromSeq(s)
}

/** This class implements a wrapper around <code>Seq[Node]</code> that
 *  adds XPath and comprehension methods.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
abstract class NodeSeq extends immutable.Sequence[Node] with SequenceTemplate[Node, NodeSeq] {
  import NodeSeq.seqToNodeSeq // import view magic for NodeSeq wrappers

  /** Creates a list buffer as builder for this class */
  override protected[this] def newBuilder = NodeSeq.newBuilder

  def theSeq: Seq[Node]
  def length = theSeq.length
  override def elements = theSeq.elements
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
   *  <code>"foo"</code>. Use <code>\ "_"</code> as a wildcard. Use
   *  <code>ns \ "@foo"</code> to get the unprefixed attribute "foo".
   *  Use <code>ns \ "@{uri}foo"</code> to get the prefixed attribute
   *  "pre:foo" whose prefix "pre" is resolved to the namespace "uri".
   *  For attribute projections, the resulting NodeSeq attribute values are
   *  wrapped in a Group.
   *  There is no support for searching a prefixed attribute by
   *  its literal prefix.
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
      if (that.length() == 1)
        throw new IllegalArgumentException(that)
      if (that.charAt(1) == '{') {
        val i = that.indexOf('}')
        if (i == -1)
          throw new IllegalArgumentException(that)
        val (uri, key) = (that.substring(2,i), that.substring(i+1, that.length()))
        if (uri == "" || key == "")
          throw new IllegalArgumentException(that)
        val y = this(0)
        y.attribute(uri, key) match {
          case Some(x) => Group(x)
          case _       => NodeSeq.Empty
        }
      } else {
        val k = that.substring(1)
        val y = this(0)
        y.attribute(k) match {
          case Some(x) => Group(x)
          case _       => NodeSeq.Empty
        }
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
   *  <code>"foo"</code>. Use <code>\\ "_"</code> as a wildcard.  Use
   *  <code>ns \\ "@foo"</code> to get the unprefixed attribute "foo".
   *  Use <code>ns \\ "@{uri}foo"</code> to get each prefixed attribute
   *  "pre:foo" whose prefix "pre" is resolved to the namespace "uri".
   *  For attribute projections, the resulting NodeSeq attribute values are
   *  wrapped in a Group.
   *  There is no support for searching a prefixed attribute by
   *  its literal prefix.
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
    val sb = new StringBuilder()
    val it = elements
    while (it.hasNext) {
      sb.append(it.next.text)
    }
    sb.toString()
  }
}
