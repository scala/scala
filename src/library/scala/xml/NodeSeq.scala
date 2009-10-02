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
import collection.SeqLike
import collection.mutable.{Builder, ListBuffer}
import collection.generic.BuilderFactory

/** This object ...
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
object NodeSeq {
  final val Empty = fromSeq(Nil)
  def fromSeq(s: Seq[Node]): NodeSeq = new NodeSeq {
    def theSeq = s
  }
  type Coll = NodeSeq
  implicit def builderFactory: BuilderFactory[Node, NodeSeq, Coll] = new BuilderFactory[Node, NodeSeq, Coll] { def apply(from: Coll) = newBuilder }
  def newBuilder: Builder[Node, NodeSeq] = new ListBuffer[Node] mapResult fromSeq
  implicit def seqToNodeSeq(s: Seq[Node]): NodeSeq = fromSeq(s)
}

/** This class implements a wrapper around <code>Seq[Node]</code> that
 *  adds XPath and comprehension methods.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
abstract class NodeSeq extends immutable.Seq[Node] with SeqLike[Node, NodeSeq] {
  import NodeSeq.seqToNodeSeq // import view magic for NodeSeq wrappers

  /** Creates a list buffer as builder for this class */
  override protected[this] def newBuilder = NodeSeq.newBuilder

  def theSeq: Seq[Node]
  def length = theSeq.length
  override def iterator = theSeq.iterator

  def apply(i: Int): Node = theSeq(i)
  def apply(f: Node => Boolean): NodeSeq = filter(f)

  /** structural equality */
  override def equals(x: Any): Boolean = x match {
    case z:Node      => (length == 1) && z == apply(0)
    case z:Seq[_]    => sameElements(z)
    case z:String    => text == z
    case _           => false
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
  def \(that: String): NodeSeq = {
    def atResult = {
      def fail = throw new IllegalArgumentException(that)
      lazy val y = this(0)
      val attr =
        if (that.length == 1) fail
        else if (that(1) == '{') {
          val i = that indexOf '}'
          if (i == -1) fail
          val (uri, key) = (that.substring(2,i), that.substring(i+1, that.length()))
          if (uri == "" || key == "") fail
          else y.attribute(uri, key)
        }
        else y.attribute(that.substring(1))

      attr match {
        case Some(x)  => Group(x)
        case _        => NodeSeq.Empty
      }
    }

    def makeSeq(cond: (Node) => Boolean) =
      NodeSeq fromSeq (this flatMap (_.child) filter cond)

    that match {
      case "_"                                        => makeSeq(!_.isAtom)
      case _ if (that(0) == '@' && this.length == 1)  => atResult
      case _                                          => makeSeq(_.label == that)
    }
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
  def \\ (that: String): NodeSeq = {
    def filt(cond: (Node) => Boolean) = this flatMap (_.descendant_or_self) filter cond
    that match {
      case "_"                  => filt(!_.isAtom)
      case _ if that(0) == '@'  => filt(!_.isAtom) flatMap (_ \ that)
      case _                    => filt(x => !x.isAtom && x.label == that)
    }
  }

  override def toString(): String = theSeq.mkString
  def text: String                = this map (_.text) mkString
}
