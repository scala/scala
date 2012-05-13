/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml

import collection.{ mutable, immutable, generic, SeqLike, AbstractSeq }
import mutable.{ Builder, ListBuffer }
import generic.{ CanBuildFrom }
import language.implicitConversions

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
  implicit def canBuildFrom: CanBuildFrom[Coll, Node, NodeSeq] =
    new CanBuildFrom[Coll, Node, NodeSeq] {
      def apply(from: Coll) = newBuilder
      def apply() = newBuilder
    }
  def newBuilder: Builder[Node, NodeSeq] = new ListBuffer[Node] mapResult fromSeq
  implicit def seqToNodeSeq(s: Seq[Node]): NodeSeq = fromSeq(s)
}

/** This class implements a wrapper around `Seq[Node]` that adds XPath
 *  and comprehension methods.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
abstract class NodeSeq extends AbstractSeq[Node] with immutable.Seq[Node] with SeqLike[Node, NodeSeq] with Equality {
  import NodeSeq.seqToNodeSeq // import view magic for NodeSeq wrappers

  /** Creates a list buffer as builder for this class */
  override protected[this] def newBuilder = NodeSeq.newBuilder

  def theSeq: Seq[Node]
  def length = theSeq.length
  override def iterator = theSeq.iterator

  def apply(i: Int): Node = theSeq(i)
  def apply(f: Node => Boolean): NodeSeq = filter(f)

  def xml_sameElements[A](that: Iterable[A]): Boolean = {
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      if (these.next xml_!= those.next)
        return false

    !these.hasNext && !those.hasNext
  }

  protected def basisForHashCode: Seq[Any] = theSeq

  override def canEqual(other: Any) = other match {
    case _: NodeSeq   => true
    case _            => false
  }

  override def strict_==(other: Equality) = other match {
    case x: NodeSeq => (length == x.length) && (theSeq sameElements x.theSeq)
    case _          => false
  }

  /** Projection function, which returns  elements of `this` sequence based
   *  on the string `that`. Use:
   *   - `this \ "foo"` to get a list of all elements that are labelled with `"foo"`;
   *   - `\ "_"` to get a list of all elements (wildcard);
   *   - `ns \ "@foo"` to get the unprefixed attribute `"foo"`;
   *   - `ns \ "@{uri}foo"` to get the prefixed attribute `"pre:foo"` whose
   *     prefix `"pre"` is resolved to the namespace `"uri"`.
   *
   *  For attribute projections, the resulting [[scala.xml.NodeSeq]] attribute
   *  values are wrapped in a [[scala.xml.Group]].
   *
   *  There is no support for searching a prefixed attribute by its literal prefix.
   *
   *  The document order is preserved.
   */
  def \(that: String): NodeSeq = {
    def fail = throw new IllegalArgumentException(that)
    def atResult = {
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
        else y.attribute(that drop 1)

      attr match {
        case Some(x)  => Group(x)
        case _        => NodeSeq.Empty
      }
    }

    def makeSeq(cond: (Node) => Boolean) =
      NodeSeq fromSeq (this flatMap (_.child) filter cond)

    that match {
      case ""                                         => fail
      case "_"                                        => makeSeq(!_.isAtom)
      case _ if (that(0) == '@' && this.length == 1)  => atResult
      case _                                          => makeSeq(_.label == that)
    }
  }

  /** Projection function, which returns elements of `this` sequence and of
   *  all its subsequences, based on the string `that`. Use:
   *   - `this \\ 'foo` to get a list of all elements that are labelled with `"foo"`;
   *   - `\\ "_"` to get a list of all elements (wildcard);
   *   - `ns \\ "@foo"` to get the unprefixed attribute `"foo"`;
   *   - `ns \\ "@{uri}foo"` to get each prefixed attribute `"pre:foo"` whose
   *     prefix `"pre"` is resolved to the namespace `"uri"`.
   *
   *  For attribute projections, the resulting [[scala.xml.NodeSeq]] attribute
   *  values are wrapped in a [[scala.xml.Group]].
   *
   *  There is no support for searching a prefixed attribute by its literal prefix.
   *
   *  The document order is preserved.
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

  def text: String = (this map (_.text)).mkString
}
