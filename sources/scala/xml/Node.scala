/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml;

import scala.collection.Map;
import scala.collection.immutable;

object Node {

  /** the constant empty attribute sequence */
  final def NoAttributes: MetaData = Null;

  /** the empty namespace */
  val EmptyNamespace = "";

}

/**
 * Trait for representing XML using nodes of a labelled tree.
 * This trait contains an implementation of a subset of XPath for navigation.
 */
abstract class Node extends NodeSeq {

  /** prefix of this node */
  def prefix: String = null;

  /** label of this node. I.e. "foo" for &lt;foo/&gt;) */
  def label: String;

  /** used internally. Atom/Molecule = -1 PI = -2 Comment = -3 EntityRef = -5  */
  def typeTag$: Int = 0;

  /** the namespace bindings */
  def scope: NamespaceBinding = TopScope;

  def namespace = getNamespace(prefix);

  def getNamespace(_pre: String) =
    if(scope == null) null else scope.getURI(_pre);

  /**  looks up an unprefixed attribute in attributes of this node.
   *   @param  key of queried attribute.
   *   @return value of UnprefixedAttribute with given key in attributes, if
   *            it exists, otherwise null.
   */
  final def attribute(key: String) =
    attributes.getValue(key);

  /**  looks up a prefixed attribute in attributes of this node.
   *   @param  uri namespace of queried attribute (may not be null).
   *   @param  key of queried attribute.
   *   @return value of PrefixedAttribute with given namespace and
   *           given key, otherwise null.
   */
  final def attribute(uri:String, key: String) =
    attributes.getValue(uri, this, key);

  /** attribute axis - all attributes of this node, in order defined by attrib
  */
  def attributes: MetaData =
    Null;

  /** child axis (all children of this node) */
  def child:      Seq[Node];

  /** descendant axis (all descendants of this node, not including not itself) */
  def descendant: List[Node] =
    child.toList.flatMap { x => x::x.descendant } ;

  /** descendant axis (all descendants of this node, including this node) */
  def descendant_or_self: List[Node] = this :: descendant;

  /** structural equality */
  override def equals(x: Any): Boolean = x match {
    case that: Node =>
      (that.prefix == this.prefix )
      &&(that.label == this.label )
      &&(that.attributes ==  this.attributes)
      && that.child.sameElements(this.child) // sameElements
      case _ => false
  }
  /** returns a hashcode */
  override def hashCode(): Int;
    //Utility.hashCode(namespace, label, attributes.hashCode(), child);


  /** method for NodeSeq */
  final def theSeq = this :: Nil;

  /**
   * String representation of this node
   *
   * @param stripComment if true, strips comment nodes from result
   */
  def toString(stripComment: Boolean): String  =
    Utility.toXML(this, stripComment);

  /** same as toString(false).
   * @see "toString(Boolean)"
   */
  override def toString(): String =
    toString(false);

  /** appends qualified name of this node to StringBuffer
   */
  def nameToString(sb: StringBuffer): StringBuffer  = {
    if(null != prefix) {
      sb.append(prefix);
      sb.append(':');
    }
    sb.append(label);
  }

  /** returns a type symbol (e.g. DTD, XSD), default null */
  def xmlType(): TypeSymbol = null;

  override def text: String;

}
