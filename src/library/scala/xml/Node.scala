/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml

import compat.StringBuilder

/**
 * This object provides methods ...
 *
 * @author  Burak Emir
 * @version 1.0
 */
object Node {

  /** the constant empty attribute sequence */
  final def NoAttributes: MetaData = Null

  /** the empty namespace */
  val EmptyNamespace = ""

  def unapplySeq(n:Node) = Some (Tuple3(n.label, n.attributes, n.child))

}

/**
 * An abstract class representing XML with nodes of a labelled tree.
 * This class contains an implementation of a subset of XPath for navigation.
 *
 * @author  Burak Emir and others
 * @version 1.1
 */
abstract class Node extends NodeSeq {

  /** prefix of this node */
  def prefix: String = null

  /** label of this node. I.e. "foo" for &lt;foo/&gt;) */
  def label: String

  /** used internally. Atom/Molecule = -1 PI = -2 Comment = -3 EntityRef = -5
   */
  def typeTag$: Int = 0

  /**
   *  method returning the namespace bindings of this node. by default, this is TopScope,
   *  which means there are no namespace bindings except the predefined one for "xml".
   */
  def scope: NamespaceBinding = TopScope

  /**
   *  convenience, same as <code>getNamespace(this.prefix)</code>
   */
  def namespace = getNamespace(this.prefix)

  /**
   * Convenience method, same as <code>scope.getURI(pre)</code> but additionally
   * checks if scope is <code>null</code>.
   *
   * @param pre the prefix whose namespace name we would like to obtain
   * @return    the namespace if <code>scope != null</code> and prefix was
   *            found, else <code>null</code>
   */
  def getNamespace(pre: String): String = if (scope eq null) null else scope.getURI(pre)

  /**
   * Convenience method, looks up an unprefixed attribute in attributes of this node.
   * Same as <code>attributes.getValue(key)</code>
   *
   * @param  key of queried attribute.
   * @return value of <code>UnprefixedAttribute</code> with given key
   *         in attributes, if it exists, otherwise <code>null</code>.
   */
  final def attribute(key: String): Option[Seq[Node]] = attributes.get(key)

  /**
   * Convenience method, looks up a prefixed attribute in attributes of this node.
   * Same as <code>attributes.getValue(uri, this, key)</code>
   *
   * @param  uri namespace of queried attribute (may not be null).
   * @param  key of queried attribute.
   * @return value of <code>PrefixedAttribute</code> with given namespace
   *         and given key, otherwise <code>null</code>.
   */
  final def attribute(uri: String, key: String): Option[Seq[Node]] = attributes.get(uri, this, key)

  /**
   * Returns attribute meaning all attributes of this node, prefixed and unprefixed,
   * in no particular order. In class Node, this defaults to Null (the empty attribute list).
   *
   * @return all attributes of this node
   */
  def attributes: MetaData = Null

  /**
   * Returns child axis i.e. all children of this node.
   *
   * @return all children of this node
   */
  def child: Seq[Node]

  /**
   * Descendant axis (all descendants of this node, not including node itself)
   * includes all text nodes, element nodes, comments and processing instructions.
   */
  def descendant: List[Node] =
    child.toList.flatMap { x => x::x.descendant }

  /**
   * Descendant axis (all descendants of this node, including thisa node)
   * includes all text nodes, element nodes, comments and processing instructions.
   */
  def descendant_or_self: List[Node] = this :: descendant

  /**
   * Returns true if x is structurally equal to this node. Compares prefix,
   * label, attributes and children.
   *
   * @param x ...
   * @return  <code>true</code> if ..
   */
  override def equals(x: Any): Boolean = x match {
    case g:Group => false
    case that: Node =>
      ((that.prefix == this.prefix )
       &&(that.label == this.label )
       &&(that.attributes ==  this.attributes)
       && that.child.sameElements(this.child)) // sameElements
    case _ => false
  }

  /**
   *  Returns a hashcode. A standard implementation of hashcodes is obtained by calling
   *  Utility.hashCode(pre, label, attributes.hashCode(), child);
   */
  override def hashCode(): Int

  // implementations of NodeSeq methods

  /**
   *  returns a sequence consisting of only this node
   */
  def theSeq: Seq[Node] = this :: Nil

  /**
   * String representation of this node
   *
   * @param stripComment if true, strips comment nodes from result
   * @return ...
   */
  def toString(stripComment: Boolean): String =
    Utility.toXML(this, stripComment)

  /**
   * Same as <code>toString(false)</code>.
   *
   * @see <code><a href="#toString">toString(Boolean)</a></code>
   */
  override def toString(): String = toString(false)

  /**
   * Appends qualified name of this node to <code>StringBuilder</code>.
   *
   * @param sb ...
   * @return   ...
   */
  def nameToString(sb: StringBuilder): StringBuilder  = {
    if (null != prefix) {
      sb.append(prefix)
      sb.append(':')
    }
    sb.append(label);
  }

  /**
   * Returns a type symbol (e.g. DTD, XSD), default <code>null</code>.
   */
  def xmlType(): TypeSymbol = null

  /**
   * Returns a text representation of this node. Note that this is not equivalent to
   * the XPath node-test called text(), it is rather an implementation of the
   * XPath function string()
   */
  override def text: String

}
