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
  final def NoAttributes: AttributeSeq = AttributeSeq.Empty;

  /** the empty namespace */
  val EmptyNamespace = "";

}

/**
 * Trait for representing XML using nodes of a labelled tree.
 * This trait contains an implementation of a subset of XPath for navigation.
 */
abstract class Node extends NodeSeq {

  private var internalAttrMap:Map[String, String] = null;

  /** label of this node. I.e. "foo" for &lt;foo/&gt;) */
  def label: String;

  /** the namespace of this node */
  def namespace: String;

  /** used internally. Text = -1 PI = -2 Comment = -3 CDATA = -4 EntityRef = -5    */
  def typeTag$: Int = 0;

  /** attribute map for attributes with the same namespace as this element  */
  final def attribute: Map[String,String] = {
    if( internalAttrMap == null )
      internalAttrMap = new Map[String,String] {
	val theMap = new collection.mutable.HashMap[String,String];
	theMap ++= attributes.elements
	          .filter( x => x.namespace == namespace )
	          .map( x => Pair(x.key, x.value) );

	def size =
          theMap.size;

	def elements =
          theMap.elements;

	def get(x: String) =
          theMap.get(x);
      };
    internalAttrMap
  }

  /** attribute axis - all attributes of this node, in order defined by attrib
  */
  def attributes: AttributeSeq;

  /** child axis (all children of this node) */
  def child:      Seq[Node];

  /** descendant axis (all descendants of this node) */
  def descendant: List[Node] =
    child.toList.flatMap { x => x::x.descendant } ;

  /** descendant axis (all descendants of this node) */
  def descendant_or_self: List[Node] = this :: descendant;

  /** structural equality */
  override def equals(x: Any): Boolean = x match {
    case that: Node =>
      //Console.print("(Node)");
      that.label == this.label
    && that.attributes ==  this.attributes
    && that.child.sameElements(this.child) // sameElements
    case _ => false
  }

  /** returns a hashcode */
  override def hashCode(): Int =
    Utility.hashCode(namespace, label, attribute.toList.hashCode(), child);


  /** method for NodeSeq */
  final def theSeq = this :: Nil;

  /**
   * String representation of this node
   *
   * @param stripComment if true, strips comment nodes from result
   */
  def toString(stripComment: Boolean): String  =
    Utility.toXML(this, stripComment);

  /**
   * @see "toString(Boolean)"
   */
  override def toString(): String =
    toString(false);

}
