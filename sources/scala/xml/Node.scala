/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml ;

import scala.collection.Map ;
import scala.collection.immutable ;

object Node {

  /** the constant empty attribute sequence */
  final def NoAttributes: AttributeSeq = AttributeSeq.Empty;

  /** the empty namespace */
  val EmptyNamespace = "";

}
/** Trait for representing XML using nodes of a labelled tree.
 *  This trait contains an implementation of a subset of XPath for navigation.
 */
abstract class Node extends NodeSeq {

  final def theSeq = this :: Nil;
  /*
  final def length = 1;
  final def elements = new Iterator[Node] {
    var notAccessed = true;
    final def hasNext = notAccessed;
    final def next = {notAccessed = false; Node.this}
  }
  final def apply(i: Int) = if( i==0 ) this else error("singleton sequence");
  */

  /** used internally. Text = -1 PI = -2 Comment = -3 CDATA = -4 EntityRef = -5
   */
  def typeTag$:Int = 0;

  /** QName (the label of this node). I.e. "foo" for &lt;foo/&gt;) */
  def label: String;

  /** the namespace of this node */
  def namespace: String;

  private var internalAttrMap:Map[String, String] = null;

  /** attribute map for attributes with the same namespace as this element  */
  final def attribute: Map[String,String] = {
    if( internalAttrMap == null )
      internalAttrMap = new Map[String,String] {
	val theMap = new collection.mutable.HashMap[String,String];
	theMap ++=
	attributes.elements
	.filter( x => x.namespace == namespace )
	.map( x => Pair(x.key, x.value) );

	def size = theMap.size;
	def elements = theMap.elements;
	def get(x:String) = theMap.get(x);
      };
    internalAttrMap
  }

  /** attribute axis - all attributes of this node, in order defined by attrib
  */
  def attributes: AttributeSeq;

  /** child axis (all children of this node) */
  def child:      Seq[Node];

  /** descendant axis (all descendants of this node) */
  def descendant:List[Node] = child.toList.flatMap {
    x => x::x.descendant
  } ;

  /** descendant axis (all descendants of this node) */
  def descendant_or_self:List[Node] = this :: descendant;

  override def equals( x:Any ):boolean = x match {
    case that:Node =>
      //Console.print("(Node)");
      that.label == this.label &&
        that.attribute.sameElements( this.attribute ) &&
          that.child.sameElements( this.child ) // sameElements
    case _ => false
  }

 /** projection function. Similar to XPath, use this \ 'foo to get a list
   *  of all children of this node that are labelled with "foo".
   *  The document order is preserved.
    def \( that:String ): NodeSeq = that match {

      case "_" => new NodeSeq { val theSeq = child; }
      case _   =>
        var res:List[Node] = Nil;
        for( val x <- child.elements; x.label == that ) {
          res = x::res;
        }
        new NodeSeq { val theSeq = res.reverse }
    }
   */

 /** projection function. Similar to XPath, use this \\ 'foo to filter
   *  all nodes labelled with "foo" from the descendant_or_self axis.
   *  The document order is preserved.
  def \\( that:String ): NodeSeq = {
    val z = this.descendant_or_self;
    new NodeSeq(
      that match {
        case "_" => z
        case _ => z.filter( x => x.label == that );
      })
  }
   */

  override def hashCode() = Utility.hashCode(namespace, label, attribute.toList.hashCode(), child);
  /** string representation of this node */
  override def toString() = Utility.toXML(this);

}
