/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml ;

import scala.collection.mutable.AppendBuffer ;

/** Trait for representing XML using nodes of a labelled tree.
 *  This trait contains an implementation of a subset of XPath for navigation.
 */
trait Node {

  /** QName (the label of this node). I.e. "foo" for &lt;foo/&gt;) */
  def label: String;

  /** attribute axis */
  def attribute: Seq[ Pair[String,String] ];

  final def apply(key: String): Option[String] = {
    val it = attribute.elements.filter { x => key == x._1  };
    if( it.hasNext ) Some( it.next._2 ) else None
  }

  /** child axis (all children of this node) */
  def child: Seq[Node];

  /** descendant axis (all descendants of this node) */
  def descendant:Seq[Node] = child.toList.flatMap {
    x => x::x.descendant.asInstanceOf[List[Node]]
  } ;

  /** descendant axis (all descendants of this node) */
  def descendant_or_self:Seq[Node] = this::child.toList.flatMap {
    x => x::x.descendant.asInstanceOf[List[Node]]
  } ;

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
   */
    def \(that:Symbol): NodeSeq = {
      new NodeSeq({
        that.name match {

          case "_" => child.toList;
          case _ =>
            var res:List[Node] = Nil;
            for( val x <- child.elements; x.label == that.name ) {
              res = x::res;
            }
            res.reverse
        }
      });
    }

 /** projection function. Similar to XPath, use this \\ 'foo to filter
   *  all nodes labelled with "foo" from the descendant_or_self axis.
   *  The document order is preserved.
   */
  def \\(that:Symbol): NodeSeq = {
    new NodeSeq(
      that.name match {
        case "_" => this.descendant_or_self;
        case _ => this.descendant_or_self.asInstanceOf[List[Node]].
        filter( x => x.label == that.name );
      })
  }

  override def hashCode() = Utility.hashCode(label, attribute.toList.hashCode(), child);
  /** string representation of this node */
  override def toString() = Utility.toXML(this);

}
