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

  /** the constant empty attribute map */
  val NoAttributes: immutable.TreeMap[String,String] =
    immutable.TreeMap.Empty[String,String];

}
/** Trait for representing XML using nodes of a labelled tree.
 *  This trait contains an implementation of a subset of XPath for navigation.
 */
trait Node {

  /** used internally. Text = -1 PI = -2 Comment = -3 CDATA = -4 EntityRef = -5 */
  def typeTag$:Int = 0;

  /** QName (the label of this node). I.e. "foo" for &lt;foo/&gt;) */
  def label: String;

  /** attribute axis */
  def attribute: Map[String,String] ;

  /** child axis (all children of this node) */
  def child: Seq[Node];

  /** descendant axis (all descendants of this node) */
  def descendant: List[Node] = child.toList.flatMap {
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
   */
    def \( that:String ): NodeSeq = {
      new NodeSeq({
        that match {

          case "_" => child.toList;
          case _ =>
            var res:List[Node] = Nil;
            for( val x <- child.elements; x.label == that ) {
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
  def \\( that:String ): NodeSeq = {
    val z = this.descendant_or_self;
    new NodeSeq(
      that match {
        case "_" => z
        case _ => z.filter( x => x.label == that );
      })
  }

  override def hashCode() = Utility.hashCode(label, attribute.toList.hashCode(), child);
  /** string representation of this node */
  override def toString() = Utility.toXML(this);

}
