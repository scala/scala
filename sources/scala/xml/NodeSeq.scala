/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml ;

/* a wrapper that adds some XPath navigation method */
class NodeSeq( theSeq:Seq[Node] ) extends Seq[Node] {
  def length = theSeq.length;
  def elements = theSeq.elements ;
  def apply( i:int ) = theSeq.apply( i );

  /** projection function. Similar to XPath, use this./'foo to get a list
   *  of all elements of this sequence that are labelled with "foo".
   *  Use /'_ as a wildcard. The document order is preserved.
   */
  def \(that: Symbol):NodeSeq = {
    that.name match {
      case "_" =>
        val it = elements.flatMap { x:Node => x.child.elements };
        new NodeSeq( it.toList );
    case _   =>
        val list = elements.flatMap( y =>
          y.child.elements.filter{ z:Node => z.label == that.name }).toList;
      new NodeSeq( list );
    }
  }

  def \\( that:Symbol ):NodeSeq = {
    val it = elements.flatMap { x:Node => x.child.elements };
    val list = (elements.flatMap { x:Node => x.descendant_or_self.elements }).toList;
    that.name match {
      case "_" => new NodeSeq( list );
      case _ => new NodeSeq( list.filter { z:Node => z.label == that.name });
    }
  }

}
