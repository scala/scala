/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml ;


/** Trait for representation of XML elements. These are created by
 *  a dtd2scala binding tool
 */
trait Node {
  /** the label of this XML node */
    def label: String;
  /** the children of this XML node */
    def children: Seq[Node];
  /** the string representation of this XML node */
    def toXML: String;

    def /(that:Symbol): NodeList = new NodeList({
      val iter = children.elements;
      if( "_" == that.label ) {
        List.fromIterator( iter );
      } else {
        var res:List[Node] = Nil;
        for( val x <- iter; x.label == that.label ) {
          res = x::res;
        }
        res.reverse
      }
    });

    def /#(that:Symbol): NodeList = new NodeList({
      var res:List[Node] = Nil;
      for( val x <- children.elements ) {
        if ( x.label == that.label || "_" == that.label )  // order messed up here
          res = x::res;
        res =(x/#(that)).toList:::res;

      }
      res.reverse
    });

}

/* a wrapper that adds a filter method */
class NodeList(theList:List[Node]) extends List[Node] {
  val res = theList.flatMap ( x => List.fromIterator( x.children.elements ));

  def /(that:Symbol) = if( "_" == that.label ) {
    new NodeList( res )
  } else {
    new NodeList( res.filter( y => y.label == that.label ))
  }


  def toList:List[Node] = theList;

  // forwarding list methods

  def isEmpty: boolean = theList.isEmpty;
  def head: Node = theList.head;
  def tail: List[Node] = theList.tail;
  override def toString():String = "Node"+theList.toString();

  override def filter(p: Node => Boolean): NodeList =
    new NodeList( theList.filter( p ) );

  // == does not work
}
