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

  /** projection function. Similar to XPath, use this./'foo to get a list
   *  of all children of this node that are labelled with "foo".
   *  The document order is preserved.
   */
    def /(that:Symbol): NodeSeq = new NodeSeq({
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

  /** projection function. Similar to XPath, use this./#'foo to get a list
   *  of all descendants of this node that are labelled with "foo".
   *  Use /'_ as a wildcard.
   *  The document order is preserved.
   */
    def /#(that:Symbol): NodeSeq = new NodeSeq({
      var res:List[Node] = Nil;
      var tmp:List[Node] = Nil;
      for( val x <- children.elements ) {
        if ( x.label == that.label || "_" == that.label )
          tmp = x::tmp;
        tmp = tmp:::(x/#(that)).toList;
        res = res:::tmp;
        tmp = Nil
      }
      res;
    });

}

/* a wrapper that adds a filter method */
class NodeSeq(theList:List[Node]) extends Seq[Node] {
  val res = theList.flatMap ( x => List.fromIterator( x.children.elements ));

  /** projection function. Similar to XPath, use this./'foo to get a list
   *  of all elements of this sequence that are labelled with "foo".
   *  Use /'_ as a wildcard. The document order is preserved.
   */
  def /(that: Symbol) = if( "_" == that.label ) {
    new NodeSeq( res )
  } else {
    new NodeSeq( res.filter( y => y.label == that.label ))
  }


  /** projection function. Similar to XPath, use this./'foo to get a list
   *  of all children of this node that are labelled with "foo"
   *  Use /#'_ as a wildcard. The document order is preserved.
   */
    def /#(that: Symbol): NodeSeq = new NodeSeq(
      if ( "_" == that.label ) {
        theList.flatMap ( x => (x/#'_).toList )
      } else {
        theList.flatMap ( x => {
          if( x.label == that.label )
            x::(x/#(that)).toList;
          else
            (x/#(that)).toList;
        })
      });

  override def toList:List[Node] = theList;

  /* Seq methods */
  def length = theList.length;
  def elements = theList.elements ;
  def apply( i:int ) = theList.apply( i );
  /* forwarding list methods
  def isEmpty: boolean = theList.isEmpty;
  def head: Node = theList.head;
  def tail: List[Node] = theList.tail;

  override def toString():String = "Node"+theList.toString();

  override def filter(p: Node => Boolean): NodeList =
    new NodeList( theList.filter( p ) );

  override def foreach(f: Node => Unit): Unit = theList.foreach( f );

  override def flatMap[b](f: Node => List[b]): List[b] = theList.flatMap( f );

  override def :::[b >: Node](prefix: List[b]): List[b] = theList.:::( prefix );
  */

  //the  == method cannot be forwarded :-(
}
