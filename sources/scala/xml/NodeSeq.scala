/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml ;

/** a wrapper around Seq[Node] that adds XPath and comprehension methods */
class NodeSeq( theSeq:Seq[Node] ) extends Seq[Node] {

  def length = theSeq.length;
  def elements = theSeq.elements ;
  def apply( i:int ) = theSeq.apply( i );

  /** structural equality */
  override def equals( x:Any ) = x match {
    case z:Node      => ( length == 1 ) && z == apply( 0 )
    case z:Seq[Node] => sameElements( z )
    case _           => false;
  }

  /** projection function. Similar to XPath, use this \ "foo" to get a list
   *  of all elements of this sequence that are labelled with "foo".
   *  Use \ "_" as a wildcard. The document order is preserved.
   */
  def \ (that: String):NodeSeq = that match {
    case "_" => for( val x <- this;
                     val y <- new NodeSeq( x.child ) )
                yield y
    case _   => for( val x <- this;
                     val y <- new NodeSeq( x.child );
                     y.label == that )
                yield { y }
  }

  /** projection function. Similar to XPath, use this \\ 'foo to get a list
   *  of all elements of this sequence that are labelled with "foo".
   *  Use \ "_" as a wildcard. The document order is preserved.
   */

  def \\ ( that:String ):NodeSeq = that match {
      case "_" => for( val x <- this;
                       val y <- new NodeSeq( x.descendant_or_self ))
                  yield { y }
      case _ => for( val x <- this;
                     val y <- new NodeSeq( x.descendant_or_self );
                     y.label == that)
                  yield { y }
  }

  override def toString() = theSeq.elements.foldLeft ("") {
    (s:String,x:Node) => s + x.toString()
  }

  private var _asList:List[Node] = null;
  def asList = {
    if (_asList == null ) _asList = elements.toList;
    _asList
  }

  def map( f:Node => Node ):NodeSeq = {
    new NodeSeq( asList map f )
  }

  def flatMap( f:Node => NodeSeq ):NodeSeq = {
    new NodeSeq( asList flatMap { x => f(x).asList })
  }

  def filter( f:Node => boolean ):NodeSeq = {
    new NodeSeq( asList filter f )
  }

}
