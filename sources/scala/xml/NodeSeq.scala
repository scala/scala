/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml ;

object NodeSeq {
  final val Empty = new NodeSeq { def theSeq = Nil; }
}

/** a wrapper around Seq[Node] that adds XPath and comprehension methods */
abstract class NodeSeq extends Seq[Node] {

  def theSeq: Seq[Node];
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
  def \(that: String):NodeSeq = that match {
    case "_" => for( val x <- this;
                     val y <- new NodeSeq { val theSeq = x.child; })
                yield y
    case _   => for( val x <- this;
                     val y <- new NodeSeq { val theSeq = x.child; };
                     y.label == that )
                yield { y }
  }

  /** projection function. Similar to XPath, use this \\ 'foo to get a list
   *  of all elements of this sequence that are labelled with "foo".
   *  Use \ "_" as a wildcard. The document order is preserved.
   */

  def \\ ( that:String ):NodeSeq = that match {
      case "_" => for( val x <- this;
                       val y <- new NodeSeq { val theSeq = x.descendant_or_self })
                  yield { y }
      case _ => for( val x <- this;
                     val y <- new NodeSeq { val theSeq =  x.descendant_or_self  };
                     y.label == that)
                  yield { y }
  }

  override def toString():String = theSeq.elements.foldLeft ("") {
    (s:String,x:Node) => s + x.toString()
  }

  private var _asList:List[Node] = null;
  def asList = {
    if (_asList == null ) _asList = elements.toList;
    _asList
  }

  def map( f:Node => Node ):NodeSeq = {
    new NodeSeq{ final def theSeq = NodeSeq.this.asList map f }
  }

  def flatMap( f:Node => NodeSeq ):NodeSeq = {
    new NodeSeq{ final def theSeq = NodeSeq.this.asList flatMap { x => f(x).asList }}
  }

  def filter( f:Node => boolean ):NodeSeq = {
    new NodeSeq{ val theSeq = NodeSeq.this.asList filter f  }
  }

}
