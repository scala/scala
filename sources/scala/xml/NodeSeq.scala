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
  def \ (that: String):NodeSeq = {
    that match {
      case "_" =>
        val it = elements.flatMap { x:Node => x.child.elements };
        new NodeSeq( it.toList );
    case _   =>
        val list = elements.flatMap( y =>
          y.child.elements.filter{ z:Node => z.label == that }).toList;
      new NodeSeq( list );
    }
  }

  def \\ ( that:String ):NodeSeq = {
    val list = (elements.flatMap { x:Node => x.descendant_or_self.elements }).toList;
    that match {
      case "_" => new NodeSeq( list );
      case _ => new NodeSeq( list.filter { z:Node => z.label == that });
    }
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

}
