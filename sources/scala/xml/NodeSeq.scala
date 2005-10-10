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
  def fromSeq(s:Seq[Node]):NodeSeq = new NodeSeq {
    def theSeq = s;
  }
  def view(s:Seq[Node]):NodeSeq = fromSeq(s);
}

/** a wrapper around Seq[Node] that adds XPath and comprehension methods */
abstract class NodeSeq extends Seq[Node] {
  import NodeSeq.view; // import view magic for NodeSeq wrappers
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
                     val y <- x.child: NodeSeq)
                yield { y }
    case _ if that.charAt(0) == '@' =>
                val attrib = that.substring(1);
                (for( val x <- this;
                      val y <- x.child: NodeSeq;
                      val z <- y.attributes;
                      z.key == attrib )
                yield { Text(z.value) }): NodeSeq
    case _   => for( val x <- this;
                     val y <- x.child: NodeSeq;
                     y.label == that )
                yield { y }
  }

  /** projection function. Similar to XPath, use this \\ 'foo to get a list
   *  of all elements of this sequence that are labelled with "foo".
   *  Use \ "_" as a wildcard. The document order is preserved.
   */

  def \\ ( that:String ): NodeSeq = that match {
      case "_" => for( val x <- this;
                       val y <- x.descendant_or_self: NodeSeq )
                  yield { y }
      case _ if that.charAt(0) == '@' =>
                  val attrib = that.substring(1);
                  (for( val x <- this;
                       val y <- x.descendant_or_self: NodeSeq;
                       val z <- y.attributes;
                       z.key == attrib )
                yield { Text(z.value) }): NodeSeq
      case _ => for( val x <- this;
                     val y <- x.descendant_or_self: NodeSeq;
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

  def map(f: Node => Node): NodeSeq = { val x = asList map f; x }

  def flatMap(f:Node => NodeSeq): NodeSeq = { val y = asList flatMap { x => f(x).asList }; y }

  def filter(f:Node => Boolean): NodeSeq = { val x = asList filter f; x }

}
