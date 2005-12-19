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
  implicit def view(s:Seq[Node]):NodeSeq = fromSeq(s);
}

/** a wrapper around Seq[Node] that adds XPath and comprehension methods */
abstract class NodeSeq extends Seq[Node] {
  import NodeSeq.view; // import view magic for NodeSeq wrappers
  def theSeq: Seq[Node];
  def length = theSeq.length;
  def elements = theSeq.elements ;
  def apply(i: int ): Node = theSeq.apply( i );

  def apply(f: Node => Boolean): NodeSeq = filter(f);

  /** structural equality */
  override def equals(x: Any) = x match {
    case z:Node      => (length == 1) && z == apply(0)
    case z:Seq[Node] => sameElements( z )
    case z:String    => text == z
    case _           => false;
  }

  /** projection function. Similar to XPath, use this \ "foo" to get a list
   *  of all elements of this sequence that are labelled with "foo".
   *  Use \ "_" as a wildcard. The document order is preserved.
   */
  def \(that: String):NodeSeq = {
    var res: NodeSeq = NodeSeq.Empty;
    that match {
      case "_" =>
        res = for( val x <- this; val y <- x.child: NodeSeq) yield { y }

      case _ if (that.charAt(0) == '@') && (this.length == 1) =>
        val k = that.substring(1);
        val y = this(0);
        val v = y.attribute(k);
        if( v != null ) {
          res = NodeSeq.fromSeq(Seq.single(Text(v)));
        }

      case _   =>
        res = for( val x <- this; val y <- x.child: NodeSeq; y.label == that )
              yield { y }
    }
    res
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
        (for(val x <- this;
             val y <- x.descendant_or_self: NodeSeq;
             val z <- y \ that)
         yield { z }):NodeSeq
      case _ => for( val x <- this;
                     val y <- x.descendant_or_self: NodeSeq;
                     y.label == that)
                  yield { y }
  }

  override def toString():String = theSeq.elements.foldLeft ("") {
    (s:String,x:Node) => s + x.toString()
  }

  def asList = elements.toList;

  def map(f: Node => Node): NodeSeq = { val x = asList map f; x }

  def flatMap(f:Node => NodeSeq): NodeSeq = { val y = asList flatMap { x => f(x).asList }; y }

  def filter(f:Node => Boolean): NodeSeq = { val x = asList filter f; x }

  def text: String = {
    val sb = new StringBuffer();
    val it = elements;
    while(it.hasNext) {
      sb.append(it.next.text);
    }
    sb.toString();
  }
}
