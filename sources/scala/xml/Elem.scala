package scala.xml ;

import scala.collection.immutable.{Map,ListMap} ;

case class Elem( name:String, children:Node* ) extends AttributedNode {

  /** Returns the symbol name as a string.
  */
  def label:String = name;

  /** Returns the list of children of this symbol.
  def children: NodeSeq = new NodeSeq( List.fromIterator(
      elems.elements map ( x => x match {
        case n:Node => n;
        case _      => Text(x.toString());
      })));
  */

  /** Returns a map representing the attributes of this node.
  */
  def attributes: Map[String, String] = ListMap.Empty;

  /** returns a new symbol with updated attributes
  */
  final def %(attrs: List[Pair[String, String]]) =
    new Elem( name, children:_* ) {
      val themap = Elem.this.attributes.incl( attrs );
      override def attributes = themap;
    };

  /** returns a new symbol with updated attribute
  */
  final def %(attr: Pair[String, String])  =
    new Elem( name, children:_* ) {
      val themap = Elem.this.attributes.incl( attr );
      override def attributes = themap;
    };

}
