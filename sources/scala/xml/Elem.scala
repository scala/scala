package scala.xml ;

import scala.collection.mutable.HashMap ;

case class Elem( label:String, child:Node* ) extends Node {

  def similar( x:Any ) = {
    x match {
      case that:Node => (label == that.label) && child.equals( that.child )
      // sameElements
      case _ => false;
    }
  }

  private val hmap = new HashMap[String,String]();

  /** the attributes axis - default is Nil
  */
  def attribute: Seq[Pair[String, String]] = hmap.elements.toSeq( hmap.size );

  /** returns a new element with updated attributes
  */
  final def %(attrs: Seq[Pair[String, String]]):Elem = {
    val newmap = new HashMap[String,String]();
    for( val p <- hmap.elements ) { newmap += p._1 -> p._2 };
    for( val p <- attrs )         { newmap += p._1 -> p._2 };
    new Elem( label, child:_* ) {
      private val hmap = newmap;
      override def attribute = newmap.elements.toSeq( newmap.size );
    };
  }
  /** returns a new symbol with updated attribute
  */
  final def %(attr: Pair[String, String]):Elem = {
    val newmap = new HashMap[String,String]();
    for( val p <- hmap.elements ) { newmap += p._1 -> p._2 };
    newmap += attr._1 -> attr._2;
    new Elem( label, child:_* ) {
      private val hmap = newmap;
      override def attribute = newmap.elements.toSeq( newmap.size );
    };
  }
}
