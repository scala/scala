package scala.xml.parsing;

import scala.collection.immutable.Map ;
import scala.collection.mutable ;

/** @todo: make ConstructingMarkupHandler */
abstract class ConstructingHandler extends MarkupHandler[Node,String] {

  //def attributeCDataValue(pos: int, str:String): AttribValue[String];
  //def attributeEmbedded(pos: int, x:MarkupType): AttribValue[String];

  def element(pos: int, label: String, attrMap1: mutable.Map[String,AttribValue[String]], args: mutable.Buffer[Node]) = {

    var attrs = new Array[Attribute](attrMap1.size);
    {
      var i = 0;
      val it = attrMap1.elements;
      while( it.hasNext ) {
        val Pair(ke:String, va: AttribValue[String]) = it.next;
        attrs( i ) = Attribute("",ke,va.value);
        i = i + 1;
      }
    }
    val attrSeq: Seq[Attribute] = attrs;
    val nodes = new Array[Node]( args.length );
    {
      var i = 0;
      val it = args.elements;
      while( i < args.length ) {
        nodes(i) = it.next;
        i = i + 1;
      }
    }
    val ch: Seq[Node] = nodes;
    Elem("", label, AttributeSeq.fromAttrs(attrSeq:_*), ch:_*);
  };

  def charData(pos: Int, txt: String ) =
    CharData( txt );

  def procInstr(pos: Int, target: String, txt: String ) =
    ProcInstr(target, txt);

  def comment(pos: Int, txt: String ) =
    Comment( txt );

  def entityRef(pos: Int, n: String) =
    EntityRef( n );

  def text(pos: Int, txt:String) =
    Text( txt );

}
