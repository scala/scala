package scala.xml.parsing;

import scala.collection.immutable.Map ;
import scala.collection.mutable ;

/** */
class ConstructingHandler extends MarkupHandler[Node] {

  //def attributeCDataValue(pos: int, str:String) = CDataValue(str);

  //def attributeNamespaceDecl(pos: int, uri: String) = NamespaceDecl(uri);

  def element(pos: int, uri: String, label: String, attrMap1: mutable.Map[Pair[String,String],Attribute], args: mutable.Buffer[Node]) = {

    var attrs = new Array[Attribute](attrMap1.size);
    {
      var i = 0;
      val it = attrMap1.elements;
      while( it.hasNext ) {
        val Pair(Pair(uri:String, key:String), va: Attribute) = it.next;
        attrs( i ) = va;
        /*
        va match {
          case CDataValue(str) => attrs( i ) = Attribute(uri, key, str);
        }
        */
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
    Some(Elem(uri, label, AttributeSeq.fromAttrs(attrSeq:_*), ch:_*));
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
