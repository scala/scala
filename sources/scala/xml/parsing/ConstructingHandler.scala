package scala.xml.parsing;

/** implementation of MarkupHandler that constructs nodes */
abstract class ConstructingHandler(presWS: Boolean) extends MarkupHandler {

  val preserveWS = presWS;

  def elem(pos: int, pre: String, label: String, attrs: MetaData, pscope: NamespaceBinding, nodes: NodeSeq): NodeSeq =
    Elem(pre, label, attrs, pscope, nodes:_*);


  def procInstr(pos: Int, target: String, txt: String ) =
    ProcInstr(target, txt);

  def comment(pos: Int, txt: String ) =
    Comment( txt );

  def entityRef(pos: Int, n: String) =
    EntityRef( n );

  def text(pos: Int, txt:String) =
    Text( txt );

}
