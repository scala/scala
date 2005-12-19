package scala.xml.parsing;

/** default implemenation of markup handler always returns NodeSeq.Empty */
abstract class DefaultMarkupHandler extends MarkupHandler {

  def elem(pos: int, pre: String, label: String, attrs: MetaData, scope:NamespaceBinding, args: NodeSeq) = NodeSeq.Empty;

  def procInstr(pos: Int, target: String, txt: String) = NodeSeq.Empty;

  def comment(pos: Int, comment: String ): NodeSeq = NodeSeq.Empty;

  def entityRef(pos: Int, n: String) = NodeSeq.Empty;

  def text(pos: Int, txt:String) = NodeSeq.Empty;

}
