package scala.xml;

import parsing.MarkupHandler;

trait NodeTraverser extends MarkupHandler {

  def traverse(n:Node): Unit = n match {
    case x:ProcInstr => procInstr(0, x.target, x.text)
    case x:Comment   => comment(0, x.text)
    case x:Text      => text(0, x.data)
    case x:EntityRef => entityRef(0, x.entityName)
    case _ =>
      elemStart(0, n.prefix, n.label, n.attributes, n.scope);
      for(val m <- n.child)
        traverse(m);
      elem(0, n.prefix, n.label, n.attributes, n.scope, NodeSeq.fromSeq(n.child));
      elemEnd(0, n.prefix, n.label);
  }
}
