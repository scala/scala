package scala.xml.xsd ;

import scala.util.regexp.WordExp;
import scala.util.automata._;

object ContentModel extends WordExp  {

  type _labelT = ElemRef;
  type _regexpT = RegExp;

  object Translator extends WordBerrySethi  {
    override val lang: ContentModel.this.type = ContentModel.this;
    import lang._ ;
  }

  case class ElemRef(name: String) extends Label {
    override def toString() = name;
  }

  def fromSchema(nodes:Seq[Node]): List[RegExp] =
    nodes.foldLeft (Nil:List[RegExp]) { (list, n:Node) => fromSchema(n)::list }.reverse;

  def fromSchema(node:Node): RegExp = node.label match {
    case "sequence" => Sequ(fromSchema(node.child):_*);
    case "choice"   =>  Alt(fromSchema(node.child):_*);
    case "group"    => Sequ(fromSchema(node.child):_*);
    case "element"  =>
      val name = node.attribute("name").toString();
      Letter(ElemRef(name)); // ouch, anonymous? references?
  }
}

sealed abstract class ContentModel ;

case class ELEMENTS(r:ContentModel.RegExp) extends ContentModel ;

case class MIXED(r:ContentModel.RegExp) extends ContentModel ;

case object SimpleContent extends ContentModel ;
