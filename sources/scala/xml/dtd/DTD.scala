package scala.xml.dtd;

import scala.collection.mutable.{ HashMap, Map }

/** a document type declaration */
abstract class DTD {

  var externalID: ExternalID = null;

  def notations: Seq[NotationDecl] = Nil;

  def unparsedEntities: Seq[EntityDecl] = Nil;

  var elem: Map[String, ElemDecl] =
    new HashMap[String, ElemDecl]();

  var attr: Map[String, AttListDecl] =
    new HashMap[String, AttListDecl]();


  var decls: List[Decl] = Nil;

  //def getElemDecl(elem:String): ElemDecl;

  //def getAttribDecl(elem: String, attr: String): AttrDecl;

  override def toString() = {
    val sb = new StringBuffer();
    sb.append("DTD [\n");
    if(null != externalID)
      sb.append(externalID.toString()).append('\n');
    for(val d <- decls)
      sb.append(d.toString()).append('\n');
    sb.append("]").toString()
  }

  /** creates fresh type symbols from declarations */
  def createTypeSymbols(): Unit = {
    elem.clear;
    /*
    for(val d <- decl)
      d.match {
        case ElemDecl(name, contentModel) =>
          elementType.update(name, new ElementType(name, contentModel)
      }
      */
  }
}
