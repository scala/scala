package scala.xml.dtd;

/** a document type declaration */
abstract class DTD {

  var externalID: ExternalID = null;

  def notations: Seq[NotationDecl] = Nil;

  def unparsedEntities: Seq[EntityDecl] = Nil;

  var decls: List[MarkupDecl] = Nil;

  //def getElemDecl(elem:String): ElemDecl;

  //def getAttribDecl(elem: String, attr: String): AttrDecl;

  override def toString() = {
    val s = super.toString();
    "[DTD "+s.substring(s.indexOf('@'), s.length())+"]";
  }

}
