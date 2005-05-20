package scala.xml.dtd;

import scala.io.Source;
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

  var ent: Map[String, EntityDecl] =
    new HashMap[String, EntityDecl]();

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

  def initializeEntities() = {
    for(val x <- decls) x match {
      case y @ ParsedEntityDecl(name, _) => ent.update(name, y);
      case y @ UnparsedEntityDecl(name, _, _) => ent.update(name, y);
      case y @ ParameterEntityDecl(name, _) => ent.update(name, y);
      case _ =>
    }
  }

  def replacementText( entityName: String ): Source = {
    ent.get(entityName) match {
      case Some(ParsedEntityDecl(_, IntDef(value))) =>
        Source.fromString(value);
      case Some(_) =>
        Source.fromString("<!-- "+entityName+"; -->");
      case None =>
        Source.fromString("<!-- unknown entity "+entityName+"; -->")
    }
  }

}
