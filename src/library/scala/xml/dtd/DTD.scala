/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml.dtd

import scala.collection.mutable.{HashMap, Map}

/** A document type declaration.
 *
 *  @author Burak Emir
 */
abstract class DTD {

  var externalID: ExternalID = null

  def notations: Seq[NotationDecl] = Nil

  def unparsedEntities: Seq[EntityDecl] = Nil

  var elem: Map[String, ElemDecl]    = new HashMap[String, ElemDecl]()

  var attr: Map[String, AttListDecl] = new HashMap[String, AttListDecl]()

  var ent:  Map[String, EntityDecl]  = new HashMap[String, EntityDecl]()

  var decls: List[Decl] = Nil

  //def getElemDecl(elem:String): ElemDecl

  //def getAttribDecl(elem: String, attr: String): AttrDecl

  override def toString() = {
    val sb = new StringBuilder("DTD [\n")
    if (null != externalID)
      sb.append(externalID.toString()).append('\n')
    for (d <- decls)
      sb.append(d.toString()).append('\n')
    sb.append("]").toString()
  }

  /*
  def initializeEntities() = {
    for (x <- decls) x match {
      case y @ ParsedEntityDecl(name, _)      => ent.update(name, y);
      case y @ UnparsedEntityDecl(name, _, _) => ent.update(name, y);
      case y @ ParameterEntityDecl(name, _)   => ent.update(name, y);
      case _ =>
    }
  }
  */

}
