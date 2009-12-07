/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml
package dtd

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

  override def toString() =
    "DTD [\n%s%s]".format(
      Option(externalID) getOrElse "",
      decls.mkString("", "\n", "\n")
    )
}
