/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.xml
package parsing

import scala.collection.mutable
import scala.io.Source
import scala.util.logging.Logged
import scala.xml.dtd._

/** class that handles markup - provides callback methods to MarkupParser.
 *  the default is nonvalidating behaviour
 *
 *  @author  Burak Emir
 *  @version 1.0
 *
 *  @todo can we ignore more entity declarations (i.e. those with extIDs)?
 *  @todo expanding entity references
 */
abstract class MarkupHandler extends Logged
{
  /** returns true is this markup handler is validating */
  val isValidating: Boolean = false

  var decls: List[Decl] = Nil
  var ent: mutable.Map[String, EntityDecl] = new mutable.HashMap[String, EntityDecl]()

  def lookupElemDecl(Label: String): ElemDecl = {
    for (z @ ElemDecl(Label, _) <- decls)
      return z

    null
  }

  def replacementText(entityName: String): Source =
    Source fromString ((ent get entityName) match {
      case Some(ParsedEntityDecl(_, IntDef(value)))     => value
      case Some(ParameterEntityDecl(_, IntDef(value)))  => " %s " format value
      case Some(_)                                      => "<!-- %s; -->" format entityName
      case None                                         => "<!-- unknown entity %s; -->" format entityName
    })

  def endDTD(n: String): Unit = ()

  /** callback method invoked by MarkupParser after start-tag of element.
   *
   *  @param pos      the position in the sourcefile
   *  @param pre      the prefix
   *  @param label    the local name
   *  @param attrs    the attributes (metadata)
   */
  def elemStart(pos: Int, pre: String, label: String, attrs: MetaData, scope: NamespaceBinding): Unit = ()

  /** callback method invoked by MarkupParser after end-tag of element.
   *
   *  @param pos      the position in the source file
   *  @param pre      the prefix
   *  @param label    the local name
   */
  def elemEnd(pos: Int, pre: String, label: String): Unit = ()

  /** callback method invoked by MarkupParser after parsing an element,
   *  between the elemStart and elemEnd callbacks
   *
   *  @param pos      the position in the source file
   *  @param pre      the prefix
   *  @param label    the local name
   *  @param attrs    the attributes (metadata)
   *  @param empty    `true` if the element was previously empty; `false` otherwise.
   *  @param args     the children of this element
   */
  def elem(pos: Int, pre: String, label: String, attrs: MetaData, scope: NamespaceBinding, empty: Boolean, args: NodeSeq): NodeSeq

  /** callback method invoked by MarkupParser after parsing PI.
   */
  def procInstr(pos: Int, target: String, txt: String): NodeSeq

  /** callback method invoked by MarkupParser after parsing comment.
   */
  def comment(pos: Int, comment: String): NodeSeq

  /** callback method invoked by MarkupParser after parsing entity ref.
   *  @todo expanding entity references
   */
  def entityRef(pos: Int, n: String): NodeSeq

  /** callback method invoked by MarkupParser after parsing text.
   */
  def text(pos: Int, txt: String): NodeSeq

  // DTD handler methods

  def elemDecl(n: String, cmstr: String): Unit = ()

  def attListDecl(name: String, attList: List[AttrDecl]): Unit = ()

  private def someEntityDecl(name: String, edef: EntityDef, f: (String, EntityDef) => EntityDecl): Unit =
    edef match {
      case _: ExtDef if !isValidating =>  // ignore (cf REC-xml 4.4.1)
      case _  =>
        val y = f(name, edef)
        decls ::= y
        ent.update(name, y)
    }

  def parameterEntityDecl(name: String, edef: EntityDef): Unit =
    someEntityDecl(name, edef, ParameterEntityDecl.apply _)

  def parsedEntityDecl(name: String, edef: EntityDef): Unit =
    someEntityDecl(name, edef, ParsedEntityDecl.apply _)

  def peReference(name: String) { decls ::= PEReference(name) }
  def unparsedEntityDecl(name: String, extID: ExternalID, notat: String): Unit = ()
  def notationDecl(notat: String, extID: ExternalID): Unit = ()
  def reportSyntaxError(pos: Int, str: String): Unit
}
