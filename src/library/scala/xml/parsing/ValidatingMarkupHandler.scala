/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.xml
package parsing

import scala.xml.dtd._
import scala.util.logging.Logged

abstract class ValidatingMarkupHandler extends MarkupHandler with Logged {

  var rootLabel:String = _
  var qStack: List[Int] = Nil
  var qCurrent: Int = -1

  var declStack: List[ElemDecl] = Nil
  var declCurrent: ElemDecl = null

  final override val isValidating = true

  override def log(msg: String) {}

  /*
  override def checkChildren(pos: Int, pre: String, label:String,ns:NodeSeq): Unit = {
    Console.println("checkChildren()");
    val decl = lookupElemDecl(label);
    // @todo: nice error message
    val res = decl.contentModel.validate(ns);
    Console.println("res = "+res);
    if(!res)
      //sys.error("invalid!");
  }
  */

  override def endDTD(n:String) = {
    rootLabel = n
  }
  override def elemStart(pos: Int, pre: String, label: String, attrs: MetaData, scope:NamespaceBinding) {

    def advanceDFA(dm:DFAContentModel) = {
      val trans = dm.dfa.delta(qCurrent)
      log("advanceDFA(dm): " + dm)
      log("advanceDFA(trans): " + trans)
      trans.get(ContentModel.ElemName(label)) match {
          case Some(qNew) => qCurrent = qNew
          case _          => reportValidationError(pos, "DTD says, wrong element, expected one of "+trans.keys);
        }
    }
    // advance in current automaton
    log("[qCurrent = "+qCurrent+" visiting "+label+"]")

    if (qCurrent == -1) { // root
      log("  checking root")
      if (label != rootLabel)
        reportValidationError(pos, "this element should be "+rootLabel)
    } else {
      log("  checking node")
      declCurrent.contentModel match {
        case ANY =>
        case EMPTY =>
          reportValidationError(pos, "DTD says, no elems, no text allowed here")
        case PCDATA =>
          reportValidationError(pos, "DTD says, no elements allowed here")
        case m @ MIXED(r) =>
          advanceDFA(m)
        case e @ ELEMENTS(r) =>
          advanceDFA(e)
      }
    }
    // push state, decl
    qStack    =    qCurrent :: qStack
    declStack = declCurrent :: declStack

    declCurrent = lookupElemDecl(label)
    qCurrent = 0
    log("  done  now")
  }

  override def elemEnd(pos: Int, pre: String, label: String) {
    log("  elemEnd")
    qCurrent = qStack.head
    qStack   = qStack.tail
    declCurrent = declStack.head
    declStack   = declStack.tail
    log("    qCurrent now" + qCurrent)
    log("    declCurrent now" + declCurrent)
  }

  final override def elemDecl(name: String, cmstr: String) {
    decls = ElemDecl(name, ContentModel.parse(cmstr)) :: decls
  }

  final override def attListDecl(name: String, attList: List[AttrDecl]) {
    decls = AttListDecl(name, attList) :: decls
  }

  final override def unparsedEntityDecl(name: String, extID: ExternalID, notat: String) {
    decls = UnparsedEntityDecl(name, extID, notat) :: decls
  }

  final override def notationDecl(notat: String, extID: ExternalID) {
    decls = NotationDecl(notat, extID) :: decls;
  }

  final override def peReference(name: String) {
    decls = PEReference(name) :: decls
  }

  /** report a syntax error */
  def reportValidationError(pos: Int, str: String): Unit

}
