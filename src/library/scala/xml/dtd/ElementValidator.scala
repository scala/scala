/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.xml
package dtd

import PartialFunction._
import ContentModel.ElemName
import MakeValidationException._    // @todo other exceptions
import scala.util.automata._
import scala.collection.mutable

/** validate children and/or attributes of an element
 *  exceptions are created but not thrown.
 */
class ElementValidator() extends Function1[Node,Boolean] {

  private var exc: List[ValidationException] = Nil

  protected var contentModel: ContentModel           = _
  protected var dfa:          DetWordAutom[ElemName] = _
  protected var adecls:       List[AttrDecl]         = _

  /** set content model, enabling element validation */
  def setContentModel(cm: ContentModel) = {
    contentModel = cm
    cm match {
      case ELEMENTS(r) =>
        val nfa = ContentModel.Translator.automatonFrom(r, 1)
        dfa = new SubsetConstruction(nfa).determinize
      case _ =>
        dfa = null
    }
  }

  def getContentModel = contentModel

  /** set meta data, enabling attribute validation */
  def setMetaData(adecls: List[AttrDecl]) { this.adecls = adecls }

  def getIterable(nodes: Seq[Node], skipPCDATA: Boolean): Iterable[ElemName] = {
    def isAllWhitespace(a: Atom[_]) = cond(a.data) { case s: String if s.trim == "" => true }

    nodes.filter {
      case y: SpecialNode => y match {
        case a: Atom[_] if isAllWhitespace(a) => false  // always skip all-whitespace nodes
        case _                                => !skipPCDATA
      }
      case x                                  => x.namespace eq null
    } . map (x => ElemName(x.label))
  }

  /** check attributes, return true if md corresponds to attribute declarations in adecls.
   */
  def check(md: MetaData): Boolean = {
    val len: Int = exc.length
    var ok = new mutable.BitSet(adecls.length)

    for (attr <- md) {
      def attrStr = attr.value.toString
      def find(Key: String): Option[AttrDecl] = {
        adecls.zipWithIndex find {
          case (a @ AttrDecl(Key, _, _), j) => ok += j ; return Some(a)
          case _                            => false
        }
        None
      }

      find(attr.key) match {
        case None =>
          exc ::= fromUndefinedAttribute(attr.key)

        case Some(AttrDecl(_, tpe, DEFAULT(true, fixedValue))) if attrStr != fixedValue =>
          exc ::= fromFixedAttribute(attr.key, fixedValue, attrStr)

        case _ =>
      }
    }

    adecls.zipWithIndex foreach {
      case (AttrDecl(key, tpe, REQUIRED), j) if !ok(j) => exc ::= fromMissingAttribute(key, tpe)
      case _ =>
    }

    exc.length == len //- true if no new exception
  }

  /** check children, return true if conform to content model
   *  @note contentModel != null
   */
  def check(nodes: Seq[Node]): Boolean = contentModel match {
    case ANY    => true
    case EMPTY  => getIterable(nodes, false).isEmpty
    case PCDATA => getIterable(nodes, true).isEmpty
    case MIXED(ContentModel.Alt(branches @ _*))  =>   // @todo
      val j = exc.length
      def find(Key: String): Boolean =
        branches exists { case ContentModel.Letter(ElemName(Key)) => true ; case _ => false }

      getIterable(nodes, true) map (_.name) filterNot find foreach {
        exc ::= MakeValidationException fromUndefinedElement _
      }
      (exc.length == j)   // - true if no new exception

    case _: ELEMENTS =>
      dfa isFinal {
        getIterable(nodes, false).foldLeft(0) { (q, e) =>
          (dfa delta q).getOrElse(e, throw ValidationException("element %s not allowed here" format e))
        }
      }
    case _ => false
  }

  /** applies various validations - accumulates error messages in exc
   *  @todo fail on first error, ignore other errors (rearranging conditions)
   */
  def apply(n: Node): Boolean =
    //- ? check children
    ((contentModel == null) || check(n.child)) &&
    //- ? check attributes
    ((adecls == null) || check(n.attributes))
}
