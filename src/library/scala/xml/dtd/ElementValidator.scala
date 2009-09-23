/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml
package dtd

import PartialFunction._
import ContentModel.ElemName
import scala.util.automata._

/** validate children and/or attributes of an element
 *  exceptions are created but not thrown.
 */
class ElementValidator() extends Function1[Node,Boolean] {

  var exc: List[ValidationException] = Nil

  protected var contentModel: ContentModel           = _
  protected var dfa:          DetWordAutom[ElemName] = _
  protected var adecls:       List[AttrDecl]         = _

  /** set content model, enabling element validation */
  def setContentModel(cm:ContentModel) = {
    contentModel = cm; cm match {
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

  def getIterator(nodes: Seq[Node], skipPCDATA: Boolean): Iterator[ElemName] = {
    def isAllWhitespace(a: Atom[_]) = cond(a.data) { case s: String if s.trim.isEmpty  => true }

    nodes.filter {
      case y: SpecialNode => y match {
        case a: Atom[_] if isAllWhitespace(a) => false  // always skip all-whitespace nodes
        case _                                => !skipPCDATA
      }
      case x                                  => x.namespace eq null
    } . map (x => ElemName(x.label)) iterator
  }

  /** check attributes, return true if md corresponds to attribute declarations in adecls.
   */
  def check(md: MetaData): Boolean = {
    //@todo other exceptions
    import MakeValidationException._;
    val len: Int = exc.length;
    var j = 0;
    var ok = new scala.collection.mutable.BitSet(adecls.length);
    def find(Key:String): AttrDecl = {
      var attr: AttrDecl = null;
      val jt = adecls.iterator; while(j < adecls.length) {
        jt.next match {
          case a @ AttrDecl(Key, _, _) => attr = a; ok += j; j = adecls.length;
          case _                       => j = j + 1;
        }
      }
      attr
    }
    val it = md.iterator; while(it.hasNext) {
      val attr = it.next
      j = 0
      find(attr.key) match {

        case null =>
          //Console.println("exc");
          exc = fromUndefinedAttribute( attr.key ) :: exc;

        case AttrDecl(_, tpe, DEFAULT(true, fixedValue)) if attr.value.toString != fixedValue =>
          exc = fromFixedAttribute( attr.key, fixedValue, attr.value.toString) :: exc;

        case s =>
          //Console.println("s: "+s);

      }
    }

    //val missing = ok.toSet(false); FIXME: it doesn't seem to be used anywhere
    j = 0
    var kt = adecls.iterator
    while (kt.hasNext) {
      kt.next match {
        case AttrDecl(key, tpe, REQUIRED) if !ok(j) =>
          exc = fromMissingAttribute( key, tpe ) :: exc;
          j = j + 1;
        case _ =>
          j = j + 1;
      }
    }
    exc.length == len //- true if no new exception
  }

  /** check children, return true if conform to content model
   *  @pre contentModel != null
   */
  def check(nodes: Seq[Node]): Boolean = contentModel match {
    case ANY    => true
    case EMPTY  => !getIterator(nodes, false).hasNext
    case PCDATA => !getIterator(nodes, true).hasNext
    case MIXED(ContentModel.Alt(branches @ _*))  =>   // @todo
      val j = exc.length
      def find(Key: String): Boolean =
        branches exists { case ContentModel.Letter(ElemName(Key)) => true ; case _ => false }

      getIterator(nodes, true) map (_.name) filterNot find foreach {
        exc ::= MakeValidationException fromUndefinedElement _
      }
      (exc.length == j)   // - true if no new exception

    case _: ELEMENTS =>
      var q = 0
      getIterator(nodes, false) foreach { e =>
        (dfa delta q get e) match {
          case Some(p)  => q = p
          case _        => throw ValidationException("element %s not allowed here" format e)
        }
      }

      dfa isFinal q       // - true if arrived in final state
  }

  /** applies various validations - accumulates error messages in exc
   *  @todo: fail on first error, ignore other errors (rearranging conditions)
   */
  def apply(n: Node): Boolean =
    //- ? check children
    ((contentModel == null) || check(n.child)) &&
    //- ? check attributes
    ((adecls == null) || check(n.attributes))
}
