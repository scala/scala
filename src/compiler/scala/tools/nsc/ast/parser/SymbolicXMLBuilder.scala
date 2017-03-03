/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Burak Emir
 */

package scala.tools.nsc
package ast.parser

import scala.collection.mutable
import symtab.Flags.MUTABLE
import scala.reflect.internal.util.ListOfNil
import scala.reflect.internal.util.StringOps.splitWhere

/** This class builds instance of `Tree` that represent XML.
 *
 *  Note from martin: This needs to have its position info reworked. I don't
 *  understand exactly what's done here. To make validation pass, I set many
 *  positions to be transparent. Not sure this is a good idea for navigating
 *  XML trees in the IDE but it's the best I can do right now. If someone
 *  who understands this part better wants to give it a shot, please do!
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
abstract class SymbolicXMLBuilder(p: Parsers#Parser, preserveWS: Boolean) {
  val global: Global
  import global._

  private[parser] var isPattern: Boolean = _

  private object xmltypes extends TypeNames {
    val _Comment: NameType             = "Comment"
    val _Elem: NameType                = "Elem"
    val _EntityRef: NameType           = "EntityRef"
    val _Group: NameType               = "Group"
    val _MetaData: NameType            = "MetaData"
    val _NamespaceBinding: NameType    = "NamespaceBinding"
    val _NodeBuffer: NameType          = "NodeBuffer"
    val _PCData: NameType              = "PCData"
    val _PrefixedAttribute: NameType   = "PrefixedAttribute"
    val _ProcInstr: NameType           = "ProcInstr"
    val _Text: NameType                = "Text"
    val _Unparsed: NameType            = "Unparsed"
    val _UnprefixedAttribute: NameType = "UnprefixedAttribute"
  }

  private object xmlterms extends TermNames {
    val _Null: NameType     = "Null"
    val __Elem: NameType    = "Elem"
    val _PCData: NameType   = "PCData"
    val __Text: NameType    = "Text"
    val _buf: NameType      = "$buf"
    val _md: NameType       = "$md"
    val _plus: NameType     = "$amp$plus"
    val _scope: NameType    = "$scope"
    val _tmpscope: NameType = "$tmpscope"
    val _xml: NameType      = "xml"
  }

  import xmltypes.{
    _Comment, _Elem, _EntityRef, _Group, _MetaData, _NamespaceBinding, _NodeBuffer,
    _PCData, _PrefixedAttribute, _ProcInstr, _Text, _Unparsed, _UnprefixedAttribute
  }

  import xmlterms.{ _Null, __Elem, __Text, _buf, _md, _plus, _scope, _tmpscope, _xml }

  /** Attachment for trees deriving from text nodes (Text, CData, entities). Used for coalescing. */
  case class TextAttache(pos: Position, text: String)

  // convenience methods
  private def LL[A](x: A*): List[List[A]] = List(List(x:_*))
  private def const(x: Any) = Literal(Constant(x))
  private def wild                          = Ident(nme.WILDCARD)
  private def wildStar                      = Ident(tpnme.WILDCARD_STAR)
  private def _scala(name: Name)            = Select(Select(Ident(nme.ROOTPKG), nme.scala_), name)
  private def _scala_xml(name: Name)        = Select(_scala(_xml), name)

  private def _scala_xml_Comment            = _scala_xml(_Comment)
  private def _scala_xml_Elem               = _scala_xml(_Elem)
  private def _scala_xml_EntityRef          = _scala_xml(_EntityRef)
  private def _scala_xml_Group              = _scala_xml(_Group)
  private def _scala_xml_MetaData           = _scala_xml(_MetaData)
  private def _scala_xml_NamespaceBinding   = _scala_xml(_NamespaceBinding)
  private def _scala_xml_NodeBuffer         = _scala_xml(_NodeBuffer)
  private def _scala_xml_Null               = _scala_xml(_Null)
  private def _scala_xml_PrefixedAttribute  = _scala_xml(_PrefixedAttribute)
  private def _scala_xml_ProcInstr          = _scala_xml(_ProcInstr)
  private def _scala_xml_Text               = _scala_xml(_Text)
  private def _scala_xml_Unparsed           = _scala_xml(_Unparsed)
  private def _scala_xml_UnprefixedAttribute= _scala_xml(_UnprefixedAttribute)
  private def _scala_xml__Elem              = _scala_xml(__Elem)
  private def _scala_xml__Text              = _scala_xml(__Text)

  /** Wildly wrong documentation deleted in favor of "self-documenting code." */
  protected def mkXML(
    pos: Position,
    isPattern: Boolean,
    pre: Tree,
    label: Tree,
    attrs: Tree,
    scope: Tree,
    empty: Boolean,
    children: Seq[Tree]): Tree =
  {
    def starArgs =
      if (children.isEmpty) Nil
      else List(Typed(makeXMLseq(pos, children), wildStar))

    def pat    = Apply(_scala_xml__Elem, List(pre, label, wild, wild) ::: convertToTextPat(children))
    def nonpat = New(_scala_xml_Elem, List(List(pre, label, attrs, scope, if (empty) Literal(Constant(true)) else Literal(Constant(false))) ::: starArgs))

    atPos(pos) { if (isPattern) pat else nonpat }
  }

  final def entityRef(pos: Position, n: String) =
    atPos(pos)( New(_scala_xml_EntityRef, LL(const(n))) )

  private def coalescing = settings.XxmlSettings.isCoalescing

  // create scala.xml.Text here <: scala.xml.Node
  final def text(pos: Position, txt: String): Tree = atPos(pos) {
    val t = if (isPattern) makeTextPat(const(txt)) else makeText1(const(txt))
    if (coalescing) t updateAttachment TextAttache(pos, txt) else t
  }

  def makeTextPat(txt: Tree)                = Apply(_scala_xml__Text, List(txt))
  def makeText1(txt: Tree)                  = New(_scala_xml_Text, LL(txt))
  def comment(pos: Position, text: String)  = atPos(pos)( Comment(const(text)) )
  def charData(pos: Position, txt: String)  = if (coalescing) text(pos, txt) else atPos(pos) {
    if (isPattern) Apply(_scala_xml(xmlterms._PCData), List(const(txt)))
    else New(_scala_xml(_PCData), LL(const(txt)))
  }

  def procInstr(pos: Position, target: String, txt: String) =
    atPos(pos)( ProcInstr(const(target), const(txt)) )

  protected def Comment(txt: Tree)                  = New(_scala_xml_Comment, LL(txt))
  protected def ProcInstr(target: Tree, txt: Tree)  = New(_scala_xml_ProcInstr, LL(target, txt))

  /** @todo: attributes */
  def makeXMLpat(pos: Position, n: String, args: Seq[Tree]): Tree = {
    val (prepat, labpat) = splitPrefix(n) match {
      case (Some(pre), rest)  => (const(pre), const(rest))
      case _                  => (wild, const(n))
    }
    mkXML(pos, isPattern = true, prepat, labpat, null, null, empty = false, args)
  }

  protected def convertToTextPat(t: Tree): Tree = t match {
    case _: Literal => makeTextPat(t)
    case _          => t
  }
  protected def convertToTextPat(buf: Seq[Tree]): List[Tree] =
    (buf map convertToTextPat).toList

  def parseAttribute(pos: Position, s: String): Tree = {
    import xml.Utility.parseAttributeValue

    parseAttributeValue(s, text(pos, _), entityRef(pos, _)) match {
      case Nil      => gen.mkNil
      case t :: Nil => t
      case ts       => makeXMLseq(pos, ts.toList)
    }
  }

  def isEmptyText(t: Tree) = t match {
    case Literal(Constant("")) => true
    case _ => false
  }

  /** could optimize if args.length == 0, args.length == 1 AND args(0) is <: Node. */
  def makeXMLseq(pos: Position, args: Seq[Tree]) = {
    val buffer = ValDef(NoMods, _buf, TypeTree(), New(_scala_xml_NodeBuffer, ListOfNil))
    val applies = args filterNot isEmptyText map (t => Apply(Select(Ident(_buf), _plus), List(t)))

    atPos(pos)( Block(buffer :: applies.toList, Ident(_buf)) )
  }

  /** Returns (Some(prefix) | None, rest) based on position of ':' */
  def splitPrefix(name: String): (Option[String], String) = splitWhere(name, _ == ':', doDropIndex = true) match {
    case Some((pre, rest))  => (Some(pre), rest)
    case _                  => (None, name)
  }

  /** Various node constructions. */
  def group(pos: Position, args: Seq[Tree]): Tree =
    atPos(pos)( New(_scala_xml_Group, LL(makeXMLseq(pos, args))) )

  def unparsed(pos: Position, str: String): Tree =
    atPos(pos)( New(_scala_xml_Unparsed, LL(const(str))) )

  def element(pos: Position, qname: String, attrMap: mutable.Map[String, Tree], empty: Boolean, args: Seq[Tree]): Tree = {
    def handleNamespaceBinding(pre: String, z: String): Tree = {
      def mkAssign(t: Tree): Tree = Assign(
        Ident(_tmpscope),
        New(_scala_xml_NamespaceBinding, LL(const(pre), t, Ident(_tmpscope)))
      )

      val uri1 = attrMap(z) match {
        case Apply(Select(New(Select(Select(Select(Ident(nme.ROOTPKG), nme.scala_), nme.xml), tpnme.Text)), nme.CONSTRUCTOR), List(uri @ Literal(Constant(_)))) =>
          mkAssign(uri)
        case Select(_, nme.Nil)                         => mkAssign(const(null))  // allow for xmlns="" -- bug #1626
        case x                                          => mkAssign(x)
      }
      attrMap -= z
      uri1
    }

    /* Extract all the namespaces from the attribute map. */
    val namespaces: List[Tree] =
      for (z <- attrMap.keys.toList ; if z startsWith "xmlns") yield {
        val ns = splitPrefix(z) match {
          case (Some(_), rest)  => rest
          case _                => null
        }
        handleNamespaceBinding(ns, z)
      }

    val (pre, newlabel) = splitPrefix(qname) match {
      case (Some(p), x) => (p, x)
      case (None, x)    => (null, x)
    }

    def mkAttributeTree(pre: String, key: String, value: Tree) = atPos(pos.makeTransparent) {
      // XXX this is where we'd like to put Select(value, nme.toString_) for #1787
      // after we resolve the Some(foo) situation.
      val baseArgs = List(const(key), value, Ident(_md))
      val (clazz, attrArgs) =
        if (pre == null) (_scala_xml_UnprefixedAttribute, baseArgs)
                    else (_scala_xml_PrefixedAttribute  , const(pre) :: baseArgs)

      Assign(Ident(_md), New(clazz, LL(attrArgs: _*)))
    }

    def handlePrefixedAttribute(pre: String, key: String, value: Tree)  = mkAttributeTree(pre, key, value)
    def handleUnprefixedAttribute(key: String, value: Tree)             = mkAttributeTree(null, key, value)

    val attributes: List[Tree] =
      for ((k, v) <- attrMap.toList.reverse) yield splitPrefix(k) match {
        case (Some(pre), rest)  => handlePrefixedAttribute(pre, rest, v)
        case _                  => handleUnprefixedAttribute(k, v)
      }

    lazy val scopeDef     = ValDef(NoMods, _scope, _scala_xml_NamespaceBinding, Ident(_tmpscope))
    lazy val tmpScopeDef  = ValDef(Modifiers(MUTABLE), _tmpscope, _scala_xml_NamespaceBinding, Ident(_scope))
    lazy val metadataDef  = ValDef(Modifiers(MUTABLE), _md, _scala_xml_MetaData, _scala_xml_Null)
    val makeSymbolicAttrs = if (!attributes.isEmpty) Ident(_md) else _scala_xml_Null

    val (attrResult, nsResult) =
      (attributes.isEmpty, namespaces.isEmpty) match {
        case (true ,  true)   => (Nil, Nil)
        case (true , false)   => (scopeDef :: Nil, tmpScopeDef :: namespaces)
        case (false,  true)   => (metadataDef :: attributes, Nil)
        case (false, false)   => (scopeDef :: metadataDef :: attributes, tmpScopeDef :: namespaces)
      }

    val body = mkXML(
      pos.makeTransparent,
      isPattern = false,
      const(pre),
      const(newlabel),
      makeSymbolicAttrs,
      Ident(_scope),
      empty,
      args
    )

    atPos(pos.makeTransparent)( Block(nsResult, Block(attrResult, body)) )
  }
}
