/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Burak Emir
 */
// $Id: SymbolicXMLBuilder.scala 17756 2009-05-18 14:28:59Z rytz $

package scala.tools.nsc
package ast.parser

import collection.mutable.Map
import xml.{ EntityRef, Text }
import xml.XML.{ xmlns }
import util.Position
import symtab.Flags.MUTABLE

/** This class builds instance of <code>Tree</code> that represent XML.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
abstract class SymbolicXMLBuilder(p: Parsers#Parser, preserveWS: Boolean)
{
  val global: Global
  import global._
  def freshName(prefix: String): Name

  var isPattern: Boolean = _

  def _Comment             = global.newTypeName("Comment")
  def _Elem                = global.newTypeName("Elem")
  def _EntityRef           = global.newTypeName("EntityRef")
  def _Group               = global.newTypeName("Group")
  def _MetaData            = global.newTypeName("MetaData")
  def _NamespaceBinding    = global.newTypeName("NamespaceBinding")
  def _NodeBuffer          = global.newTypeName("NodeBuffer")
  def _PrefixedAttribute   = global.newTypeName("PrefixedAttribute")
  def _ProcInstr           = global.newTypeName("ProcInstr")
  def _Text                = global.newTypeName("Text")
  def _Unparsed            = global.newTypeName("Unparsed")
  def _UnprefixedAttribute = global.newTypeName("UnprefixedAttribute")

  def __Elem               = global.newTermName("Elem")
  def __Text               = global.newTermName("Text")
  def _Null                = global.newTermName("Null")
  def _plus                = global.newTermName("$amp$plus")
  def _xml                 = global.newTermName("xml")

  final def _buf           = global.newTermName("$buf")
  final def _md            = global.newTermName("$md")
  final def _scope         = global.newTermName("$scope")
  final def _tmpscope      = global.newTermName("$tmpscope")

  // convenience methods
  private def LL[A](x: A*): List[List[A]] = List(List(x:_*))
  private def const(x: Any) = x match {
    case s: runtime.RichString  => Literal(Constant(s.toString))  // not our finest hour
    case _                      => Literal(Constant(x))
  }
  private def wild                          = Ident(nme.WILDCARD)
  private def wildStar                      = Ident(nme.WILDCARD_STAR.toTypeName)
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
    scope:Tree,
    children: Seq[Tree]): Tree =
  {
    def starArgs =
      if (children.isEmpty) Nil
      else List(Typed(makeXMLseq(pos, children), wildStar))

    def pat    = Apply(_scala_xml__Elem, List(pre, label, wild, wild) ::: convertToTextPat(children))
    def nonpat = New(_scala_xml_Elem, List(List(pre, label, attrs, scope) ::: starArgs))

    atPos(pos) { if (isPattern) pat else nonpat }
  }

  final def entityRef(pos: Position, n: String) =
    atPos(pos)( New(_scala_xml_EntityRef, LL(const(n))) )

  // create scala.xml.Text here <: scala.xml.Node
  final def text(pos: Position, txt: String): Tree = atPos(pos) {
    if (isPattern) makeTextPat(const(txt))
    else makeText1(const(txt))
  }

  def makeTextPat(txt: Tree)                = Apply(_scala_xml__Text, List(txt))
  def makeText1(txt: Tree)                  = New(_scala_xml_Text, LL(txt))
  def comment(pos: Position, text: String)  = atPos(pos)( Comment(const(text)) )
  def charData(pos: Position, txt: String)  = atPos(pos)( makeText1(const(txt)) )

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
    mkXML(pos, true, prepat, labpat, null, null, args)
  }

  protected def convertToTextPat(t: Tree): Tree = t match {
    case _: Literal => makeTextPat(t)
    case _          => t
  }
  protected def convertToTextPat(buf: Seq[Tree]): List[Tree] =
    (buf map convertToTextPat).toList

  def parseAttribute(pos: Position, s: String): Tree = {
    val ts = xml.Utility.parseAttributeValue(s) map {
      case Text(s)      => text(pos, s)
      case EntityRef(s) => entityRef(pos, s)
    }
    ts.length match {
      case 0 => gen.mkNil
      case 1 => ts.head
      case _ => makeXMLseq(pos, ts.toList)
    }
  }

  def isEmptyText(t: Tree) = t match {
    case Literal(Constant("")) => true
    case _ => false
  }

  /** could optimize if args.length == 0, args.length == 1 AND args(0) is <: Node. */
  def makeXMLseq(pos: Position, args: Seq[Tree]) = {
    val buffer = ValDef(NoMods, _buf, TypeTree(), New(_scala_xml_NodeBuffer, List(Nil)))
    val applies = args filterNot isEmptyText map (t => Apply(Select(Ident(_buf), _plus), List(t)))

    atPos(pos)( Block(buffer :: applies.toList, Ident(_buf)) )
  }

  /** Returns (Some(prefix) | None, rest) based on position of ':' */
  def splitPrefix(name: String): (Option[String], String) = (name indexOf ':') match {
    case -1   => (None, name)
    case i    => (Some(name take i), name drop (i + 1))
  }

  /** Various node constructions. */
  def group(pos: Position, args: Seq[Tree]): Tree =
    atPos(pos)( New(_scala_xml_Group, LL(makeXMLseq(pos, args))) )

  def unparsed(pos: Position, str: String): Tree =
    atPos(pos)( New(_scala_xml_Unparsed, LL(const(str))) )

  def element(pos: Position, qname: String, attrMap: Map[String, Tree], args: Seq[Tree]): Tree = {
    def handleNamespaceBinding(pre: String, z: String): Tree = {
      def mkAssign(t: Tree): Tree = Assign(
        Ident(_tmpscope),
        New(_scala_xml_NamespaceBinding, LL(const(pre), t, Ident(_tmpscope)))
      )

      val uri1 = attrMap(z) match {
        case Apply(_, List(uri @ Literal(Constant(_)))) => mkAssign(uri)
        case Select(_, nme.Nil)                         => mkAssign(const(null))  // allow for xmlns="" -- bug #1626
        case x                                          => mkAssign(x)
      }
      attrMap -= z
      uri1
    }

    /** Extract all the namespaces from the attribute map. */
    val namespaces: List[Tree] =
      for (z <- attrMap.keys.toList ; if z startsWith xmlns) yield {
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

    def mkAttributeTree(pre: String, key: String, value: Tree) = atPos(pos) {
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

    lazy val scopeDef     = atPos(pos)(ValDef(NoMods, _scope, _scala_xml_NamespaceBinding, Ident(_tmpscope)))
    lazy val tmpScopeDef  = atPos(pos)(ValDef(Modifiers(MUTABLE), _tmpscope, _scala_xml_NamespaceBinding, Ident(_scope)))
    lazy val metadataDef  = atPos(pos)(ValDef(Modifiers(MUTABLE), _md, _scala_xml_MetaData, _scala_xml_Null))
    val makeSymbolicAttrs = if (!attributes.isEmpty) Ident(_md) else _scala_xml_Null

    val (attrResult, nsResult) =
      (attributes.isEmpty, namespaces.isEmpty) match {
        case (true ,  true)   => (Nil, Nil)
        case (true , false)   => (scopeDef :: Nil, tmpScopeDef :: namespaces)
        case (false,  true)   => (metadataDef :: attributes, Nil)
        case (false, false)   => (scopeDef :: metadataDef :: attributes, tmpScopeDef :: namespaces)
      }

    val body = mkXML(
      pos,
      false,
      const(pre),
      const(newlabel),
      makeSymbolicAttrs,
      Ident(_scope),
      args
    )

    atPos(pos)( Block(nsResult, Block(attrResult, body)) )
  }
}
