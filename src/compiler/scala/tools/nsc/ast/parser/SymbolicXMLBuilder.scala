/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Burak Emir
 */
// $Id: SymbolicXMLBuilder.scala 16884 2009-01-09 16:52:09Z cunei $

package scala.tools.nsc.ast.parser

import scala.collection.mutable.{Buffer, HashMap, ListBuffer, Map}
import scala.tools.nsc.util.Position
import scala.xml.{EntityRef, Text}
import symtab.Flags.MUTABLE

/** This class builds instance of <code>Tree</code> that represent XML.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
abstract class SymbolicXMLBuilder(make: TreeBuilder, p: Parsers # Parser, preserveWS: Boolean) {

  val global: Global
  import global._

  var isPattern: Boolean = _

  def _Attribute           = global.newTypeName("Attribute")
  def _MetaData            = global.newTypeName("MetaData")
  def _NamespaceBinding    = global.newTypeName("NamespaceBinding")
  def _NodeBuffer          = global.newTypeName("NodeBuffer")
  def _Null                = global.newTermName("Null")

  def _PrefixedAttribute   = global.newTypeName("PrefixedAttribute")
  def _UnprefixedAttribute = global.newTypeName("UnprefixedAttribute")
  def _Elem                = global.newTypeName("Elem")
  def __Elem               = global.newTermName("Elem")
  def _Group               = global.newTypeName("Group")
  def _Unparsed            = global.newTypeName("Unparsed")
  def _Seq                 = global.newTypeName("Seq")
  def _immutable           = global.newTermName("immutable")
  def _mutable             = global.newTermName("mutable")
  def _append              = global.newTermName("append")
  def _plus                = global.newTermName("$amp$plus")
  def _collection          = global.newTermName("collection")
  def _toList              = global.newTermName("toList")
  def _xml                 = global.newTermName("xml")
  def _Comment             = global.newTypeName("Comment")
  def _Node                = global.newTypeName("Node")
  def _None                = global.newTermName("None")
  def _Some                = global.newTypeName("Some")
  def _ProcInstr           = global.newTypeName("ProcInstr")
  def _Text                = global.newTypeName("Text")
  def __Text               = global.newTermName("Text")
  def _EntityRef           = global.newTypeName("EntityRef")

  final def _buf = global.newTermName("$buf")
  final def _md = global.newTermName("$md")
  final def _scope = global.newTermName("$scope")
  final def _tmpscope = global.newTermName("$tmpscope")

  // convenience methods
  private def LL[A](x: A*): List[List[A]] = List(List(x:_*))

  private def _scala(name: Name) =
    Select(Select(Ident(nme.ROOTPKG), nme.scala_), name)

  private def _scala_Seq   = _scala(_Seq)
  private def _scala_xml(name: Name) = Select(_scala(_xml), name)

  private def _scala_xml_MetaData           = _scala_xml(_MetaData)
  private def _scala_xml_NamespaceBinding   = _scala_xml(_NamespaceBinding)
  private def _scala_xml_Null               = _scala_xml(_Null)
  private def _scala_xml_PrefixedAttribute  = _scala_xml(_PrefixedAttribute)
  private def _scala_xml_UnprefixedAttribute= _scala_xml(_UnprefixedAttribute)
  private def _scala_xml_Node               = _scala_xml(_Node)
  private def _scala_xml_NodeBuffer         = _scala_xml(_NodeBuffer)
  private def _scala_xml_EntityRef          = _scala_xml(_EntityRef)
  private def _scala_xml_Comment            = _scala_xml(_Comment)
  private def _scala_xml_ProcInstr          = _scala_xml(_ProcInstr)
  private def _scala_xml_Text               = _scala_xml(_Text)
  private def _scala_xml__Text              = _scala_xml(__Text)
  private def _scala_xml_Elem               = _scala_xml(_Elem)
  private def _scala_xml__Elem              = _scala_xml(__Elem)
  private def _scala_xml_Attribute          = _scala_xml(_Attribute)
  private def _scala_xml_Group              = _scala_xml(_Group)
  private def _scala_xml_Unparsed           = _scala_xml(_Unparsed)

  // create scala xml tree

  /**
   *  @arg  namespace: a Tree of type defs.STRING_TYPE
   *  @arg  label:     a Tree of type defs.STRING_TYPE
   *  @todo map:       a map of attributes !!!
   */

  protected def mkXML(pos: Position, isPattern: Boolean, pre: Tree, label: Tree, attrs: /*Array[*/Tree/*]*/ , scope:Tree, children: Buffer[Tree]): Tree = {
    if (isPattern) {
      convertToTextPat(children)
      atPos (pos) { //@todo maybe matching on attributes, scope?
        Apply( _scala_xml__Elem, List(
          pre, label, Ident(nme.WILDCARD) /* md */ , Ident(nme.WILDCARD)) /* scope */ ::: children.toList )
      }
    } else {
      var ab = List(pre, label, attrs, scope)
      if (children.length > 0)
        ab = ab ::: List(Typed(makeXMLseq(pos, children), Ident(nme.WILDCARD_STAR.toTypeName)));
      atPos(pos) { New( _scala_xml_Elem, List(ab) )}
    }
  }

  final def entityRef(pos: Position, n: String) = {
    atPos(pos) { New( _scala_xml_EntityRef, LL(Literal(Constant( n )))) }

  };
  // create scala.xml.Text here <: scala.xml.Node
  final def text(pos: Position, txt:String): Tree =  {
    //makeText( isPattern, gen.mkStringLit( txt ))
    val txt1 = Literal(Constant(txt))
    atPos(pos) {
      if (isPattern)
        makeTextPat(txt1)
      else
        makeText1(txt1)
    }
  }

  // create scala.xml.Text here <: scala.xml.Node
  def makeTextPat(txt: Tree) = Apply(_scala_xml__Text, List(txt))

  def makeText1(txt: Tree) =
    New(_scala_xml_Text, LL(txt))

  // create
  def comment(pos: Position, text: String): Tree =
    atPos(pos) { Comment( Literal(Constant(text))) }

  // create
  def charData(pos: Position, txt: String): Tree =
    atPos(pos) { makeText1(Literal(Constant(txt))) }; //{ CharData( Literal(Constant(txt))) };

  // create scala.xml.Text here <: scala.xml.Node
  def procInstr( pos: Position, target: String, txt: String ) =
    atPos(pos) { ProcInstr(Literal(Constant(target)), Literal(Constant(txt))) }

  protected def Comment(txt: Tree) = New(_scala_xml_Comment, LL(txt))

  protected def ProcInstr(target: Tree, txt: Tree) =
    New(_scala_xml_ProcInstr, LL(target, txt))

  /** @todo: attributes */
  def makeXMLpat(pos: Position, n: String, args: Buffer[Tree]): Tree = {
    val (prepat, labpat) = n.indexOf(':') match {
      case -1 => (Ident(nme.WILDCARD), Literal(Constant(n)))
      //case 0  => // is erroneous, but cannot happen
      case i  => //if(i+1<n.length) // we ensure i+1<n.length in method xName
        (Literal(Constant(n.substring(0,i))), Literal(Constant(n.substring(i+1,n.length))))
        //else { p.syntaxError(pos,"nonsensical qualified name in XML"); return Ident(nme.WILDCARD).setPos(pos)}
    }
    mkXML(pos,
          true,
          prepat, //Ident( nme.WILDCARD ),
          labpat, //Literal(Constant(n)),
          null, //Array[Tree](),
          null,
          args);
  }

  protected def convertToTextPat(t: Tree): Tree = t match {
    case _:Literal => makeTextPat(t)
    case _ => t
  }

  def parseAttribute(pos: Position, s: String): Tree = {
    val ns = xml.Utility.parseAttributeValue(s)
    val ts: ListBuffer[Tree] = new ListBuffer
    val it = ns.iterator
    while (it.hasNext) it.next match {
      case Text(s)      => ts += text(pos, s) // makeText1(Literal(Constant(s)))
      case EntityRef(s) => ts += entityRef(pos, s)
    }
    ts.length match {
      case 0 => gen.mkNil
      case 1 => val t = ts(0); ts.clear; t
      case _ => makeXMLseq(pos, ts)
    }
  }

  protected def convertToTextPat(buf: Buffer[Tree]) {
    var i = 0; while (i < buf.length) {
      val t1 = buf(i)
      val t2 = convertToTextPat(t1)
      if (!t1.eq(t2)) {
        buf.remove(i)
        buf.insert(i, t2)
      }
      i += 1
    }
  }

  def freshName(prefix: String): Name

  def isEmptyText(t: Tree) = t match {
    case Literal(Constant("")) => true
    case _ => false
  }

  // could optimize if args.length == 0, args.length == 1 AND args(0) is <: Node.
  def makeXMLseq(pos: Position, args: Buffer[Tree] ) = {
    //var _buffer = New( _scala_xml_NodeBuffer, List(Nil))

    var as:List[Tree] = ValDef(NoMods, _buf, TypeTree(), New( _scala_xml_NodeBuffer, List(Nil)))::Nil
    val it = args.iterator
    while (it.hasNext) {
      val t = it.next
      if (!isEmptyText(t)) {
        //_buffer = Apply(Select(_buffer, _plus), List(t))
        as = Apply(Select(Ident(_buf), _plus), List(t))::as
      }
    }
    //atPos(pos) { Select(_buffer, _toList) }

    atPos(pos) {
      Block(as.reverse, Ident(_buf))
    }
  }
  /** returns Some(prefix) if pre:name, None otherwise */
  def getPrefix(name: String): Option[String] = {
    val i = name.indexOf(':')
    if (i != -1) Some(name.substring(0, i)) else None
  }

  def group(pos: Position, args: Buffer[Tree]): Tree = {
    atPos(pos) { New( _scala_xml_Group, LL( makeXMLseq(pos, args))) }
  }

  /** code that constructs an unparsed node
  */
  def unparsed(pos: Position, str: String): Tree = {
    atPos(pos) { New( _scala_xml_Unparsed, LL( Literal(Constant(str)))) }
  }

  /** makes an element */
  def element(pos: Position, qname: String, attrMap: Map[String,Tree], args: Buffer[Tree]): Tree = {
    //Console.println("SymbolicXMLBuilder::element("+pos+","+qname+","+attrMap+","+args+")");
    var setNS = new HashMap[String, Tree]

    var tlist: List[Tree] = List()

    /* pre can be null */
    def handleNamespaceBinding(pre: String , uri1: Tree) {
      def mkAssign(t: Tree): Tree =
        Assign(Ident(_tmpscope), New( _scala_xml_NamespaceBinding,
                                      LL(Literal(Constant(pre)), t, Ident( _tmpscope))))
      uri1 match {
        case Apply(_, List(uri @ Literal(Constant(_)))) => //text
          tlist = mkAssign(uri) :: tlist
        case Select(_, nme.Nil) =>  // allow for xmlns="" -- bug #1626
          tlist = mkAssign(Literal(Constant(null))) :: tlist
        case _ =>
          tlist = mkAssign(uri1) :: tlist
          //println("SymbolicXMLBuilder::handleNamespaceBinding:")
          //println(t.toString())
      }
    }

    /* DEBUG */
    val attrIt = attrMap.keysIterator
    while (attrIt.hasNext) {
      val z = attrIt.next
      if (z startsWith "xmlns") {  // handle namespace
        val i = z indexOf ':'
        if (i == -1)
          handleNamespaceBinding(null, attrMap(z))
          //setNS.update("default", attrMap(z))
        else {
          val zz = z.substring(i+1, z.length())
          //setNS.update( zz, attrMap( z ) );
          handleNamespaceBinding(zz, attrMap(z))
        }
        attrMap -= z
      }
    }

    val moreNamespaces = (0 < tlist.length)
    val i = qname indexOf ':'
    var newlabel = qname
    val pre = getPrefix(qname) match {
      case Some(p) =>
        newlabel = qname.substring(p.length()+1, qname.length())
        p
      case None =>
        null
    }
    var tlist2: List[Tree] = List()

    // make attributes

    def handlePrefixedAttribute(pre:String, key:String, value:Tree) {
      val t = atPos(pos) {
        Assign(Ident(_md), New( _scala_xml_PrefixedAttribute,
                                       LL(
                                         Literal(Constant(pre)),
                                         Literal(Constant(key)),
                                         value,
                                         Ident(_md)
                                       )))};
      tlist2 = t :: tlist2;
     // Console.println("SymbolicXMLBuilder::handlePrefixed :");
     // Console.println(t.toString());
    }

    def handleUnprefixedAttribute(key: String, value:Tree) {
      val t = atPos(pos) {
        Assign(Ident(_md), New(_scala_xml_UnprefixedAttribute,
                               LL(Literal(Constant(key)),value,Ident(_md))
                             ))};
      tlist2 = t :: tlist2
    }

    var it = attrMap.iterator
    while (it.hasNext) {
      val ansk = it.next
      getPrefix(ansk._1) match {
        case Some(pre) =>
          val key = ansk._1.substring(pre.length()+1, ansk._1.length())
          handlePrefixedAttribute(pre, key, ansk._2)
        case None      =>
          handleUnprefixedAttribute(ansk._1, ansk._2)
      }
    }
    //  attrs

    val moreAttributes = (0 < tlist2.length)

    var ts: List[Tree] = tlist
    var ts2: List[Tree] = List()

    if (moreAttributes) {
      ts2 = atPos(pos) {ValDef(Modifiers(MUTABLE),
                              _md,
                              _scala_xml_MetaData,
                              _scala_xml_Null)} :: tlist2;
    }
    if (moreNamespaces) {
      ts = atPos(pos) {
      ValDef(Modifiers(MUTABLE),
             _tmpscope,
             _scala_xml_NamespaceBinding,
             Ident(_scope))} :: ts;

      ts2 = ValDef(NoMods, _scope, _scala_xml_NamespaceBinding, Ident(_tmpscope)) :: ts2
    }

    val makeSymbolicAttrs =
      if (moreAttributes) Ident(_md) else _scala_xml_Null

    var t = mkXML(pos,
                  false,
                  Literal(Constant(pre)) /* can be null */ ,
                  Literal(Constant(newlabel)): Tree,
                  makeSymbolicAttrs,
                  Ident(_scope),
                  args);

    atPos(pos) { Block(ts, Block(ts2, t)) }
  }
}

