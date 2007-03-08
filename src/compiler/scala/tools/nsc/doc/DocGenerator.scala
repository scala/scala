/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Sean McDirmid
 */
// $Id$

package scala.tools.nsc.doc

import java.io.{File, FileOutputStream, FileWriter}
import java.net.URLEncoder
import java.util.StringTokenizer
import java.util.regex.Pattern

import compat.Platform.{EOL => LINE_SEPARATOR}
import scala.collection.immutable.{ListMap, TreeMap, TreeSet}
import scala.collection.mutable.{HashMap, HashSet, ListBuffer, Map}
import scala.tools.nsc.models.Models
import scala.tools.nsc.symtab.Flags
import scala.xml._

/**
 *  @author  Sean McDirmid, Stephane Micheloud
 *  @version 1.0
 */
abstract class DocGenerator extends Models {
  import global._
  import DocUtil._
  import Kinds._
  import compat.StringBuilder


  def outdir: String
  def windowTitle: String
  def documentTitle: String
  def contentFrame = "contentFrame"
  def classesFrame = "classesFrame"
  def modulesFrame = "modulesFrame"
  def emptyMap = ListMap.empty[Kind, TreeSet[HasTree]]

  override def acceptPrivate = false

  abstract class Frame extends UrlContext {
    def path: String // relative to outdir
    def relative: String = {
      assert(path ne null)
      var idx = 0
      var ct = new StringBuilder
      while (idx != -1) {
        idx = path.indexOf('/', idx)
        //System.err.println(path + " idx=" + idx)
        ct.append(if (idx != -1) "../" else "")
        idx = idx + (if (idx == -1) 0 else 1)
      }
      ct.toString
    }

    def body: NodeSeq
    def title: String

    def save(nodes: NodeSeq) = {
      val path0 = outdir + File.separator + path + FILE_EXTENSION_HTML
      if (settings.debug.value) inform("Writing XML nodes to " + path0)
      val file = new File(path0)
      val parent = file.getParentFile()
      if (!parent.exists()) parent.mkdirs()
      val writer = new FileWriter(file)
      val str = dtype + LINE_SEPARATOR + nodes.toString()
      writer.write(str, 0, str.length())
      writer.close()
    }

    /**
     *  @param sym    ...
     *  @param target ...
     *  @return       ...
     */
    def urlFor(tree: Tree, target: String): NodeSeq = try {
      val sym = tree.symbol
      if (sym == NoSymbol)
        Text(tree.asInstanceOf[ValOrDefDef].name.toString())
      else if (sym.sourceFile eq null)
        Text(sym.fullNameString('.'))
      else
        aref(urlFor(sym), target, sym.nameString)
    } catch {
      case e: Error =>
        //System.err.println("SYM=" + sym)
        Text(tree.symbol.toString())
    }

    /**
     *  @param tpe    ...
     *  @param target ...
     *  @return       ...
     */
    def urlFor(tpe: Type, target: String): NodeSeq = try {
      if (tpe.symbol.hasFlag(Flags.JAVA) || (tpe.symbol.sourceFile eq null))
        <a class={tpe.toString().replace('.', '_')} href=""
          target={target}>{tpe.toString()}</a>
      /*
      else if (tpe.symbol.sourceFile eq null)
        Text(tpe.toString())
      */
      else {
        val n = tpe.typeArgs.length
        if (n > 0) {
          if (definitions.isFunctionType(tpe)) {
            val (ts, List(r)) = tpe.typeArgs.splitAt(n-1)
            Text("(") ++ (
              if (ts.isEmpty) NodeSeq.Empty
              else
                urlFor(ts.head, target) ++ {
                  val sep = Text(", ")
                  for (val t <- ts.tail)
                  yield Group(sep ++ urlFor(t, target))
                }
            )
            .++ (Text(") => ")) ++ (urlFor(r, target))
          } else
            aref(urlFor(tpe.symbol), target, tpe.symbol.fullNameString)
            .++ (Text("[") ++ urlFor(tpe.typeArgs.head, target)
            .++ {
              val sep = Text(", ")
              for (val t <- tpe.typeArgs.tail)
              yield Group(sep ++ urlFor(t, target))
            }
            .++ (Text("]")))
        } else tpe match {
          case RefinedType(parents, _) =>
            //Console.println("***** parents= " + parents)//debug
            val parents1 =
              if ((parents.length > 1) &&
                  (parents.head.symbol eq definitions.ObjectClass)) parents.tail
              else parents
            aref(urlFor(parents1.head.symbol), target, parents1.head.toString())
            .++ {
              val sep = Text(" with ")
              for (val t <- parents1.tail)
              yield Group(sep ++ urlFor(t, target))
            }
          case _ =>
            aref(urlFor(tpe.symbol), target, tpe.toString())
        }
      }
    } catch {
      case e: Error =>
        //System.err.println("SYM=" + sym)
        Text(tpe.symbol.toString())
    }

    def urlFor0(sym: Symbol, orig: Symbol): String = {
      (if (sym == NoSymbol) "XXX"
       else if (sym.owner.isPackageClass) sym.fullNameString('/')
       else urlFor0(sym.owner, orig) + "." + Utility.escape(sym.nameString)) +
      (sym match {
        case msym: ModuleSymbol =>
          if (msym hasFlag Flags.PACKAGE) NAME_SUFFIX_PACKAGE
          else NAME_SUFFIX_OBJECT
        case csym: ClassSymbol =>
          if (csym.isModuleClass) {
            if (csym hasFlag Flags.PACKAGE) NAME_SUFFIX_PACKAGE
            else NAME_SUFFIX_OBJECT
          }
          else ""
        case _ =>
          //System.err.println("XXX: class or object " + orig + " not found in " + sym)
          "" //"XXXXX"
      })
    }

    private val patVal = Pattern.compile(
      "scala\\.(Byte|Boolean|Char|Double|Float|Int|Long|Short)")

    def docName(sym: Symbol): String = {
      def javaParams(paramTypes: List[Type]): String = {
        def javaName(pt: Type): String = {
          val s = pt.toString
          val matVal = patVal.matcher(s)
          if (matVal.matches) matVal.group(1).toLowerCase
          else s.replaceAll("\\$", ".")
        }
        paramTypes.map(pt => javaName(pt)).mkString("(", ",", ")")
      }
      def scalaParams(paramTypes: List[Type]): String = {
        def scalaName(pt: Type): String = pt.toString.replaceAll(" ", "")
        paramTypes.map(pt => scalaName(pt)).mkString("(", ",", ")")
      }
      URLEncoder.encode(sym.nameString +
        (sym.tpe match {
          case MethodType(paramTypes, _) =>
            if (sym hasFlag Flags.JAVA) javaParams(paramTypes)
            else scalaParams(paramTypes)
          case PolyType(_, MethodType(paramTypes, _)) =>
            if (sym hasFlag Flags.JAVA) javaParams(paramTypes)
            else scalaParams(paramTypes)
          case _ => ""
        }), encoding)
    }

    def urlFor(sym: Symbol): String = sym match {
      case msym: ModuleSymbol => urlFor0(sym, sym) + FILE_EXTENSION_HTML
      case csym: ClassSymbol =>  urlFor0(sym, sym) + FILE_EXTENSION_HTML
      case _ => urlFor(sym.owner) + "#" + docName(sym)
    }

    def hasBody = true

    save(page(title, body, hasBody))
  }

  private val doctitle: NodeSeq =
    <div class="doctitle-larger">
      {load(documentTitle)}
    </div>;

  abstract class ListModuleFrame extends Frame {
    val path  = "modules"
    val title = "List of all packages"
    def modules: TreeMap[String, ModuleClassSymbol]
    def body: NodeSeq =
      <div>
        {doctitle}
        <a href="all-classes.html" target={classesFrame} onclick="resetKind();">{"All objects and classes"}</a>
      </div>
      <div class="kinds">
        Packages
      </div>
      <ul class="list">
        { {
          for (val top <- modules.elements.toList) yield
            <li><a href={urlFor(top._2)} target={classesFrame} onclick="resetKind();">{top._2.fullNameString('.')}</a></li>
        } }
      </ul>;
  }

  abstract class ListModuleContentFrame extends Frame {
    val path  = "root-content"
    val title = "All Packages"
    def modules: TreeMap[String, ModuleClassSymbol]
    def body: NodeSeq =
      <div class="page-title">
        Scala 2
        <br/>API Specification
      </div>
      <p>
        This document is the API specification for Scala 2.
      </p><hr/>
      <table cellpadding="3" class="member" summary="">
        <tr><td colspan="2" class="title">
          Package Summary
        </td></tr>
        { {
          for (val top <- modules.elements.toList) yield
            <tr><td class="signature">
              <code>{Text("package")}
              {(aref(top._2.fullNameString('/') + "$content.html", "_self", top._2.fullNameString('.')))}</code>
            </td></tr>;
        } }
      </table>;
  }

  abstract class ListClassFrame extends Frame {
    def classes: ListMap[Kind, TreeSet[HasTree]]

    def navLabel: String

    private def path0 = {
      val p = path
      if (p endsWith NAME_SUFFIX_PACKAGE)
        p.substring(0, p.length() - NAME_SUFFIX_PACKAGE.length())
      else p
    }

    def body: NodeSeq = {
      val nav =
        <table class="navigation" summary="">
          <tr><td valign="top" class="navigation-links">
            {aref(path0 + "$content.html", contentFrame, navLabel)}
          </td></tr>
        </table>;

      val ids = new HashSet[String]
      def idFor(kind: Kind, t: Tree): String = {
        val ch = t.symbol.nameString.charAt(0)
        val id = pluralFor(kind) + "_" + ch
        if (ids contains id) null
        else { ids += id; id }
      }
      val body = <div>{ { for (val kind <- KINDS; classes contains kind) yield {
        <div id={pluralFor(kind)} class="kinds">
          {Text(pluralFor(kind))}
        </div>
        <ul class="list">
          { {
            for (val mmbr <- classes(kind).toList) yield
              <li id={idFor(kind, mmbr.tree)}>{urlFor(mmbr.tree, contentFrame)}</li>
          } }
        </ul>
      } } }</div>;

      nav ++ body
    }
  }

  abstract class ContentFrame0 extends Frame {

    private def extendsFor(mmbr: HasTree): NodeSeq = mmbr match {
      case mmbr: ImplMod =>
        val parents = mmbr.treey.impl.parents
        if (parents.isEmpty) NodeSeq.Empty
        else
          <dd>
            <code>{Text(" extends ")}</code>{forType(parents.head.tpe)}
          </dd> ++ (
          { {
            for (val parent <- parents.tail) yield
              <dd>
                <code>{Text(" with ")}</code>{forType(parent.tpe)}
              </dd>
          } })
      case _ =>
        NodeSeq.Empty
    }

    private def nameFor(tree: Tree) =
      if (tree.symbol == NoSymbol) tree.asInstanceOf[ValOrDefDef].name.toString()
      else tree.symbol.nameString

    private def attrsFor(tree: Tree): NodeSeq =
      if (tree.symbol.attributes.isEmpty || tree.symbol.hasFlag(Flags.CASE))
        NodeSeq.Empty
      else {
        def attrFor(attr: AnnotationInfo[Constant]): Node = {
          val buf = new StringBuilder
          val AnnotationInfo(tpe, args, nvPairs) = attr
          val name = aref(urlFor(tpe.symbol), contentFrame, tpe.toString)
          if (!args.isEmpty)
            buf.append(args.map(.escapedStringValue).mkString("(", ",", ")"))
          if (!nvPairs.isEmpty)
            for (val ((name, value), index) <- nvPairs.zipWithIndex) {
              if (index > 0)
                buf.append(", ")
              buf.append(name).append(" = ").append(value)
            }
          Group(name ++ Text(buf.toString))
        }
        val sep = Text("@")
        for (val attr <- tree.symbol.attributes)
        yield Group(br(sep ++ attrFor(attr)))
      }

    /**
     *  @param mmbr ...
     *  @return     ...
     */
    def fullHeader(mmbr: HasTree): NodeSeq = Group(
      { {
        if (mmbr.isInstanceOf[ImplMod]) NodeSeq.Empty
        //else <a name={Utility.escape(mmbr.tree.symbol.nameString)}></a>
        else <a name={docName(mmbr.tree.symbol)}></a>
      } } ++ (
      <dl>
        <dt>
          { attrsFor(mmbr.tree) }
          <code>
            { { for (val str <- stringsFor(mmbr.mods)) yield Text(str + " ") } }
            { Text(codeFor(mmbr.kind)) }
          </code>
          <em>{ Text(nameFor(mmbr.tree)) }</em>
          { typesFor(mmbr) }{ argsFor(mmbr)}{resultFor(mmbr) }
        </dt>
        <dd>{ extendsFor(mmbr) }</dd>
      </dl>) ++ fullComment(mmbr) ++ hr(listSubclasses(mmbr)) ++ lists(mmbr))
//      { lists(mmbr) }

    /** Return a NodeSeq with the known subclasses for <code>mmbr</code>, if any.
     *
     *  @param mmbr ...
     *  @return     ...
     */
    def listSubclasses(mmbr: HasTree): NodeSeq = {
      val subcs = subclasses(mmbr.tree.symbol)
      if (subcs.isEmpty)
        NodeSeq.Empty
      else
        <dl>
          <dt style="margin:10px 0 0 20px;">
            <b>Direct Known Subclasses:</b>
          </dt>
          <dd>{ {
            val links =
              for (val subc <- subcs)
              yield aref(urlFor(subc), contentFrame, subc.nameString)
            links.reduceRight { (link: Seq[Node], seq: Seq[Node]) => link ++ Text(", ") ++ seq }
          } }</dd>
        </dl>;
    }

    def lists(mmbr: HasTree): NodeSeq = mmbr match {
      case cmod: ImplMod =>
        <span>
          { listMembersShort(mmbr) }
          { listMembersFull(mmbr) }
        </span>
      case _ =>
        NodeSeq.Empty
    }

    /**
     *  @param mmbr ...
     *  @return     ...
     */
    def listMembersShort(mmbr: HasTree): NodeSeq =
      if (mmbr.isInstanceOf[Composite]) {
        val map = organize(mmbr.asInstanceOf[Composite], emptyMap)
        for (val kind <- KINDS) yield Group(
          (if (map contains kind)
             <table cellpadding="3" class="member" summary="">
               <tr>
                 <td colspan="2" class="title">{Text(labelFor(kind))} Summary</td>
               </tr>
               { {
                 for (val mmbr <- map(kind).toList) yield
                   shortHeader(mmbr)
               } }
             </table>
           else
             NodeSeq.Empty
          ) ++ (listInheritedMembers(mmbr.tree.symbol, kind)))
      } else
        NodeSeq.Empty

    /**
     *  @param sym  the symbol
     *  @param kind the selected kind of members
     *  @return     a sequence of HTML tables containing inherited members
     */
    def listInheritedMembers(sym: Symbol, kind: Kind): NodeSeq = {
      val ignored = List(definitions.ObjectClass, definitions.ScalaObjectClass)
      def isVisible(sym: Symbol) =
        (kind == DEF && sym.isMethod && !sym.isConstructor &&
         !sym.isCaseFactory && !sym.hasFlag(Flags.ACCESSOR)) ||
        (kind == VAR && sym.isVariable) ||
        (kind == VAL && sym.isValue && !sym.isVariable && sym.hasGetter)
      val parents = sym.info.parents
      for (val p <- parents; !ignored.contains(p.symbol);
           val decls = p.decls.toList filter(member => isVisible(member));
           !decls.isEmpty) yield Group(
        <table cellpadding="3" class="inherited" summary="">
          <tr>
            <td colspan="2" class="title">
              {Text(kind.toString + " inherited from ") ++ urlFor(p, contentFrame)}
            </td>
          </tr>
          <tr> {
            def aref1(sym: Symbol): NodeSeq = {
              val isJava = sym hasFlag Flags.JAVA
              if (isJava || (sym.sourceFile eq null)) {
                <a class={sym.owner.fullNameString.replace('.', '_')}
                  href={"#" + docName(sym)}
                  target={contentFrame}>{sym.nameString}</a>
              }
              else
                aref(urlFor(sym), contentFrame, sym.nameString)
            }
            val members = decls.sort(
              (x, y) => (x.nameString compareTo y.nameString) < 0)
            <td colspan="2" class="signature">
              {aref1(members.head)}
              {for (val m <- members.tail) yield Text(", ") ++ aref1(m)}
            </td>
          } </tr>
        </table>)
    }

    /**
     *  @param mmbr ...
     *  @return     ...
     */
    def listMembersFull(mmbr: HasTree): NodeSeq =
      if (mmbr.isInstanceOf[Composite]) {
        val map = organize(mmbr.asInstanceOf[Composite], emptyMap)
        val mmbrx = mmbr
        val pathx = path
        for (val kind0 <- List(OBJECT, CLASS); map contains kind0)
          for (val mmbr <- map(kind0))
            new ContentFrame {
              def clazz = mmbr.asInstanceOf[ImplMod]
              def kind = kind0
              def title =
                labelFor(kind0) + " " + mmbr.tree.symbol.nameString + " in " +
                codeFor(mmbrx.kind) + " " + mmbr.tree.symbol.owner.fullNameString('.')
            }
        for (val kind <- List(TRAIT, CONSTRUCTOR, VAL, VAR, DEF); map contains kind) yield Group(
          <table cellpadding="3" class="member-detail" summary="">
            <tr>
              <td class="title">{Text(labelFor(kind))} Detail</td>
            </tr>
          </table>
          <div>
            {for (val mmbr <- map(kind).toList) yield fullHeader(mmbr)}
          </div>)
      } else
        NodeSeq.Empty

    /**
     *  @param mmbr ...
     *  @return     ...
     */
    def shortHeader(mmbr: HasTree): NodeSeq =
      <tr>
        <td valign="top" class="modifiers">
          { for (val str <- stringsFor(mmbr.mods)) yield <code>{(Text(str + " "))}</code> }
        </td>
        <td class="signature">
          <code>{Text(codeFor(mmbr.kind))}</code>
          <em>{urlFor(mmbr.tree, contentFrame)}</em>
          { typesFor(mmbr) }
          {  argsFor(mmbr) }
          {resultFor(mmbr) }
          <br>{shortComment(mmbr)}</br>
        </td>
      </tr>

    def fullComment(mmbr: HasTree): NodeSeq = {
      val sym = mmbr.tree.symbol
      val (sym1, kind1) =
        if (sym.isPrimaryConstructor) (sym.owner, CONSTRUCTOR)
        else (sym, if (sym.isConstructor) DEF else kindOf(mmbr.tree))
      comments.get(sym1) match {
        case Some(text) => comment(text, false, kind1)
        case None => NodeSeq.Empty
      }
    }

    def shortComment(mmbr: HasTree): NodeSeq =
      comments.get(mmbr.tree.symbol) match {
        case Some(text) => comment(text, true, kindOf(mmbr.tree))
        case None => NodeSeq.Empty
      }

    def ifT(cond: Boolean, nodes: NodeSeq) =
      if (cond) nodes else NodeSeq.Empty

    def ifT(tree: Tree, nodes: NodeSeq, before: Boolean) =
      if (tree != EmptyTree &&
          tree.tpe.symbol != definitions.AnyClass &&
          tree.tpe.symbol != definitions.AllClass) {
        if (before) nodes ++ forTree(tree)
        else {
          val ret = forTree(tree) ++ nodes
          //System.err.println("RET: " + ret)
          ret
        }
      } else NodeSeq.Empty

    def forType(tpe: Type): NodeSeq =
      urlFor(tpe, contentFrame)

    def forTree(tree: Tree): NodeSeq = tree match {
      case vdef: ValDef =>
        Text(vdef.symbol.name.toString()) ++ Text(": ") ++ forTree(vdef.tpt)
      case sel: Select =>
        forTree(sel.qualifier) ++ Text(sel.symbol.nameString)
      case tree: AbsTypeDef =>
        val cflags = if (tree.mods.isCovariant) "+"
        else if (tree.mods.isContravariant) "-"
        else ""
        Text(cflags + tree.symbol.nameString) ++ ifT(tree.hi, Text(" <: "), true) ++ ifT(tree.lo, Text(" >: "), true)
      case tpt: TypeTree =>
        urlFor(tpt.tpe, contentFrame)
      case id: Ident =>
        Text("YY: " + id.symbol.nameString)
      case EmptyTree =>
        NodeSeq.Empty
      case _ =>
        Text("XX=" + tree.getClass() + " " + tree.toString())
    }

    /**
     *  @param trees ...
     *  @return      ...
     */
    def forTrees(trees: List[Tree]): NodeSeq =
      if (trees.isEmpty) NodeSeq.Empty
      else {
        val head = forTree(trees.head)
        head ++ (if (trees.tail.isEmpty) NodeSeq.Empty
                    else Text(", ")) ++ forTrees(trees.tail)
      }

    private def surround(open: String, close: String, node: NodeSeq): NodeSeq =
      Text(open) ++ node ++ Text(close)

    /**
     *  @param ht ...
     *  @return   ...
     */
    private def typesFor(ht: HasTree): NodeSeq = {
      val tparams = ht.tree match {
        case cdef: ClassDef     => cdef.tparams
        case ddef: DefDef       => ddef.tparams
        case adef: AliasTypeDef => adef.tparams
        case _ => Nil
      }
      if (tparams.isEmpty) Text("")
      else surround("[", "]", forTrees(tparams))
    }

    /**
     *  @param ht ...
     *  @return   ...
     */
    private def argsFor(ht: HasTree): NodeSeq = ht.tree match {
      case ddef: DefDef =>
        if (!ddef.vparamss.isEmpty &&
            (!ddef.vparamss.tail.isEmpty || !ddef.vparamss.head.isEmpty)) {
          val nodes = for (val vparams <- ddef.vparamss)
            yield surround("(", ")", forTrees(vparams));
          nodes.flatMap(x => x.toList)
        } else NodeSeq.Empty
      case _ => NodeSeq.Empty
    }

    /**
     *  @param ht ...
     *  @return   ...
     */
    private def resultFor(ht: HasTree): NodeSeq = ht.tree match {
      case vdef: ValOrDefDef =>
        if (!vdef.symbol.nameString.equals("this"))
          Text(": ") ++ forType(vdef.tpt.tpe)
        else
          NodeSeq.Empty
      case _ =>
        NodeSeq.Empty
    }
  }

  abstract class ListClassContentFrame extends ContentFrame0 {
    def classes: ListMap[Kind, TreeSet[HasTree]]
    def module: ModuleClassSymbol

    def path  = module.fullNameString('/') + "$content"
    def title = "All Classes and Objects in " + module.fullNameString('.')

    def body: NodeSeq =
      <div class="page-title">
        Scala 2<br/>API Specification
      </div>
      <p>
        This document is the API specification for Scala 2.
      </p> ++ (for (val kind <- KINDS; classes contains kind) yield Group(hr(
          <table cellpadding="3" class="member" summary="">
            <tr>
              <td colspan="2" class="title">
                {labelFor(kind)} Summary
              </td>
            </tr>
            { {
              for (val mmbr <- classes(kind).toList) yield shortHeader(mmbr)
            } }
          </table>)))
  }

  abstract class ContentFrame extends ContentFrame0 {
    def clazz: ImplMod
    def kind: Kind
    def body: NodeSeq = <span>{navigation}{header0}{fullHeader(clazz)}</span>;

    final def path = urlFor0(clazz.tree.symbol, clazz.tree.symbol)

    // <td class="navigation-enabled">{aref("help.html"     , "_self", "Help"    )}</td>
    // <td class="navigation-enabled">{aref("root-page.html", "_self", "Overview")}</td>
    // <td class="navigation-enabled">{aref("index.html"    , null, "Index"   )}</td>
    private def navigation: NodeSeq =
      <table class="navigation" summary="">
        <tr>
          <td valign="top" class="navigation-links">
            <table><tr>
            </tr></table>
          </td>
          <td align="right" valign="top" style="white-space:nowrap;" rowspan="2">
            {doctitle}
          </td>
        </tr>
        <tr><td></td></tr>
      </table>;

    private def header0: NodeSeq = <span>
      <hr/> in {aref(urlFor(clazz.tree.symbol.owner), "_self", clazz.tree.symbol.owner.fullNameString('.'))}
      <div class="entity">
        {Text(codeFor(kind))}
        <span class="entity">{Text(clazz.tree.symbol.nameString)}</span>
      </div><hr/>
    </span>;
  }

  private val kinds =
    new TreeMap[String, Symbol => Boolean] +
      "Constructor" -> ((s: Symbol) => s.isConstructor) +
      "Def" -> ((s: Symbol) => s.isMethod)

  /** This abstract class contains two abstract methods <code>sym</code> and
   *  <code>descr</code> which must be defined for each primitive type in
   *  order to generated the appropriate HTML documentation page.
   *
   *  @author  Stephane Micheloud
   *  @version 1.0
   */
  private abstract class PrimitiveContentFrame extends ContentFrame0 {
    def sym: Symbol
    def descr: String
    def path = urlFor0(sym, sym)
    private def kind = CLASS // todo
    def title =
      labelFor(kind) + " " + sym.nameString + " in " +
      codeFor(kind) + " " + sym.owner.fullNameString('.')
    def body = NodeSeq.fromSeq(
      <table class="navigation" summary="">
        <tr>
          <td valign="top" class="navigation-links">
            <table><tr>
            </tr></table>
          </td>
          <td align="right" valign="top" style="white-space:nowrap;" rowspan="2">
            {doctitle}
          </td>
        </tr>
      </table>
      <hr/>
      <div>in {aref(urlFor(sym.owner), "_self", sym.owner.fullNameString('.'))}</div>
      <div class="entity">
        { Text(codeFor(kind)) }
        <span class="entity">{Text(sym.nameString)}</span>
      </div>
      <hr/>
      <dl>
        <dt>
          <code>{Text(Flags.flagsToString(sym.flags & ~Flags.TRAIT) + " class ")}</code>
          <em>{Text(sym.nameString)}</em>
        </dt>
        <dd>{
          if (sym.info.parents.isEmpty) NodeSeq.Empty
          else {
            val parent = sym.info.parents.head
            <code>{Text(" extends ")}</code> ++ (
            aref(urlFor(parent.symbol), contentFrame, parent.toString()))
          }
        }</dd>
        <dd>{comment(descr, true, kind)}</dd>
      </dl>
      <hr/> ++ ({
        val decls = sym.tpe.decls.toList
        //compute table members once for each relevant kind
        val tables = for (val k <- kinds.keys.toList)
                     yield (k, decls filter kinds(k))
        for (val (k, members) <- tables; !members.isEmpty) yield
          <table cellpadding="3" class="member" summary="">
            <tr>
              <td colspan="2" class="title">{k} Summary</td>
            </tr>
            { {
              for (val m <- members) yield
                <tr>
                  <td valign="top" class="modifiers">
                  </td>
                  <td class="signature"><code>def</code>
                    { Text(m.nameString + " ") ++ (m.tpe match {
                        case MethodType(typeParams, resultType) =>
                          (if (typeParams.isEmpty)
                             NodeSeq.Empty
                           else
                             Text("(") ++ typeParams.map(forType) ++ (")")
                          ) ++ (": ") ++ (forType(resultType))
                        case PolyType(typeParams, resultType) =>
                          val tp =
                            if (typeParams.isEmpty) ""
                            else (typeParams map (.defString)).mkString("[", ",", "]")
                          Text(tp + ": ") ++ forType(resultType)
                        case _ =>
                          Text(": ") ++ forType(m.tpe)
                      }) ++ (comments.get(m) match {
                        case Some(text) => comment(text, false, DEF)
                        case None => NodeSeq.Empty
                      })
                    }
                  </td>
                </tr>
            } }
          </table>
      }))
  }

  private val loader = getClass().getClassLoader()

  /** Map a class to it's known subclasses */
  private val subclasses = new HashMap[Symbol, List[Symbol]] {
    override def default(key: Symbol): List[Symbol] = {
      this += key -> (Nil: List[Symbol])
      Nil: List[Symbol]
    }
  }

  def process(units: Iterator[CompilationUnit]): Unit = {
    var members = emptyMap

    var topLevel = ListMap.empty[ModuleClassSymbol, ListMap[Kind,TreeSet[HasTree]]]
    for (val unit <- units) {
      val sourceMod = new SourceMod(unit)
      for (val mmbr <- sourceMod.members) mmbr.tree match {
        case cdef: ImplDef =>
          assert(cdef.symbol.owner != NoSymbol)
          val sym = cdef.symbol.owner.asInstanceOf[ModuleClassSymbol]
          if (!sym.isEmptyPackageClass) {
            if (!topLevel.contains(sym))
              topLevel = topLevel.update(sym, emptyMap)
            topLevel = topLevel.update(sym, organize0(mmbr, topLevel(sym)))
          }
          for (val p <- cdef.symbol.info.parents) {
            subclasses(p.symbol) = cdef.symbol :: subclasses(p.symbol)
          }
          import Flags._
          val mmbrs = cdef.symbol.info.findMember(nme.ANYNAME, MUTABLE | METHOD | BRIDGE | ACCESSOR, 0, false).alternatives
          for (val c <- mmbrs; c.isClass)
            for (val p <- c.info.parents) {
              subclasses(p.symbol) = c :: subclasses(p.symbol)
            }

        case _ =>
          error("unknown: " + mmbr.tree + " " + mmbr.tree.getClass())
      }
    }

    val modules0 = {
      var modules0 = new TreeMap[String, ModuleClassSymbol]
      for (val top <- topLevel.elements)
        modules0 = modules0.insert(top._1.fullNameString, top._1)
      modules0
    }

    new ListModuleFrame {
      def modules = modules0
    }
    new ListModuleContentFrame {
      def modules = modules0
    }

    new Frame {
      def title="navigation"
      def path="nav-classes"
      override def body0(hasBody: Boolean, nodes: NodeSeq): NodeSeq =
        if (!hasBody) nodes
        else <body style="margin:1px 0 0 1px; padding:1px 0 0 1px;">{nodes}</body>;
      def body =
        <form>
          <select id="kinds" onchange="gotoKind()">
            <option value="#Classes" selected="selected">Classes</option>
            <option value="#Objects">Objects</option>
            <!--<option value="#Traits">Traits</option>-->
          </select>
          <span id="alphabet" style="font-family:Courier;word-spacing:-8px;">
          <a href={Unparsed("javascript:gotoName('A')")}>A</a>
          <a href={Unparsed("javascript:gotoName('B')")}>B</a>
          <a href={Unparsed("javascript:gotoName('C')")}>C</a>
          <a href={Unparsed("javascript:gotoName('D')")}>D</a>
          <a href={Unparsed("javascript:gotoName('E')")}>E</a>
          <a href={Unparsed("javascript:gotoName('G')")}>G</a>
          <a href={Unparsed("javascript:gotoName('I')")}>I</a>
          <a href={Unparsed("javascript:gotoName('L')")}>L</a>
          <a href={Unparsed("javascript:gotoName('M')")}>M</a>
          <a href={Unparsed("javascript:gotoName('P')")}>P</a>
          <a href={Unparsed("javascript:gotoName('R')")}>R</a>
          <a href={Unparsed("javascript:gotoName('T')")}>T</a>
          <a href={Unparsed("javascript:gotoName('V')")}>V</a>
          <a href={Unparsed("javascript:gotoName('X')")}>X</a>
          </span>
        </form>
    }

    new ListClassFrame {
      def classes = {
        var allClasses = emptyMap
        for (val top <- topLevel.elements)
          allClasses = merge(allClasses, top._2)
        allClasses
      }
      def title = "List of all classes and objects"
      def path = "all-classes"
      def navLabel = "root-page"
    }

    // class from for each module.
    for (val top <- topLevel.elements) {
      val module = top._1
      val members = top._2

      new ListClassFrame {
        def title =
          "List of classes and objects in package " + module.fullNameString('.')
        def classes = top._2
        def path = module.fullNameString('/') + NAME_SUFFIX_PACKAGE
        def navLabel = module.fullNameString('.')
      }
      val module0 = module
      new ListClassContentFrame {
        def classes = top._2
        def module = module0
      }

      // do root frame for each class and object
      for (val kind <- members.elements) for (val mmbr <- kind._2.toList) {
        val kind0 = kind._1
        new ContentFrame {
          def title =
            labelFor(kind0) + " " + mmbr.tree.symbol.nameString +
            " in package " + mmbr.tree.symbol.owner.fullNameString('.')
          def clazz = mmbr.asInstanceOf[ImplMod]
          def kind = kind0
        }
      }

    }

    new Frame {
      def title = windowTitle
      def body = index
      def path = "index"
      override def hasBody = false
    }
    new PrimitiveContentFrame {
      def sym = definitions.AllClass // Nothing
      def descr = """
        /** <p>
         *    Class <code>Nothing</code> (previously named <code>All</code> in
         *    <a href="http://scala-lang.org" target="_top">Scala</a> 2.2.0 and
         *    older versions) is - together with class <a href="Null.html">
         *    <code>Null</code></a> - at the bottom of the
         *    <a href="http://scala-lang.org" target="_top">Scala</a> type
         *    hierarchy.
         *  </p>
         *  <p>
         *    Type <code>Nothing</code> is a subtype of every other type
         *    (including <a href="Null.html"><code>Null</code></a>); there
         *    exist <em>no instances</em> of this type. Even though type
         *    <code>Nothing</code> is empty, it is nevertheless useful as a
         *    type parameter. For instance, the <a href="http://scala-lang.org"
         *    target="_top">Scala</a> library defines a value
         *    <a href="Nil$object.html"><code>Nil</code></a> of type
         *    <code><a href="List.html">List</a>[Nothing]</code>. Because lists
         *    are covariant in <a href="http://scala-lang.org" target="_top">Scala</a>,
         *    this makes <a href="Nil$object.html"><code>Nil</code></a> an
         *    instance of <code><a href="List.html">List</a>[T]</code>, for
         *    any element type <code>T</code>.
         *  </p>
         */"""
    }
    new PrimitiveContentFrame {
      def sym = definitions.AllRefClass // Null
      def descr = """
        /** <p>
         *    Class <code>Null</code> (previously named <code>AllRef</code> in
         *    <a href="http://scala-lang.org" target="_top">Scala</a> 2.2.0 and
         *    older versions) is - together with class <a href="Nothing.html">
         *    <code>Nothing</code> - at the bottom of the
         *    <a href="http://scala-lang.org" target="_top">Scala</a> type
         *    hierarchy.
         *  </p>
         *  <p>
         *    Type <code>Null</code> is a subtype of all reference types; its
         *    only instance is the <code>null</code> reference.
         *    Since <code>Null</code> is not a subtype of value types,
         *    <code>null</code> is not a member of any such type. For instance,
         *    it is not possible to assign <code>null</code> to a variable of
         *    type <a href="Int.html"><code>Int</code></a>.
         * </p>
         */"""
    }
    new PrimitiveContentFrame {
      def sym = definitions.AnyClass
      def descr = """
        /** <p>
         *    Class <code>Any</code> is the root of the <a
         *    href="http://scala-lang.org/"
         *    target="_top">Scala</a> class hierarchy. Every class in a
         *    <a href="http://scala-lang.org/" target="_top">Scala</a> execution
         *    environment inherits directly or indirectly from this class.
         *    Class <code>Any</code> has two direct subclasses:
         *    <a href="AnyRef.html"><code>AnyRef</code></a> and
         *    <a href="AnyVal.html"><code>AnyVal</code></a>.
         *  </p>
         */"""
    }
    comments(definitions.Object_isInstanceOf) = """
      /** <p>
       *    The method <code>isInstanceOf</code> is the pendant of the Java
       *    operator <code>instanceof</code>.
       *  </p>
       *  <p>
       *    See also:
       *  </p>
       *  <ul>
       *    <li>
       *      Java Language Specification (2<sup>nd</sup> Ed.):
       *      <a href="http://java.sun.com/docs/books/jls/second_edition/html/expressions.doc.html#80289"
       *      target="_top">Operator <code>instanceof</code></a>.
       *    </li>
       *  </ul>
       */"""
    comments(definitions.Object_synchronized) = """
      /** <p>
       *    To make your programs thread-safe, you must first identify what
       *    data will be shared across threads. If you are writing data that
       *    may be read later by another thread, or reading data that may
       *    have been written by another thread, then that data is shared,
       *    and you must synchronize when accessing it.
       *  </p>
       *  <p>
       *    See also:
       *  <p>
       *  <ul>
       *    <li>
       *      The Java Tutorials:
       *      <a href="http://java.sun.com/docs/books/tutorial/essential/concurrency/sync.html"
       *      target="_top">Synchronization</a>.
       *    </li>
       *    <li>
       *      IBM developerWorks:
       *      <a href="http://www-128.ibm.com/developerworks/java/library/j-threads1.html"
       *      target="_top">Synchronization is not the enemy</a>.
       *    </li>
       *  </ul>
       */"""
    new PrimitiveContentFrame {
      def sym = definitions.AnyRefClass
      def descr = """
        /** <p>
         *    Class <code>AnyRef</code> is the root class of all
         *    <em>reference types</em>.
         *  </p>
         */"""
    }
    new PrimitiveContentFrame {
      def sym = definitions.AnyValClass
      def descr = """
        /** <p>
         *    Class <code>AnyVal</code> is the root class of all
         *    <em>value types</em>.
         *  </p>
         *  <p>
         *    <code>AnyVal</code> has a fixed number subclasses, which
         *    describe values which are not implemented as objects in the
         *    underlying host system.
         *  </p>
         *  <p>
         *    Classes <a href="Double.html"><code>Double</code></a>,
         *    <a href="Float.html"><code>Float</code></a>,
         *    <a href="Long.html"><code>Long</code></a>,
         *    <a href="Int.html"><code>Int</code></a>,
         *    <a href="Char.html"><code>Char</code></a>,
         *    <a href="Short.html"><code>Short</code></a>, and
         *    <a href="Byte.html"><code>Byte</code></a> are together called
         *    <em>numeric value types</em>.
         *    Classes <a href="Byte.html"><code>Byte</code></a>,
         *    <a href="Short.html"><code>Short</code></a>, or
         *    <a href="Char.html"><code>Char</code></a>
         *    are called <em>subrange types</em>. Subrange types, as well as
         *    <a href="Int.html"><code>Int</code></a> and
         *    <a href="Long.html"><code>Long</code></a> are called
         *    <em>integer types</em>, whereas
         *    <a href="Float.html"><code>Float</code></a> and
         *    <a href="Double.html"><code>Double</code></a> are called
         *    <em>floating point types</em>.
         *  </p>
         */"""
    }
    new PrimitiveContentFrame {
      def sym = definitions.BooleanClass
      def descr = """
        /** <p>
         *    Class <code>Boolean</code> has only two values: <code>true</code>
         *    and <code>false</code>.
         *  </p>
         */"""
    }
    def numericValDescr(sym: Symbol) = {
      val maxValue = "MAX_" + sym.name.toString().toUpperCase()
      val minValue = "MIN_" + sym.name.toString().toUpperCase();
      """
      /** <p>
       *    Class <code>""" + sym.name + """ </code> belongs to the value
       *    classes whose instances are not represented as objects by the
       *    underlying host system. All value classes inherit from class
       *    <a href="AnyVal.html"><code>AnyVal</code></a>.
       *  </p>
       *  <p>
       *    Values <code>""" + maxValue + """</code> and <code>""" + minValue + """</code>
       *    are in defined in object <a href="compat/Math$object.html">scala.compat.Math</a>.
       *  </p>
       */"""
    }
    new PrimitiveContentFrame {
      def sym = definitions.ByteClass
      def descr = numericValDescr(sym)
    }
    new PrimitiveContentFrame {
      def sym = definitions.CharClass
      def descr = numericValDescr(sym)
    }
    new PrimitiveContentFrame {
      def sym = definitions.DoubleClass
      def descr = numericValDescr(sym)
    }
    new PrimitiveContentFrame {
      def sym = definitions.FloatClass
      def descr = numericValDescr(sym)
    }
    new PrimitiveContentFrame {
      def sym = definitions.IntClass
      def descr = numericValDescr(sym)
    }
    new PrimitiveContentFrame {
      def sym = definitions.LongClass
      def descr = numericValDescr(sym)
    }
    new PrimitiveContentFrame {
      def sym = definitions.ShortClass
      def descr = numericValDescr(sym)
    }
    new PrimitiveContentFrame {
      def sym = definitions.UnitClass
      def descr = """
        /** <p>
         *    Class <code>Unit</code> has only one value: <code>()</code>.
         *  </p>
         */"""
    }
    def boxedValDescr(sym: Symbol) = """
      /** <p>
       *    Class <code>""" + sym.name + """</code> implements the
       *    boxing/unboxing from/to value types.
       *  </p>
       *  <p>
       *    Boxing and unboxing enable value types to be treated as objects;
       *    they provide a unified view of the type system wherein a value
       *    of any type can ultimately be treated as an object.
       *  </p>
       */"""
    new PrimitiveContentFrame {
      def sym = definitions.getClass("scala.runtime.BoxedFloat")
      def descr = boxedValDescr(sym)
    }
    new PrimitiveContentFrame {
      def sym = definitions.getClass("scala.runtime.BoxedInt")
      def descr = boxedValDescr(sym)
    }
    new PrimitiveContentFrame {
      def sym = definitions.getClass("scala.runtime.BoxedLong")
      def descr = boxedValDescr(sym)
    }
    new PrimitiveContentFrame {
      def sym = definitions.getClass("scala.runtime.BoxedNumber")
      def descr = boxedValDescr(sym)
    }
    val rsrcdir = "scala/tools/nsc/doc/".replace('/', File.separatorChar)
    for (val base <- List("style.css", "script.js")) {
      try {
        val in = loader.getResourceAsStream(rsrcdir + base)
        val out = new FileOutputStream(new File(outdir + File.separator + base))
        val buf = new Array[byte](1024)
        var len = 0
        while (len != -1) {
          out.write(buf, 0, len)
          len = in.read(buf)
        }
        in.close()
        out.close()
      } catch {
        case _ =>
          error("Resource file '" + base + "' not found")
      }
    }
  }

  def organize(c: Composite, map0: ListMap[Kind, TreeSet[HasTree]]) = {
    var map = map0
    //System.err.println("MemBERS: " + c.members.toList)
    for (val mmbr <- c.members.toList) map = organize0(mmbr, map)
    map
  }

  def organize0(mmbr: HasTree, map0: ListMap[Kind, TreeSet[HasTree]]) = {
    var map = map0
    assert(mmbr.kind ne null)
    if (!map.contains(mmbr.kind))
      map = map.update(mmbr.kind, new TreeSet[HasTree])
    val sz = map(mmbr.kind).size
    map = map.update(mmbr.kind, map(mmbr.kind) + mmbr)
    /*if (map(mmbr.kind).size == sz)
      System.err.println(""+mmbr + " not added");*/
    map
  }

  def parse(str: String): NodeSeq = {
    new SpecialNode {
      def label = "#PCDATA"
      def toString(sb: StringBuilder): StringBuilder = {
        sb.append(str.trim())
        sb
      }

    }
    /*
    import java.io.StringReader;
    import org.xml.sax.InputSource;
    val isrc1       = new InputSource(new StringReader(str));
    val parsedxml1  = XML.load(isrc1);
    if (parsedxml1 eq null) Text("BAD_COMMENT???");
    else parsedxml1;
    */
  }

  //http://java.sun.com/j2se/1.5.0/docs/tooldocs/windows/javadoc.html#javadoctags
  //http://java.sun.com/j2se/javadoc/writingdoccomments/
  //REMINDER: update file "src/manual/scala/man1/scaladoc.scala" accordingly!
  private def tag(name: String): NodeSeq =
    <b> {
      Text((name match {
        case "author"     => "Author"
        case "deprecated" => "Deprecated"
        case "exception"  => "Throws"
        case "param"      => "Parameters"
        case "pre"        => "Precondition"
        case "return"     => "Returns"
        case "see"        => "See Also"
        case "since"      => "Since"
        case "throws"     => "Throws"
        case "todo"       => "Todo"
        case "version"    => "Version"
        case _ => name
      }) + ":")
    } </b>

  // patterns for standard tags with 1 and 2 arguments
  private val pat1 = Pattern.compile(
    "[ \t]*@(author|deprecated|pre|return|see|since|todo|version)[ \t]*(.*)")
  private val pat2 = Pattern.compile(
    "[ \t]*@(exception|param|throws)[ \t]+(\\p{Graph}*)[ \t]*(.*)")

  // constructors have no description and get only attributes of the form 'pat2';
  // classes have a description and get only attributes of the form 'pat1';
  // no restriction applies to other kinds.
  private def comment(comment: String, isShort: Boolean, kind: Kind): NodeSeq = {
    var ret: List[Node] = Nil
    assert(comment ne null)
    // strip out any stars.
    var comment0 = comment.trim()
    assert(comment0 startsWith JDOC_START)
    comment0 = comment0.substring(JDOC_START.length())
    assert(comment0 endsWith JDOC_END)
    comment0 = comment0.substring(0, comment0.length() - JDOC_END.length())
    val buf = new StringBuilder
    type AttrDescr = (String, String, StringBuilder)
    val attributes = new ListBuffer[AttrDescr]
    var attr: AttrDescr = null
    val tok = new StringTokenizer(comment0, LINE_SEPARATOR)
    while (tok.hasMoreTokens) {
      val s = tok.nextToken.replaceFirst("\\p{Space}?\\*", "")
      val mat1 = pat1.matcher(s)
      if (mat1.matches) {
        attr = (mat1.group(1), null, new StringBuilder(mat1.group(2)))
        if (kind != CONSTRUCTOR) attributes += attr
      } else {
        val mat2 = pat2.matcher(s)
        if (mat2.matches) {
          attr = (mat2.group(1), mat2.group(2), new StringBuilder(mat2.group(3)))
          if (kind != CLASS) attributes += attr
        } else if (attr ne null)
          attr._3.append(s + LINE_SEPARATOR)
        else
          buf.append(s + LINE_SEPARATOR)
      }
    }
    val exceptions = new TreeMap[String, (Symbol, String)] +
      "Predef.IndexOutOfBoundsException" ->
        (definitions.PredefModule, "IndexOutOfBoundsException") +
      "Predef.NoSuchElementException" ->
        (definitions.PredefModule, "NoSuchElementException") +
      "Predef.NullPointerException" ->
        (definitions.PredefModule, "NullPointerException") +
      "Predef.UnsupportedOperationException" ->
        (definitions.PredefModule, "UnsupportedOperationException")
    val body = buf.toString
    if (isShort)
      <span>{parse(body)}</span>;
    else {
      val attrs = <dl>
    { {
      for (val attr <- attributes.toList) yield
        <dt style="margin:10px 0 0 20px;">
          {tag(attr._1)}
        </dt>
        <dd> {
          if (attr._2 eq null) NodeSeq.Empty
          else if (attr._1.equals("throws"))
            <code>{ exceptions.get(attr._2) match {
              case Some(p) =>
                val (sym, s) = p
                val path = "../" //todo: fix path
                val href = path + sym.fullNameString('/') +
                  (if (sym.isModule || sym.isModuleClass) NAME_SUFFIX_OBJECT else "") +
                  "#" + s
                <a href={href}>{attr._2}</a>
              case None => Text(attr._2)
            }}{Text(" - ")}</code>
          else
            <code>{attr._2 + " - "}</code>
        } {(parse(attr._3.toString))}
        </dd>;
    } } </dl>;
      if (kind == CONSTRUCTOR) attrs
      else <span><dl><dd>{parse(body)}</dd></dl>{attrs}</span>;
    }
  }

  val index =
    <frameset cols="25%, 75%">
    <frameset rows="50%, 28, 50%">
      <frame src="modules.html" name={modulesFrame}></frame>
      <frame src="nav-classes.html" name="navigationFrame"></frame>
      <frame src="all-classes.html" name={classesFrame}></frame>
    </frameset>
    <frame src="root-content.html" name={contentFrame}></frame>
  </frameset>;

  val root = <b></b>

  private val NAME_SUFFIX_OBJECT  = "$object"
  private val NAME_SUFFIX_PACKAGE = "$package"
  private val FILE_EXTENSION_HTML = ".html"

  private val JDOC_START = "/**"
  private val JDOC_END   = "*/"
}
