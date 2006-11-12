/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Sean McDirmid
 */
// $Id$

package scala.tools.nsc.doc

import java.io.{File, FileOutputStream, FileWriter}
import java.util.StringTokenizer
import java.util.regex.Pattern

import compat.Platform.{EOL => LINE_SEPARATOR}
import scala.collection.immutable.{ListMap, TreeMap, TreeSet}
import scala.collection.mutable.{HashMap, ListBuffer, Map}
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
  import compat.StringBuilder


  def outdir: String
  def windowTitle: String
  def documentTitle: String
  def contentFrame = "contentFrame"
  def classesFrame = "classesFrame"
  def modulesFrame = "modulesFrame"
  def emptyMap = ListMap.Empty[Kind, TreeSet[HasTree]]

  override def acceptPrivate = false

  abstract class Frame extends UrlContext {
    def path: String // relative to outdir
    def relative: String = {
      assert(path != null)
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
      else if (sym.sourceFile == null)
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
      if (tpe.symbol.hasFlag(Flags.JAVA) || tpe.symbol.sourceFile == null)
        <a class={tpe.toString().replace('.', '_')} href=""
          target={target}>{tpe.toString()}</a>
      /*
      else if (tpe.symbol.sourceFile == null)
        Text(tpe.toString())
      */
      else {
        val n = tpe.typeArgs.length
        if (n > 0) {
          if (definitions.isFunctionType(tpe)) {
            val Pair(ts, List(r)) = tpe.typeArgs.splitAt(n-1)
            Text("(")
              .concat(
                if (ts.isEmpty) NodeSeq.Empty
                else
                  urlFor(ts.head, target).concat(
                    for (val t <- ts.tail)
                    yield Group(Text(", ").concat(urlFor(t, target)))))
              .concat(Text(") => "))
              .concat(urlFor(r, target))
          } else
            aref(urlFor(tpe.symbol), target, tpe.symbol.fullNameString)
              .concat(Text("[")
              .concat(urlFor(tpe.typeArgs.head, target))
              .concat(
                for (val t <- tpe.typeArgs.tail)
                yield Group(Text(", ").concat(urlFor(t, target))))
              .concat(Text("]")))
        } else
          aref(urlFor(tpe.symbol), target, tpe.toString())
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

    def urlFor(sym: Symbol): String = sym match {
      case msym: ModuleSymbol => urlFor0(sym, sym) + FILE_EXTENSION_HTML
      case csym: ClassSymbol =>  urlFor0(sym, sym) + FILE_EXTENSION_HTML
      case _ => urlFor(sym.owner) + "#" + Utility.escape(sym.nameString)
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
    def body: NodeSeq = {
      val x = doctitle concat
        aref("all-classes.html", classesFrame, "All objects and classes")
      val y = <p/><b>Packages</b>
        <table class="list" summary="">
          <tr><td style="white-space:nowrap;">
          { {
            for (val top <- modules.elements.toList) yield
              {br(aref(urlFor(top._2), classesFrame, top._2.fullNameString('.')))};
          } }
          </td></tr>
        </table>;
      x.concat(y)
    }
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

      val body = <div>{ { for (val kind <- KINDS; classes contains kind) yield {
        <p>
          <b>{Text(pluralFor(kind))}</b>
        </p>
        <table class="list" summary="">
          <tr><td style="white-space;nowrap;">
            { {
              for (val mmbr <- classes(kind).toList) yield
                br(urlFor(mmbr.tree, contentFrame));
            } }
          </td></tr>
        </table>
      } } }</div>;

      nav.concat(body)
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
          </dd>.concat(
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
        def attrFor(attr: AttrInfo): Node = {
          val buf = new StringBuilder
          val Triple(tpe, args, nvPairs) = attr
          val name = aref(urlFor(tpe.symbol), contentFrame, tpe.toString)
          if (!args.isEmpty)
            buf.append(args.map(.escapedStringValue).mkString("(", ",", ")"))
          if (!nvPairs.isEmpty)
            for (val Pair(Pair(name, value), index) <- nvPairs.zipWithIndex) {
              if (index > 0)
                buf.append(", ")
              buf.append(name).append(" = ").append(value)
            }
          Group(name concat Text(buf.toString))
        }
        var res: NodeSeq = Text("[")
        val attrs = tree.symbol.attributes
        for (val i <- attrs.indices) {
          if (i > 0) res = res.concat(Text("," + LINE_SEPARATOR))
          res = res.concat(attrFor(attrs(i)))
        }
        br(res.concat(Text("]")))
      }

    /**
     *  @param mmbr ...
     *  @return     ...
     */
    def fullHeader(mmbr: HasTree): NodeSeq = Group(
      { {
        if (mmbr.isInstanceOf[ImplMod]) NodeSeq.Empty
        else <a name = {Utility.escape(mmbr.tree.symbol.nameString)}></a>
      } }.concat(
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
      </dl>)
        .concat(fullComment(mmbr))
        .concat(hr(listSubclasses(mmbr)))
        .concat(lists(mmbr)))
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
            links.reduceRight { (link: Seq[Node], seq: Seq[Node]) => link.concat(Text(", ")).concat(seq) }
          } }</dd>
        </dl>;
    }

    def lists(mmbr: HasTree): NodeSeq = mmbr match {
      case cmod: ImplMod =>
        <span>
          { listMembersShort(mmbr) }
          { listInheritedMembers(mmbr) }
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
        for (val kind <- KINDS; map contains kind) yield Group(br(
          <table cellpadding="3" class="member" summary="">
            <tr>
              <td colspan="2" class="title">{Text(labelFor(kind))} Summary</td>
            </tr>
            { {
              for (val mmbr <- map(kind).toList) yield
                shortHeader(mmbr)
            } }
          </table>))
      } else
        NodeSeq.Empty

    /**
     *  @param mmbr ...
     *  @return     a sequence of HTML tables containing inherited members
     */
    def listInheritedMembers(mmbr: HasTree): NodeSeq =
      if (mmbr.isInstanceOf[Composite]) {
        val sym = mmbr.tree.symbol
        val ignored = List(definitions.ObjectClass, definitions.ScalaObjectClass)
        val parents = sym.info.parents
        for (val p <- parents; !ignored.contains(p.symbol)) yield Group(br(
          <table cellpadding="3" class="inherited" summary="">
            <tr>
              <td colspan="2" class="title">
                {Text("Methods inherited from ").concat(urlFor(p, contentFrame))}
              </td>
            </tr>
            <tr>
            { {
              val decls = p.decls.toList filter(d =>
                d.isMethod && !d.isConstructor)
              if (decls.isEmpty) NodeSeq.Empty // scope empty
              else {
                def aref1(sym: Symbol): NodeSeq = {
                  val isJava = sym hasFlag Flags.JAVA
                  if (isJava || sym.sourceFile == null) {
                    val name = sym.nameString
                    val args =
                      if (isJava) "()" // todo: arguments
                      else ""
                    <a class={sym.owner.fullNameString.replace('.', '_')}
                       href={"#" + name + args}
                       target={contentFrame}>{name}</a>
                  }
                  else
                    aref(urlFor(sym), contentFrame, sym.nameString)
                }
                val members = decls.sort(
                  (x, y) => (x.nameString compareTo y.nameString) < 0)
                <td colspan="2" class="signature">
                  {aref1(members.head)}
                  {for (val m <- members.tail) yield Text(", ").concat(aref1(m))}
                </td>
              }
            } }
            </tr>
          </table>))
      } else
        NodeSeq.Empty

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
          { { for (val str <- stringsFor(mmbr.mods)) yield <code>{(Text(str + " "))}</code>; } }
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

    def fullComment(mmbr: HasTree): NodeSeq =
      comments.get(mmbr.tree.symbol) match {
        case Some(text) => comment(text, false)
        case None => NodeSeq.Empty
      }

    def shortComment(mmbr: HasTree): NodeSeq =
      comments.get(mmbr.tree.symbol) match {
        case Some(text) => comment(text, true)
        case None => NodeSeq.Empty
      }

    def ifT(cond: Boolean, nodes: NodeSeq) =
      if (cond) nodes else NodeSeq.Empty

    def ifT(tree: Tree, nodes: NodeSeq, before: Boolean) =
      if (tree != EmptyTree &&
          tree.tpe.symbol != definitions.AnyClass &&
          tree.tpe.symbol != definitions.AllClass) {
        if (before) nodes.concat(forTree(tree))
        else {
          val ret = forTree(tree).concat(nodes)
          //System.err.println("RET: " + ret)
          ret
        }
      } else NodeSeq.Empty

    def forType(tpe: Type): NodeSeq =
      urlFor(tpe, contentFrame)

    def forTree(tree: Tree): NodeSeq = tree match {
      case vdef: ValDef =>
        Text(vdef.symbol.name.toString()).concat(Text(": ")).concat(forTree(vdef.tpt))
      case sel: Select =>
        forTree(sel.qualifier).concat(Text(sel.symbol.nameString))
      case tree: AbsTypeDef =>
        Text(tree.symbol.nameString)
          .concat(ifT(tree.hi, Text(" <: "), true))
          .concat(ifT(tree.lo, Text(" >: "), true))
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
        head.concat(if (trees.tail.isEmpty) NodeSeq.Empty
                    else Text(", ")).concat(forTrees(trees.tail))
      }

    private def surround(open: String, close: String, node: NodeSeq): NodeSeq =
      Text(open).concat(node).concat(Text(close))

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
          Text(": ").concat(forTree(vdef.tpt))
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
      </p>.concat(
        for (val kind <- KINDS; classes contains kind) yield Group(hr(
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
            <code>{Text(" extends ")}</code>.concat(
            aref(urlFor(parent.symbol), contentFrame, parent.toString()))
          }
        }</dd>
        <dd>{comment(descr, true)}</dd>
      </dl>
      <hr/>.concat({
        val decls = sym.tpe.decls.toList
        //compute table members once for each relevant kind
        val tables = for (val k <- kinds.keys.toList)
                     yield Pair(k, decls filter kinds(k))
        for (val Pair(k, members) <- tables; !members.isEmpty) yield
          <table cellpadding="3" class="member" summary="" style="margin:0 0 1.2em 0;">
            <tr>
              <td colspan="2" class="title">{k} Summary</td>
            </tr>
            { {
              for (val m <- members) yield
                <tr>
                  <td valign="top" class="modifiers">
                  </td>
                  <td class="signature"><code>def</code>
                    { Text(m.nameString + " ").concat(m.tpe match {
                        case MethodType(typeParams, resultType) =>
                          (if (typeParams.isEmpty)
                             NodeSeq.Empty
                           else
                             Text("(").concat(typeParams.map(p => forType(p))).concat(")")
                          ).concat(": ").concat(forType(resultType))
                        case PolyType(typeParams, resultType) =>
                          val tp =
                            if (typeParams.isEmpty) ""
                            else (typeParams map (.defString)).mkString("[", ",", "]")
                          Text(tp + ": ").concat(forType(resultType))
                        case _ =>
                          Text(": ").concat(forType(m.tpe))
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

    var topLevel = ListMap.Empty[ModuleClassSymbol, ListMap[Kind,TreeSet[HasTree]]]
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
         *    <a href="http://scala.epfl.ch" target="_top">Scala</a> 2.2.0 and
         *    older versions) is - together with class <a href="Null.html">
         *    <code>Null</code></a> - at the bottom of the
         *    <a href="http://scala.epfl.ch" target="_top">Scala</a> type
         *    hierarchy.
         *  </p>
         *  <p>
         *    Type <code>Nothing</code> is a subtype of every other type
         *    (including <a href="Null.html"><code>Null</code></a>); there
         *    exist <em>no instances</em> of this type. Even though type
         *    <code>Nothing</code> is empty, it is nevertheless useful as a
         *    type parameter. For instance, the <a href="http://scala.epfl.ch"
         *    target="_top">Scala</a> library defines a value
         *    <a href="Nil$object.html"><code>Nil</code></a> of type
         *    <code><a href="List.html">List</a>[Nothing]</code>. Because lists
         *    are covariant in <a href="http://scala.epfl.ch" target="_top">Scala</a>,
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
         *    <a href="http://scala.epfl.ch" target="_top">Scala</a> 2.2.0 and
         *    older versions) is - together with class <a href="Nothing.html">
         *    <code>Nothing</code> - at the bottom of the
         *    <a href="http://scala.epfl.ch" target="_top">Scala</a> type
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
         *    Class <code>Any</code> is the root of the <a href="http://scala.epfl.ch/"
         *    target="_top">Scala</a> class hierarchy. Every class in a
         *    <a href="http://scala.epfl.ch/" target="_top">Scala</a> execution
         *    environment inherits directly or indirectly from this class.
         *    Class <code>Any</code> has two direct subclasses:
         *    <a href="AnyRef.html"><code>AnyRef</code></a> and
         *    <a href="AnyVal.html"><code>AnyVal</code></a>.
         *  </p>
         */"""
    }
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
    def numericValDescr(sym: Symbol) = """
      /** <p>
       *    Class <code>""" + sym.name + """ </code> belongs to the value
       *    classes whose instances are not represented as objects by the
       *    underlying host system. All value classes inherit from class
       *    <a href="AnyVal.html"><code>AnyVal</code></a>.
       *  </p>
       */"""
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
    assert(mmbr.kind != null)
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
    if (parsedxml1 == null) Text("BAD_COMMENT???");
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
    "[ \t]*@(author|deprecated|return|see|since|todo|version)[ \t]*(.*)")
  private val pat2 = Pattern.compile(
    "[ \t]*@(exception|param|throws)[ \t]+(\\p{Graph}*)[ \t]*(.*)")

  private def comment(comment: String, isShort: Boolean): NodeSeq = {
    var ret: List[Node] = Nil
    assert(comment != null)
    // strip out any stars.
    var comment0 = comment.trim()
    assert(comment0 startsWith JDOC_START)
    comment0 = comment0.substring(JDOC_START.length())
    assert(comment0 endsWith JDOC_END)
    comment0 = comment0.substring(0, comment0.length() - JDOC_END.length())
    val buf = new StringBuilder
    type AttrDescr = Triple[String, String, StringBuilder]
    val attributes = new ListBuffer[AttrDescr]
    var attr: AttrDescr = null
    val tok = new StringTokenizer(comment0, LINE_SEPARATOR)
    while (tok.hasMoreTokens) {
      val s = tok.nextToken.replaceFirst("\\p{Space}?\\*", "")
      val mat1 = pat1.matcher(s)
      if (mat1.matches) {
        attr = Triple(mat1.group(1), null, new StringBuilder(mat1.group(2)))
        attributes += attr
      } else {
        val mat2 = pat2.matcher(s)
        if (mat2.matches) {
          attr = Triple(mat2.group(1), mat2.group(2), new StringBuilder(mat2.group(3)))
          attributes += attr
        } else if (attr != null)
          attr._3.append(s + LINE_SEPARATOR)
        else
          buf.append(s + LINE_SEPARATOR)
      }
    }
    val exceptions = new TreeMap[String, Pair[Symbol, String]] +
      "Predef.IndexOutOfBoundsException" ->
        Pair(definitions.PredefModule, "IndexOutOfBoundsException") +
      "Predef.NoSuchElementException" ->
        Pair(definitions.PredefModule, "NoSuchElementException") +
      "Predef.NullPointerException" ->
        Pair(definitions.PredefModule, "NullPointerException") +
      "Predef.UnsupportedOperationException" ->
        Pair(definitions.PredefModule, "UnsupportedOperationException")
    val body = buf.toString
    if (isShort) <span>{parse(body)}</span>;
    else <span><dl><dd>{parse(body)}</dd></dl><dl>
    { {
      for (val attr <- attributes.toList) yield
        <dt style="margin:10px 0 0 20px;">
          {tag(attr._1)}
        </dt>
        <dd> {
          if (attr._2 == null) NodeSeq.Empty
          else if (attr._1.equals("throws"))
            <code>{ exceptions.get(attr._2) match {
              case Some(p) =>
                val Pair(sym, s) = p
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
    } } </dl></span>;
  }

  val index =
    <frameset cols="25%, 75%">
    <frameset rows="50%, 50%">
      <frame src="modules.html" name={modulesFrame}></frame>
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
