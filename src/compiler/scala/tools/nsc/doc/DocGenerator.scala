package scala.tools.nsc.doc;

import scala.tools.nsc._;
import java.io.File;
import scala.tools.nsc.models._;
import scala.collection.immutable._;
import scala.xml._;

abstract class DocGenerator extends Models {
  import global._;

  def dquote(str : String) = Text("\"" + str + "\"");
  val header = <meta http-equiv="content-type" content="text/html; charset=iso-8859-1"/>
               <meta name="generator" content="scaladoc (1.4.0.4)"/>
               <link rel="stylesheet" type="text/css" href="style.css"/>
               <script type="text/javascript" src="script.js"></script>;


  def page = <HTML></HTML>;


  val emptyMap = ListMap.Empty[Kind,TreeSet[HasTree]];
  def process(units : Iterator[CompilationUnit], outdir : String) : Unit = {
    val outdir0 = new File(outdir);
    if (!outdir0.exists()) outdir0.mkdir();
    var members = emptyMap;
    var topLevel = ListMap.Empty[ModuleSymbol,ListMap[Kind,TreeSet[HasTree]]];
    for (val unit <- units) {
      val sourceMod = new SourceMod(unit);
      for (val mmbr <- sourceMod.members) mmbr.tree match {
      case cdef:  ImplDef =>
        assert(cdef.symbol.owner != NoSymbol);
        val sym = cdef.symbol.owner.asInstanceOf[ModuleSymbol];
        if (!topLevel.contains(sym)) topLevel = topLevel.update(sym, emptyMap);
        topLevel = topLevel.update(sym, organize0(mmbr, topLevel(sym)));
      case _ => throw new Error("unknown: " + mmbr.tree + " " + mmbr.tree.getClass());
      }
    }
    var packages = new TreeMap[String,ModuleSymbol];
    for (val top <- topLevel.elements)
      packages = packages.insert(top._1.fullNameString, top._1);


    val packageFrame = <HTML>
      <HEAD><TITLE>List of modules</TITLE>
            {header}
      </HEAD>
      <body>
      <div class="doctitle-larger">
        Scala<br/>2.0
      </div>
      <a href="module-page.html" target="classesFrame">All objects and classes</a><p/>
      <b>Modules</b>
      <table class="list">
      <tr>
      <td style="white-space:nowrap;">
      { {
       for (val top <- packages.elements)
         yield urlFor0(top._2, "classesFrame");
      } }
      </td></tr></table></body></HTML>;


    System.out.println("PACKAGE_FRAME");
    System.out.println(packageFrame);


    // output HTML for each package.
    for (val top <- topLevel.elements) {
      val sym = top._1;
      val members = top._2;
      val index = {process(members, sym)};


      System.out.println("SAVE-TO: " + sym);
      System.out.println(index);
    }
  }

  def nameFor0(sym : Symbol) : NodeSeq = Text(sym.fullNameString);
  def urlFor0(sym : Symbol, target : String) : NodeSeq =
		<a href={urlFor1(sym)} target={("\"" + target + "\"")}>{nameFor0(sym)}</a><br/>;

  def urlFor2(sym : Symbol) : String = {
    if (sym == sym.toplevelClass) "";
    else {
      val rest = urlFor2(sym.owner);
      val rest0 = if (rest.equals("")) rest else rest + ".";
      rest0 + sym.nameString;
    }
  }

  def urlFor1(sym : Symbol) : String = sym match {
    case _ : ModuleSymbol =>
      ("\"" + sym.fullNameString('/') + "/module-page.html\"");
    case csym : ClassSymbol =>
      if (csym == csym.toplevelClass)
         "\"" + csym.fullNameString('/') + "-class-page.html\"";
      else    urlFor1(sym.toplevelClass) + "#" + urlFor2(sym);
    case _ => urlFor1(sym.toplevelClass) + "#" + urlFor2(sym);
  }





  def process(members : ListMap[Kind,TreeSet[HasTree]], sym : Symbol) : NodeSeq = {
    for (val kind <- KINDS; members.contains(kind)) yield process(kind, members(kind), sym);



    urlFor0(sym, "classesFrame");

  };

  def organize(c : Composite, map0 : ListMap[Kind,TreeSet[HasTree]]) = {
    var map = map0;
    for (val mmbr <- c.members.toList) map = organize0(mmbr, map);
    map;
  }
  def organize0(mmbr : HasTree, map0 : ListMap[Kind,TreeSet[HasTree]]) = {
    var map = map0;
    if (!map.contains(mmbr.kind))
      map = map.update(mmbr.kind, new TreeSet[HasTree]);
    map = map.update(mmbr.kind, map(mmbr.kind) + mmbr);
    map;
  }


  def process(kind : Kind, set : TreeSet[HasTree], sym : Symbol) : Node = {
    val ret =
    <table cellpadding="3" class="member">
    <tr>
      <td colspan="2" class="title">
      { { labelFor(kind) + " Summary"; } }
      </td>
    </tr>
    { { for (val mmbr <- set.toList) yield process(mmbr, sym); } }
    </table>;
    ret;
  }
  def stringsFor(mods : Modifiers) = {
    var modString : List[String] = Nil;
    if (mods . isPrivate  ) modString = "private"   :: modString;
    if (mods . isProtected) modString = "protected" :: modString;
    if (mods . isOverride ) modString = "override"  :: modString;
    if (mods . isAbstract ) modString = "abstract"  :: modString;
    if (mods . isCase     ) modString = "case"      :: modString;
    if (mods . isSealed   ) modString = "sealed"    :: modString;
    if (mods . isFinal    ) modString = "final"    :: modString;
    if (mods . isMixin    ) modString = "mixin"    :: modString;
    modString;
  }


  def targetFor(ht : HasTree) : String = {
    // compute a URL.
    "Foo";
  }
  def forSymbol(symbol : Symbol, tpe : Type) : NodeSeq =
    Text(symbol.nameString);


  def ifT (cond : Boolean, nodes : NodeSeq) = if (cond) nodes else NodeSeq.Empty;
  def ifT (tree : Tree, nodes : NodeSeq, before : Boolean) =
    if (tree != EmptyTree) {
      if (before) nodes.concat(forTree(tree));
      else forTree(tree).concat(nodes);
    } else NodeSeq.Empty;

  def forTree(tree : Tree) : NodeSeq = tree match {
    case vdef : ValDef =>
      Text(vdef.symbol.name.toString()).concat(Text(" : ")).concat(forTree(vdef.tpt));
    case id  : Ident  => forSymbol(id.symbol, id.tpe);
    case sel : Select => forTree(sel.qualifier).concat(forSymbol(sel.symbol, sel.tpe));
    case tree : AbsTypeDef =>
      ifT(tree.lo, Text(" <: "), true).
        concat(forSymbol(tree.symbol, tree.tpe)).concat(ifT(tree.hi, Text(" <: "), false));
    case EmptyTree => NodeSeq.Empty;
    case _ => Text("XX=" + tree.toString());
  }
  def forTrees(trees : List[Tree]) : NodeSeq = {
    if (trees.isEmpty) NodeSeq.Empty;
    else {
      val head = forTree(trees.head);
      head.concat(if (trees.tail.isEmpty) NodeSeq.Empty else Text(", ")).concat(forTrees(trees.tail));
    }
  }

  def surround(open : String, close : String, node : NodeSeq) : NodeSeq =
    Text(open).concat(node).concat(Text(close));

  def typesFor(ht : HasTree) : NodeSeq = {
    val tparams = ht.tree match {
      case cdef : ClassDef => cdef.tparams;
      case ddef : DefDef   => ddef.tparams;
      case adef : AliasTypeDef => adef.tparams;
      case _ => Nil;
    }
    if (tparams.isEmpty) Text("");
    else surround("[", "]", forTrees(tparams));
  }
  def  argsFor(ht : HasTree) : NodeSeq = ht.tree match {
    case ddef : DefDef =>
      val nodes = for (val vparams <- ddef.vparamss)
        yield surround("(", ")", forTrees(vparams));
      nodes.flatMap(x => x.asList);
    case _ => NodeSeq.Empty;
  }
  def urlFor(ht : HasTree) : String =
    "\"unknown\"";

  def  nameFor(ht : HasTree) : NodeSeq =
    <A href={ urlFor(ht) } target="_self"> { forSymbol(ht.tree.symbol, null) } </A>;

  def resultFor(ht : HasTree) : NodeSeq = ht.tree match {
    case vdef : ValOrDefDef =>
      Text(" : ").concat(forTree(vdef.tpt));
    case cdef : ImplDef =>
      if (cdef.impl.parents.isEmpty) NodeSeq.Empty;
      else Text("extends ").concat(forTrees(cdef.impl.parents));
    case _ => NodeSeq.Empty;
  }

  def comment(comment : String, isShort : Boolean) : NodeSeq = {
    var ret : List[Node] = Nil;
    if (comment != null) {
      // strip out any stars.
      var comment0 = comment.trim();
      assert(comment0.startsWith(JDOC_START));
      comment0 = comment0.substring(JDOC_START.length());
      assert(comment0.endsWith(JDOC_END));
      comment0 = comment0.substring(0, comment0.length() - JDOC_END.length());
      var idx = 0;
      while (idx != -1) {
        idx = comment0.indexOf('*', idx);
        if (idx != -1)
          comment0 = comment0.substring(0, idx) +
            comment0.substring(idx + 1, comment0.length());
      }
      val tokenizer = new java.util.StringTokenizer(comment0, "@");
      val body = tokenizer.nextToken();

      var attributes : List[String] = Nil;
      if (!isShort) while (tokenizer.hasMoreElements())
        attributes = attributes ::: (tokenizer.nextToken() :: Nil);
      val node = <BR> { Text(comment) } </BR>;
      val nodes = { { for (val a <- attributes) yield <BR> { Text(a) } </BR>; } };
      val nodes0 : NodeSeq = node :: nodes;
      nodes0;
    } else NodeSeq.Empty;
  };


  def process(ht : HasTree, sym : Symbol) : scala.xml.Node = {
    val comment0 = if (comments.contains(ht.tree.symbol))
      comments(ht.tree.symbol) else null;


    val index =
    <tr>
      <td valign="top" class="modifiers">
        &nbsp;
        <code>{ {
          (for (val mod <- stringsFor(ht.mods)) yield Text(mod + " "));
        } }</code>
      </td>
      <td class="signature">
        <code>
        { labelFor(ht.kind).toLowerCase() }
        { nameFor(ht) }
        { typesFor(ht) }
        {  argsFor(ht) }
        {resultFor(ht) }
        </code>
        { comment(comment0, true) }
      </td>
    </tr>;

    val body = <X></X>;






    index;
  }


/*
  val index0 = <HTML>
  <HEAD><TITLE>API Documentation</TITLE></HEAD>
  <FRAMESET cols="20%,80%" title="" onLoad="top.loadFrames()">
  <FRAMESET rows="30%,70%" title="" onLoad="top.loadFrames()">
  <FRAME src="overview-frame.html" name="packageListFrame" title="All Packages">
  <FRAME src="allclasses-frame.html" name="packageFrame" title="All classes and interfaces (except non-static nested types)">
  </FRAMESET>
  <FRAME src="overview-summary.html" name="classFrame" title="Package, class and interface descriptions" scrolling="yes">
  </FRAMESET>
  </HTML>;
*/

  def index = {
    val index0 = <HTML></HTML>;
  }

  private val JDOC_START = "/**";
  private val JDOC_END   = "*/";
}
