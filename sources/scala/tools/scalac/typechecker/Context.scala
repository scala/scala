/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**
** $Id$
\*                                                                      */
import scalac.symtab._;
import scalac.ast._;
import scalac.util.Names;
import scalac.util.Name;

package scala.tools.scalac.typechecker {

object Context {
  val NONE = new Context();
  NONE.viewCache = List();
}

class Context {

  import Kinds._;

  var tree: Tree = _;                 // Tree associated with this context
  var owner: Symbol = _;              // The current owner
  var scope: Scope = _;               // The current scope
  var outer: Context = _;             // The next outer context
  var enclClass: Context = this;      // The next outer context whose tree
                                      // is a class template
  var prevImport: Context = this;     // the next outer import context
  var variance: int = _;              // Variance relative to enclosing class.
  var constructorClass: Symbol = _;   // Class for auxiliary constructor
  var viewCache: List[View] = null;   // View symbols visible in scope
  var infer: Infer = null;            // Type inferencer
  var depth: int = 0;

  def this(tree: Tree, owner: Symbol, scope: Scope, outer: Context) = {
    this();
    this.tree = tree;
    this.owner = owner;
    this.scope = scope;
    this.enclClass = if ((tree.isInstanceOf[Tree$Template] ||
			  tree.isInstanceOf[Tree$CompoundType]) &&
		         tree != outer.tree) this
		     else outer.enclClass;
    this.prevImport = if (isImportContext && tree != outer.tree) this
                      else outer.prevImport;
    this.variance = outer.variance;
    this.constructorClass = outer.constructorClass;
    this.infer = outer.infer;
    this.depth = outer.depth + 1;
    this.outer = outer;
  }

  def this(tree: Tree, outer: Context) =
    this(tree, outer.owner, outer.scope, outer);

  def outerContext(clazz: Symbol): Context = {
    var c = this;
    while (c != Context.NONE && c.owner != clazz) c = c.outer.enclClass;
    c
  }

  def isTopLevel(): boolean = tree match {
    case Tree$Block(_,_) =>
      false
    case Tree$Template(_, _) =>
      outer.tree.isInstanceOf[Tree$PackageDef]
    case Tree.Empty =>
      true
    case _ =>
      outer.isTopLevel()
  }

  def isImportContext: boolean = tree.isInstanceOf[Tree$Import];

  def importString(): String =
    if (prevImport == Context.NONE) ""
    else
      prevImport.outer.importString() +
      prevImport.tree.symbol().toString() + ";";

  override def toString(): String = {
    if (this == Context.NONE) "Context.NONE";
    else tree.toString() + "\n:: " + outer.toString()
  }

  def importPrefix(): Tree = tree match {
    case Tree$Import(expr, _) =>  expr
  }

  def importType(): Type =
    tree.symbol().getType();

  def sameImport(that: Context): boolean =
    this.importType().isSameAs(that.importType());

  def importedSymbol(name: Name): Symbol = {
    return TreeInfo.importedSymbol(tree, name);
  }

  def viewMeths: List[View] = {

    def addView(sym: Symbol, symtype: Type, qual: Tree): unit = symtype match {
      case Type$OverloadedType(alts, alttypes) =>
	var i = alts.length - 1;
	while (i >= 0) {
	  addView(alts(i), alttypes(i), qual);
	  i = i - 1;
	}
      case _ =>
	/*
	def isUnShadowed(view: View) =
	  view.context == this || !infer.specializes(view.symtype, symtype);
        */
	if (viewCache.forall(v => v.sym != sym)) {
	  val v = View(sym, symtype, qual, this);
	  //System.out.println("VIEW " + sym + ":" + symtype + " " + qual);//DEBUG
	  viewCache = v :: viewCache;//.filter(isUnShadowed);
	}
    }

    if (viewCache == null) {
      viewCache = outer.viewMeths;
      if (scope != outer.scope) {
        val e = scope.lookupEntry(Names.view);
        if (e.owner == scope && e.sym.kind == VAL)
	  addView(e.sym, e.sym.getType(), Tree.Empty);
      }
      if (prevImport == this) {
        val sym = importedSymbol(Names.view);
	if (sym.kind == VAL)
	  addView(sym, importType().memberType(sym), importPrefix());
      }
    }
    viewCache
  }
}
}
