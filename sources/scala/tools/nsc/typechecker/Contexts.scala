/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import scala.tools.util.Position;

class Contexts: Analyzer {
  import global._;

  val NoContext = new Context {
    override def imports: List[ImportInfo] = List();
  }

  val startContext = {
    import definitions._;
    var sc = NoContext.make(
      Template(List(), List()) setSymbol NoSymbol setType NoType,
      definitions.RootClass,
      definitions.RootClass.info.decls);
    def addImport(pkg: Symbol): unit = {
      val qual = gen.mkStableRef(pkg);
      val impTree = Import(qual, List(Pair(nme.WILDCARD, null)))
        setSymbol NoSymbol.newImport(Position.NOPOS).setInfo(ImportType(qual))
        setType NoType;
      sc = sc.make(
	Template(List(), List(impTree)) setSymbol NoSymbol setType NoType, sc.owner, sc.scope)
    }
    if (!settings.noimports.value) {
      addImport(JavaLangPackage);
      addImport(ScalaPackage);
      if (!settings.nopredefs.value)
	addImport(PredefModule);
    }
    sc
  }

  class Context {
    var unit: CompilationUnit = _;
    var tree: Tree = _;                     // Tree associated with this context
    var owner: Symbol = _;                  // The current owner
    var scope: Scope = _;                   // The current scope
    var outer: Context = _;                 // The next outer context
    var enclClass: Context = _;             // The next outer context whose tree is a
                                            // template or package definition
    var variance: int = _;                  // Variance relative to enclosing class.
    private var _undetparams: List[Symbol] = List(); // Undetermined type parameters
    var depth: int = 0;

    def undetparams = _undetparams;
    def undetparams_=(ps: List[Symbol]) = {
      System.out.println("undetparams = " + ps);
      _undetparams = ps
    }

    def make(unit: CompilationUnit, tree: Tree, owner: Symbol, scope: Scope): Context = {
      val c = new Context();
      c.unit = unit;
      c.tree = tree;
      c.owner = owner;
      c.scope = scope;
      c.enclClass = tree match {
        case Template(_, _) | PackageDef(_, _) => c
        case _ => this.enclClass
      }
      c.variance = this.variance;
      c.depth = this.depth + 1;
      c.outer = this;
      c
    }

    def make(unit: CompilationUnit): Context =
      make(unit, EmptyTree, this.owner, this.scope);

    def make(tree: Tree, owner: Symbol, scope: Scope): Context =
      make(this.unit, tree, owner, scope);

    def makeNewScope(tree: Tree, owner: Symbol): Context =
      make(tree, owner, new Scope(this.scope));

    def make(tree: Tree, owner: Symbol): Context =
      make(tree, owner, this.scope);

    def make(tree: Tree): Context =
      make(tree, this.owner);

    def outerContext(clazz: Symbol): Context = {
      var c = this;
      while (c != NoContext && c.owner != clazz) c = c.outer.enclClass;
      c
    }

    def isLocal(): boolean = tree match {
      case Block(_,_) => true
      case PackageDef(_, _) => false
      case EmptyTree => false
      case _ => outer.isLocal()
    }

    override def toString(): String = {
      if (this == NoContext) "NoContext";
      else tree.toString() + "\n:: " + outer.toString()
    }

    private var importsCache: List[ImportInfo] = null;

    def imports: List[ImportInfo] = {
      def collectImports(stats: List[Tree]): List[ImportInfo] = stats match {
        case Nil => outer.imports
        case (imp @ Import(_, _)) :: rest => new ImportInfo(imp, depth) :: collectImports(rest)
        case _ :: rest => collectImports(rest)
      }
      if (importsCache == null) {
        importsCache = tree match {
          case PackageDef(_, stats) => collectImports(stats)
          case Template(_, stats) => collectImports(stats)
          case Block(stats, _) => collectImports(stats)
          case _ => outer.imports
        }
      }
      importsCache
    }
  }

  class ImportInfo(tree: Import, val depth: int) {

    /** The prefix expression */
    def qual: Tree = tree.symbol.info match {
      case ImportType(expr) => expr
      case _ => throw new FatalError("symbol " + tree.symbol + " has bad type: " + tree.symbol.info);//debug
    }

    /** Is name imported explicitly, not via wildcard? */
    def isExplicitImport(name: Name): boolean =
      tree.selectors exists (._2.==(name.toTermName));

    /** The symbol with name `name' imported from import clause `tree'.
     */
    def importedSymbol(name: Name): Symbol = {
      var result: Symbol = NoSymbol;
      var renamed = false;
      var selectors = tree.selectors;
      while (selectors != Nil && result == NoSymbol) {
        if (selectors.head._2 == name.toTermName)
	  result = tree.expr.symbol.info.member(
            if (name.isTypeName) selectors.head._1.toTypeName else selectors.head._1);
        else if (selectors.head._1 == name.toTermName)
          renamed = true
        else if (selectors.head._1 == nme.WILDCARD && !renamed)
          result = tree.expr.symbol.info.member(name);
        selectors = selectors.tail
      }
      result
    }

    override def toString() = tree.toString();
  }

  case class ImportType(expr: Tree) extends Type;
}


