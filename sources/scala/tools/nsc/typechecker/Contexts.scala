/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import scala.tools.util.Position;

class Contexts: Analyzer {
  import global._;

  val NoContext = new Context();

  val startContext = {
    import definitions._;
    var sc = NoContext.make(EmptyTree, RootClass, RootClass.info.decls);
    def addImport(pkg: Symbol): unit = {
      sc = sc.make(sc.tree, sc.owner, new Scope(sc.scope));
      val impTree = Import(gen.mkGlobalRef(pkg), List(Pair(nme.WILDCARD, null)));
      impTree.setSymbol(NoSymbol.newImport(Position.NOPOS)).setType(this.ImportType(impTree));
      sc.scope.enter(impTree.symbol)
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
    var enclClass: Context = this;          // The next outer context whose tree
					    // is a class template
    var variance: int = _;                  // Variance relative to enclosing class.
    var undetparams: List[Symbol] = List(); // Undetermined type parameters
    var constructorClass: Symbol = _;       // Class for auxiliary constructor
    var depth: int = 0;
    val imports: List[Tree] = List();

    def make(unit: CompilationUnit, tree: Tree, owner: Symbol, scope: Scope): Context = {
      val c = new Context();
      c.unit = unit;
      c.tree = tree;
      c.owner = owner;
      c.scope = scope;
      c.enclClass = if ((tree.isInstanceOf[Template] ||
			 tree.isInstanceOf[CompoundTypeTree]) &&
			tree != this.tree) c
		    else this.enclClass;
      c.variance = this.variance;
      c.constructorClass = this.constructorClass;
      c.depth = this.depth + 1;
      c.outer = this;
      c
    }

    def make(unit: CompilationUnit): Context =
      make(unit, EmptyTree, this.owner, new Scope(this.owner.info.decls));

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
  }
}


