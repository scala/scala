/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**
** $Id$
\*                                                                      */
package scala.tools.scalac.typechecker;

import scalac.symtab._;
import scalac.ast.Tree;
import scalac.ast._;

object Context {
  val NONE = new Context();
}

class Context {

  var tree: Tree = _;                 // Tree associated with this context
  var owner: Symbol = _;              // The current owner
  var scope: Scope = _;               // The current scope
  var imports: ImportList = _;        // The current import list
  var outer: Context = _;             // The next outer context
  var enclClass: Context = this;      // The next outer context whose tree
                                      // is a class template
  var variance: int = _;              // Variance relative to enclosing class.
  var constructorClass: Symbol = _;   // Class for auxiliary constructor

  def this(tree: Tree, owner: Symbol, scope: Scope, outer: Context) = {
    this();
    this.tree = tree;
    this.owner = owner;
    this.scope = scope;
    this.imports = outer.imports;
    this.enclClass = if (tree.isInstanceOf[Tree$Template] ||
			 tree.isInstanceOf[Tree$CompoundType]) this
		     else outer.enclClass;
    this.variance = outer.variance;
    this.constructorClass = outer.constructorClass;
    this.outer = outer;
  }

  def this(tree: Tree, outer: Context) =
    this(tree, outer.owner, outer.scope, outer);

  def outerContext(clazz: Symbol): Context = {
    var c = this;
    while (c != Context.NONE && c.owner != clazz) c = c.outer;
    c
  }

  def isTopLevel(): boolean = tree match {
    case Tree$Block(_) =>
      false
    case Tree$Template(_, _) =>
      outer.tree.isInstanceOf[Tree$PackageDef]
    case Tree.Empty =>
      true
    case _ =>
      outer.isTopLevel()
  }
}

