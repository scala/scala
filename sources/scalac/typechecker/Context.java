/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**
** $Id$
\*                                                                      */

package scalac.typechecker;

import scalac.symtab.*;
import scalac.ast.Tree;

public class Context {
    Tree tree;                 // Tree associated with this context
    Symbol owner;              // The current owner
    Scope scope;               // The current scope
    ImportList imports;        // The current import list
    Context outer;             // The next outer context
    Context enclClass = this;  // The next outer context whose tree
                               // is a class template
    boolean delayArgs = false; // delay checking of type arguments

    public Context() {}

    public Context(Tree tree, Context outer) {
        this(tree, outer.owner, outer.scope, outer);
    }

    public Context(Tree tree, Symbol owner, Scope scope, Context outer) {
	this.tree = tree;
	this.owner = owner;
	this.scope = scope;
	this.imports = outer.imports;
	if (tree instanceof Tree.Template ||
	    tree instanceof Tree.CompoundType) this.enclClass = this;
	else this.enclClass = outer.enclClass;
	this.delayArgs = outer.delayArgs;
	this.outer = outer;
    }

    public static Context NONE = new Context();

    Context outerContext(Symbol clazz) {
	Context c = this;
	while (c != Context.NONE && c.owner != clazz) c = c.outer;
	return c;
    }

    boolean isTopLevel() {
	switch (tree) {
	case Block(_):
	    return false;
	case Template(_, _):
	    return outer.tree instanceof Tree.PackageDef;
	case Empty:
	    return true;
	default:
	    return outer.isTopLevel();
	}
    }
}

