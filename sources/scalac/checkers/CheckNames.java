/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.checkers;

import scalac.ast.Tree;
import scalac.util.Name;
import scalac.util.Debug;
import scalac.symtab.Symbol;

/**
 * Check that the of symbols are of the appropriate kind
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */

public class CheckNames extends Checker {

    public CheckNames(scalac.Global global) {
	super(global);
    }

    public void check(Tree tree) {
	switch (tree) {
	case ClassDef(_, Name name, _, _, _, _):
	    verify(tree,
		   name.isTypeName(),
		   "name kinds",
		   "class " + Debug.show(tree.symbol()) +
		   "should have a type name");

	    Symbol constr = tree.symbol().primaryConstructor();
	    verify(tree,
		   constr.name.isTypeName(),
		   "name kinds",
		   "the class constructor " + Debug.show(constr)
		   + " should have a type name");
	    break;
	}
    }
}
