/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.checkers;

import scalac.ast.Tree;
import scalac.util.Name;
import scalac.symtab.Type;
import scalac.symtab.Symbol;
import scalac.symtab.Kinds;
import scalac.Global;
import scalac.util.Debug;

/**
 * Check the following properties:
 *   1. all tree nodes have a type,
 *   2. TypeRefs supply the correct number of arguments.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class CheckTypes extends Checker {
    public CheckTypes(Global global) { super(global); }

    public void check(Tree tree) {
	verify(tree, tree.type != null, "non-null type", "type of tree is not null");

        if (tree.type != null) {
            switch (tree.type) {
            case TypeRef(Type pre, Symbol sym, Type[] args):
                if (sym.kind == Kinds.CLASS)
                    verify(tree,
                           sym.typeParams().length == args.length,
                           "enough arguments for TypeRefs",
                           "Type " + Debug.show(sym)
                           + " expects " + sym.typeParams().length + " type arguments"
                           + " but is given " + args.length);
                break;
            }
        }
    }
}
