/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.checkers;

import scalac.ast.Tree;
import scalac.symtab.Symbol;
import scalac.Global;

/**
 * Verify that all tree nodes for which hasSymbol() is true have a
 * non-null symbol.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class CheckSymbols extends Checker {
    public CheckSymbols(Global global) { super(global); }

    public void check(Tree tree) {
        verify(tree,
               implies(tree.hasSymbol(), tree.symbol() != null),
               "symbol not null",
               "hasSymbol => symbol not null");
        verify(tree,
               implies(tree.hasSymbol(), tree.symbol() != Symbol.NONE),
               "symbol not NONE",
               "hasSymbol => symbol not NONE");
    }
}
