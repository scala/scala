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
import scalac.Global;
import scalac.util.Debug;

/**
 * Check that all tree nodes have a type.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class CheckTypes extends Checker {
    public CheckTypes(Global global) { super(global); }

    public void check(Tree tree) {
	verify(tree, tree.type != null, "non-null type", "type of tree is not null");
    }
}
