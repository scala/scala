/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $Id$

package scalac.ast;

import scalac.ast.*;

/**
 * Abstract superclass for all TreeCopyFactories, which provides only
 * the code to copy the attribution from the "old" to the "new" tree.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public abstract class AbstractTreeCopyFactory implements TreeCopyFactory {
    public void attribute(Tree newTree, Tree oldTree) {
        if (newTree != oldTree) {
            newTree.type = oldTree.type;
            if (newTree.hasSymbol())
                newTree.setSymbol(oldTree.symbol());
        }
    }
}
