/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.ast;

/** A base class for expanders that generate one method per tree node. */
public abstract class AbstractTreeMethodExpander extends AbstractTreeExpander {

    //########################################################################
    // Public Methods

    public void printTreeMethods() {
        for (int i = 0; i < tree.nodes.length; i++) {
            if (tree.nodes[i].fields != null) {
                printTreeMethod(tree.nodes[i], false);
                if (tree.nodes[i].hasSymbol())
                    printTreeMethod(tree.nodes[i], true);
            }
            writer.println();
        }
    }

    public abstract void printTreeMethod(TreeNode node, boolean withSymbol);

    //########################################################################
}
