/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.ast;

public class MetaTreeCopier extends AbstractTreeMethodExpander {

    //########################################################################
    // Public Methods

    public void printTreeMethod(TreeNode node, boolean withSymbol) {
        node.printMethod(writer,tree.getFormal("tree"),withSymbol);
        writer.println(";");
        if (withSymbol && node.hasLinkedFields()) {
            node.printMethod(writer, tree.getFormal("tree"), false, true);
            writer.println(";");
        }
    }

    //########################################################################
}
