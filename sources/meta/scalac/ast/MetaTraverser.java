/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.ast;

public class MetaTraverser extends AbstractTreeCaseExpander {

    //########################################################################
    // Public Methods

    public void printTreeCaseBody(TreeNode node) {
        if (node.fields != null)
            for (int i = 0; i < node.fields.length; i++)
                if (Tree.isTree(node.fields[i].type))
                    writer.println("traverse(" + node.fields[i] + ");");
        writer.println("return;");
    }

    //########################################################################
}
