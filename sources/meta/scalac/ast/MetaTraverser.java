/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.ast;

import meta.java.Type;

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

    public void printTraverseArrays() {
        int max = tree.arrays;
        for (int i = 0; i < tree.nodes.length; i++)
            max = Math.max(max, tree.nodes[i].arrays);
        for (int i = 1; i <= max; i++)
            printTraverseArray(tree.getType(i));
    }

    public void printTraverseArray(Type type) {
        writer.print("public void traverse").
            print("(").print(type).print(" trees)").lbrace();
        writer.print("for (int i = 0; i < trees.length; i++) ").
            println("traverse(trees[i]);");
        writer.rbrace();
        writer.line();
    }

    //########################################################################
}
