/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.ast;

public class MetaStrictTreeCopier extends AbstractTreeMethodExpander {

    //########################################################################
    // Public Methods

    public void printTreeMethod(TreeNode node) {
        printTreeMethodHeader(node, tree.t_Tree + " tree");
        writer.lbrace();
        writer.print(node.name + " t = make." + node.name + "(");
        node.printArgs(writer, "tree.pos").println(");");
        writer.println("t.type = tree.type;");
        if (node.hasSymbol()) writer.println("t.setSymbol(tree.symbol());");
        writer.println("return t;");
        writer.rbrace();
    }

    //########################################################################
}
