/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.ast;

public class MetaLazyTreeCopier extends AbstractTreeMethodExpander {

    //########################################################################
    // Public Methods

    public void printTreeMethod(TreeNode node) {
        printTreeMethodHeader(node, tree.t_Tree + " tree");
        writer.lbrace();
        writer.println(node.name + " t = (" + node.name + ")tree;");
        if (node.fields.length > 0) {
            writer.print("if (").indent();
            for (int i = 0; i < node.fields.length; i++) {
                if (i > 0) writer.println(" &&");
                String name = node.fields[i].name;
                writer.print("t." + name + " == " + name);
            }
            writer.println(")");
            writer.println("return t;").undent();
            writer.print("t = copier." + node.name + "(");
            node.printArgs(writer, "tree").println(");");
            writer.println("t.type = tree.type;");
            if (node.hasSymbol())writer.println("t.setSymbol(tree.symbol());");
        }
        writer.println("return t;");
        writer.rbrace();
    }

    //########################################################################
}
