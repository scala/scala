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

    public void printTreeMethod(TreeNode node, boolean withSymbol) {
        TreeField symbol = node.getSymbol();
        node.printMethod(writer, tree.getFormal("tree"), withSymbol).lbrace();
        if (!withSymbol && node.hasLinkedFields())
            writer.println("assert tree.symbol() == null : "+
                "\"tree's symbol is not null\";");
        writer.print(node.getType(0)).print(" t = make.");
        node.printCall(writer, "tree.pos", withSymbol).println(";");
        writer.println("t.type = tree.type;");
        if (!withSymbol && node.hasSymbol() && !node.hasLinkedFields()) {
            symbol.print(writer, true).println(" = tree.symbol();");
            writer.println("if ("+symbol+" != null) t.setSymbol("+symbol+");");
        }
        writer.println("return t;");
        writer.rbrace();

        if (withSymbol && node.hasLinkedFields()) {
            node.printMethod(writer, tree.getFormal("tree"), false, true);
            writer.lbrace();
            symbol.print(writer, true).println(" = tree.symbol();");
            node.printCall(writer.print("return "), "tree", true).println(";");
            writer.rbrace();
        }
    }

    //########################################################################
}


