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

    public void printTreeMethod(TreeNode node, boolean withSymbol) {
        TreeField symbol = node.getSymbol();
        node.printMethod(writer, tree.getFormal("tree"), withSymbol).lbrace();
        if (!withSymbol && node.hasLinkedFields())
            writer.println("assert tree.symbol() == null : "+
                "\"tree's symbol is not null\";");
        writer.print(node.getType(0)).print(" t = (").
            print(node.getType(0)).println(")tree;");
        TreeField[] fields = node.getFields(withSymbol);
        // !!! why do we copy if there is no symbol and no field
        if (withSymbol || node.fields.length > 0) {
            writer.print("if (").indent();
            if (withSymbol) writer.print("t.symbol() == " + symbol);
            for (int i = 0; i < fields.length; i++) {
                if (i > 0 ? true : withSymbol) writer.println(" &&");
                writer.print("t." + fields[i] + " == " + fields[i]);
            }
            writer.println(")");
            writer.println("return t;").undent();
        }
        writer.print("return copier.");
        node.printCall(writer, "tree", withSymbol).println(";");
        writer.rbrace();

        if (withSymbol && node.hasLinkedFields()) {
            node.printMethod(writer, tree.getFormal("tree"), false, true);
            writer.lbrace();
            symbol.print(writer, true).println(" = tree.symbol();");
            node.printCall(writer.print("return "), "tree", true).println(";");
            writer.rbrace();
            return;
        }
    }

    //########################################################################
}
