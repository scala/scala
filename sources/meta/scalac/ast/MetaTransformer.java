/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.ast;

import meta.java.Type;

public class MetaTransformer extends AbstractTreeCaseExpander {

    //########################################################################
    // Public Methods

    public void printTreeCaseBody(TreeNode node) {
        if (node.fields == null) {
            writer.println("return tree;");
        } else {
            if (node.hasSymbol()) {
                writer.print("if (tree.symbol() != null)").lbrace();
                printTransformNode(node, true);
                writer.undent().print("} else").lbrace();
            }
            printTransformNode(node, false);
            if (node.hasSymbol()) writer.rbrace();
        }
    }

    public void printTransformNode(TreeNode node, boolean withSymbol) {
        TreeField[] fields = node.getFields(withSymbol);
        writer.print("return copy." + node + "(tree");
        if (withSymbol) writer.print(", tree.symbol()");
        for (int i = 0; i < fields.length; i++) {
            writer.print(", ");
            if (Tree.isTree(fields[i].type))
                writer.print("transform(" + fields[i].name + ")");
            else
                writer.print(fields[i].name);
        }
        writer.println(");");
    }

    public void printTransformArrays() {
        for (int j = 1; j <= tree.arrays; j++)
            printTransformArray(tree.getType(j), false);
        for (int i = 0; i < tree.nodes.length; i++)
            for (int j = 1; j <= tree.nodes[i].arrays; j++)
                printTransformArray(tree.nodes[i].getType(j), j == 1);
    }

    public void printTransformArray(Type type, boolean needCast) {
        Type item = type.getItemType();
        Type erased = needCast ? tree.getType(0) : item;
        String cast = needCast ? "(" + item + ")" : "";
        writer.print("public ").print(type).print(" transform").
            print("(").print(type).print(" ts)").lbrace();
        writer.print("for (int i = 0; i < ts.length; i++)").lbrace();
        writer.println(erased + " t = transform(ts[i]);");
        writer.print("if (t != ts[i])").lbrace();
        writer.println(type+" res = new "+item.newArray("[ts.length]")+";");
        writer.println("System.arraycopy(ts, 0, res, 0, i);");
        writer.println("res[i++] = "+cast+"t;");
        writer.print("for (; i < ts.length; i++)").
            println("res[i] = "+cast+"transform(ts[i]);");
        writer.println("return res;");
        writer.rbrace();
        writer.rbrace();
        writer.println("return ts;");
        writer.rbrace();
        writer.line();
    }

    //########################################################################
}
