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
            writer.print("return copy." + node + "(tree");
            for (int i = 0; i < node.fields.length; i++) {
                writer.print(", ");
                if (Tree.isTree(node.fields[i].type))
                    writer.print("transform(" + node.fields[i] + ")");
                else
                    writer.print(node.fields[i].name);
            }
            writer.println(");");
        }
    }

    public void printTransformArrays() {
        printTransformArrayOf(tree.t_Tree, false);
        printTransformArrayOf(tree.t_Trees, false);
        for (int i = 0; i < tree.nodes.length; i++)
            for (int j = 0; j < tree.nodes[i].arrays; j++)
                printTransformArrayOf(tree.nodes[i].getType(j), j == 0);
    }

    public void printTransformArrayOf(Type type, boolean needCast) {
        String cast = needCast ? "(" + type + ")" : "";
        writer.print("public "+type+"[] transform("+type+"[] ts)");
        writer.lbrace();
        writer.print("for (int i = 0; i < ts.length; i++)");
        writer.lbrace();
        writer.println((needCast?tree.t_Tree:type)+" t = transform(ts[i]);");
        writer.print("if (t != ts[i])");
        writer.lbrace();
        writer.println(type+"[] res = new "+type.newArray("[ts.length]")+";");
        writer.println("System.arraycopy(ts, 0, res, 0, i);");
        writer.println("res[i] = "+cast+"t;");
        writer.println("for (int j = i + 1; j < ts.length; j++)");
        writer.println("res[j] = "+cast+"transform(ts[j]);");
        writer.println("return res;");
        writer.rbrace();
        writer.rbrace();
        writer.println("return ts;");
        writer.rbrace();
        writer.line();
    }

    //########################################################################
}
