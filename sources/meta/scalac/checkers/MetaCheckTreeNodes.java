/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.checkers;

import meta.java.Type;
import meta.scalac.ast.AbstractTreeCaseExpander;
import meta.scalac.ast.TreeKind;
import meta.scalac.ast.TreeType;
import meta.scalac.ast.TreeNode;
import meta.scalac.ast.TreeField;

public class MetaCheckTreeNodes extends AbstractTreeCaseExpander {

    //########################################################################
    // Public Methods

    public void printTreeCaseBody(TreeNode node) {
        if (node.fields != null) {
            for (int i = 0; i < node.fields.length; i++) {
                TreeField field = node.fields[i];
                printCheckField(node, field.type, field.name, "i");
            }
        }
        printCheckNode(node);
        writer.println("return;");
    }

    private void printCheckNode(TreeNode node) {
        if (node.start.constant != null) {
            writer.println("assert global.currentPhase.id >=  " +
                "global.PHASE."+node.start.constant + ".id :").indent();
            writer.println("\"cannot create instance of " + node.name +
                " before phase " + node.start.name + ", \" +");
            writer.println(
                "\"current phase is \" + " + "global.currentPhase;").undent();
        }
        if (node.stop.constant != null) {
            writer.println("assert global.currentPhase.id <=  " +
                "global.PHASE."+node.stop.constant + ".id :").indent();
            writer.println("\"cannot create instance of " + node.name +
                " after phase " + node.stop.name + ", \" +");
            writer.println(
                "\"current phase is \" + " + "global.currentPhase;").undent();
        }
    }

    private void printCheckField(TreeNode node,Type type,String name,String i){
        if (type.isPrimitive()) return;
        writer.println("assert " + name + " != null :").indent();
        printNullValue(node, name);
        writer.println(";").undent();
        switch (type) {
        case Reference(_, _):
            break;

        case Array(Type item):
            writer.print(
                "for (int "+i+" = 0; "+i+" < "+name+".length; "+i+"++)");
            writer.lbrace();
            printCheckField(node, item, name+"["+i+"]", i+"i");
            writer.rbrace();
            break;

        case TreeType.Name(TreeKind kind):
            if (kind != TreeKind.Any && kind != TreeKind.Test) {
                writer.println("assert " + (kind == TreeKind.Type ? "" : "!")+
                    name +".isTypeName() :").indent();
                printWrongKind(node, name, kind);
                writer.println(";").undent();
            }
            break;

        case TreeType.Tree(TreeKind kind):
            if (kind != TreeKind.Any) {
                writer.println("assert " +
                    name + ".is" + kind + "() :").indent();
                printWrongKind(node, name, kind);
                writer.println(";").undent();
            }
            break;

        case TreeType.Node(_):
            break;

        default:
            throw new Error(type.getClass().getName());
        }
    }

    private void printNullValue(TreeNode node, String field) {
        String expanded = getExpandedField(field);
        writer.print("\"field " +expanded+ " of class " +node+ " is null\"");
    }

    private void printWrongKind(TreeNode node, String field, TreeKind kind) {
        String expanded = getExpandedField(field);
        writer.println("\"field " + expanded + " of class " + node +
            " is not of kind " + kind + ", \" +");
        writer.print("\"found: \" + Debug.show("+field+")");
        // !!! " + \" of kind \" + kind("+field+")";
    }

    private String getExpandedField(String field) {
        int begin = field.indexOf('[');
        if (begin < 0) return field;
        StringBuffer buffer = new StringBuffer(field.substring(0, begin));
        for (int end; (end = field.indexOf(']', begin)) >= 0; begin = end + 1){
            String index = field.substring(begin + 1, end);
            buffer.append("[\"+").append(index).append("+\"]");
        }
        return buffer.toString();
    }

    //########################################################################
}


/* !!!

    protected static String kind(Tree tree) {
        switch (tree) {
        case Select(_, Name name): return kind(name);
        case Ident(Name name): return kind(name);
        }
        return "unknown";
    }

    protected static String kind(Name name) {
        if (name.isTypeName()) return "Type";
        if (name.isTermName()) return "Term";
        if (name.isConstrName()) return "Constr";
        return "unknown";
    }

*/
