/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.ast;

import meta.java.Type;

public class MetaTree extends AbstractTreeExpander {

    //########################################################################
    // Public Methods

    public void printEmptyArrays() {
        writer.print("public static final ");
        writer.print(tree.t_Trees.toString());
        writer.print(" EMPTY_ARRAY = new ").print(tree.NAME).println("[0];");
        for (int i = 0; i < tree.nodes.length; i++) {
            TreeNode node = tree.nodes[i];
            for (int j = 1; j <= node.arrays; j++) {
                writer.print("public static final ");
                writer.print(node.getType(j).toString());
                writer.space().print(node.name).print("_EMPTY");
                for (int k = 0; k < j; k++) writer.print("_ARRAY");
                writer.print(" = new ").print(node.name).print("[0]");
                for (int k = 1; k < j; k++) writer.print("[]");
                writer.println(";");
            }
        }
    }

    public void printTreeCases() {
        for (int i = 0; i < tree.nodes.length; i++)
            printTreeCase(tree.nodes[i]);
    }

    private void printTreeCase(TreeNode node) {
        writer.printDescription(new String[] {
            node.description,
            "- kind         : " + description(node.kind),
            "- symbol       : " + description(node.symbol),
            "- introduced by: " + node.start.name,
            "- eliminated by: " + node.stop.name,
        });
        writer.print("public case ").print(node.name);
        if (node.fields != null) {
            node.printParams(writer.print("(")).print(")");
            writer.lbrace();
            writer.println("assert CheckTreeNodes.instance.checkNode(this);");
            writer.rbrace();
        } else if (node == tree.n_Empty) {
            writer.print("; static { "+node.name+".type = ").
                print(tree.t_Type).println(".NoType; }");
        } else {
            writer.println(";");
        }
        writer.println();
    }

    private String description(TreeKind kind) {
        switch (kind) {
        case Any: return "this tree is of any kind";
        case Type: return "this tree is a type";
        case Term: return "this tree is a term";
        case Dual: return "this tree is a type or a term";
        case Test: return "this tree is a type or a term (determined by the kind of the name field)";
        case None: return "this tree is neither a type nor a term";
        default    : throw new Error(kind.getClass().getName());
        }
    }

    private String description(TreeSymbol symbol) {
        switch (symbol) {
        case NoSym : return "this tree has no symbol";
        case HasSym: return "this tree references a symbol";
        case DefSym: return "this tree defines a symbol";
        default    : throw new Error(symbol.getClass().getName());
        }
    }

    public void printIsKind() {
        printIsKind(tree.nodes, TreeKind.Type);
        printIsKind(tree.nodes, TreeKind.Term);
    }

    private void printIsKind(TreeNode[] nodes, TreeKind kind) {
        writer.println("/** Returns true if this tree is a " + kind.toString().toLowerCase() + ". */");
        writer.print("public boolean is" + kind + "()");
        writer.lbrace();
        writer.println("switch (this) {");

        for (int i = 0; i < nodes.length; i++)
            if (nodes[i].kind != TreeKind.Test && nodes[i].kind.isA(kind))
                nodes[i].printCase(writer, true).println();
        writer.indent().println("return true;").undent();

        for (int i = 0; i < nodes.length; i++) {
            if (nodes[i].kind != TreeKind.Test) continue;
            writer.print("case " + nodes[i].name + "(");
            for (int j = 0; j < nodes[i].fields.length; j++) {
                if (j > 0) writer.print(", ");
                switch (nodes[i].fields[j].type) {
                case TreeType.Name(Test):
                    writer.print(nodes[i].fields[j].type + " name");
                    break;
                default:
                    writer.print("_");
                    break;
                }
            }
            writer.println("):");
            writer.indent().print("return ");
            switch (kind) {
            case TreeKind.Type:
                writer.print("name.isTypeName() || name == Name.ERROR");
                break;
            case TreeKind.Term:
                writer.print("!name.isTypeName()");
                break;
            default:
                throw new Error("unexpected kind " + kind);
            }
            writer.println(";").undent();
        }

	writer.println("default:");
        writer.indent().println("return false;").undent();

        writer.println("}");
        writer.rbrace();
        writer.println();
    }

    public void printExtClasses() {
        for (int i = 0; i < tree.nodes.length; i++)
            printExtTreeNode(tree.nodes[i]);
    }

    private void printExtTreeNode(TreeNode node) {
        if (node.symbol == TreeSymbol.NoSym) return;
        writer.print("public static class Ext"+node+" extends "+node);
        writer.lbrace();
        writer.print("private ").print(tree.t_Symbol).println(" symbol;");
        writer.println();

        node.printParams(writer.print("public Ext"+node.name+"(")).print(")");
        writer.lbrace();
        node.printArgs(writer.print("super(")).println(");");
        writer.rbrace();
        writer.println();

        writer.print("public boolean hasSymbol()");
        writer.lbrace();
        writer.println("return true;");
        writer.rbrace();
        writer.println();

        if (node.symbol == TreeSymbol.DefSym) {
            writer.print("public boolean definesSymbol()");
            writer.lbrace();
            writer.println("return true;");
            writer.rbrace();
            writer.println();
        }

        writer.print("public ").print(tree.t_Symbol).print(" symbol()");
        writer.lbrace();
        writer.println("return symbol;");
        writer.rbrace();
        writer.println();

        writer.print("public Tree setSymbol(").print(tree.t_Symbol).print(" symbol)");
        writer.lbrace();
        writer.println("this.symbol = symbol;");
        if (node.hasLinkedFields()) {
            writer.print("if (symbol != null)").lbrace();
            for (int i = 0; i < node.fields.length; i++) {
                TreeField field = node.fields[i];
                TreeFieldLink link = field.link;
                if (link == null) continue;
                writer.println("this."+field+" = symbol."+link.getLink()+";");
            }
            writer.rbrace();
        }
        writer.println("return this;");
        writer.rbrace();
        writer.println();

        writer.rbrace();
        writer.println();
    }

    //########################################################################
}
