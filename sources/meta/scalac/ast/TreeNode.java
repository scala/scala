/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.ast;

import meta.java.Type;
import meta.java.JavaWriter;
import meta.scalac.Phase;

/** This class describes a tree node. */
public class TreeNode {

    //########################################################################
    // Public Fields

    public final String name;
    public final TreeKind kind;
    public final TreeSymbol symbol;
    public String description;
    public Phase start;
    public Phase stop;
    public TreeField[] fields;
    public int arrays;

    //########################################################################
    // Public Constructors

    public TreeNode(String name, TreeKind kind, TreeSymbol symbol) {
        this(name, kind, symbol, new TreeField[0]);
    }

    public TreeNode(String name, TreeKind kind, TreeSymbol symbol,
        TreeField[] fields)
    {
        this.name = name;
        this.kind = kind;
        this.symbol = symbol;
        this.fields = fields;
    }

    //########################################################################
    // Public Methods

    public boolean hasExtClass() {
        return hasSymbol();
    }

    public boolean hasSymbol() {
        return symbol == symbol.HasSym || symbol == symbol.DefSym;
    }

    public boolean definesSymbol() {
        return symbol == symbol.DefSym;
    }

    public Type getType(int rank) {
        arrays = Math.max(arrays , rank);
        return rank == 0 ? TreeType.Node(this) : Type.Array(getType(rank - 1));
    }

    public TreeNode setDescription(String description) {
        this.description = description;
        return this;
    }

    public TreeNode setRange(Phase start, Phase stop) {
        this.start = start;
        this.stop = stop;
        return this;
    }

    public TreeNode noFields() {
        fields = null;
        return this;
    }

    public TreeNode addField(Type type, String name) {
        TreeField[] array = new TreeField[fields.length + 1];
        for (int i = 0; i < fields.length; i++) array[i] = fields[i];
        array[fields.length] = new TreeField(type, name);
        fields = array;
        return this;
    }

    public JavaWriter printCase(JavaWriter writer, boolean wildcards) {
        writer.print("case ").print(name);
        if (fields!=null) printParams(writer.print("("),wildcards).print(")");
        return writer.print(":").space();
    }

    public JavaWriter printParams(JavaWriter writer) {
        return printParams(writer, false);
    }

    public JavaWriter printParams(JavaWriter writer, String prefix) {
        return printParams(printPrefix(writer, prefix));
    }

    public JavaWriter printParams(JavaWriter writer, boolean wildcards) {
        if (fields != null) for (int i = 0; i < fields.length; i++) {
            if (i > 0) writer.print(", ");
            if (wildcards) writer.print("_");
            else writer.print(fields[i].type).space().print(fields[i].name);
        }
        return writer;
    }

    public JavaWriter printArgs(JavaWriter writer) {
        if (fields != null) for (int i = 0; i < fields.length; i++) {
            if (i > 0) writer.print(", ");
            writer.print(fields[i].name);
        }
        return writer;
    }

    public JavaWriter printArgs(JavaWriter writer, String prefix) {
        return printArgs(printPrefix(writer, prefix));
    }

    public JavaWriter printPrefix(JavaWriter writer, String prefix) {
        if (prefix != null) {
            writer.print(prefix);
            if (fields != null && fields.length > 0 && prefix != null)
                writer.print(", ");
        }
        return writer;
    }

    public JavaWriter printNew(JavaWriter writer) {
        String classname = (hasExtClass() ? "Ext" : "") + name;
        return printArgs(writer.print("new " + classname + "(")).print(")");
    }

    public String toString() {
        return name;
    }

    //########################################################################
}
