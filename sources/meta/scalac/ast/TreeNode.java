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
    // Public Methods - Initializing

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
        return addField(type, name, null);
    }

    public TreeNode addField(Type type, String name, TreeFieldLink link) {
        if (link != null && !hasSymbol())
            throw new Error("node "+this+" may not have linked fields");
        TreeField[] array = new TreeField[fields.length + 1];
        for (int i = 0; i < fields.length; i++) array[i] = fields[i];
        array[fields.length] = new TreeField(type, name, link);
        fields = array;
        return this;
    }

    //########################################################################
    // Public Methods - Querying

    public Type getType(int rank) {
        arrays = Math.max(arrays , rank);
        return rank == 0 ? TreeType.Node(this) : Type.Array(getType(rank - 1));
    }

    public boolean hasExtClass() {
        return hasSymbol();
    }

    public boolean hasSymbol() {
        switch (symbol) {
        case TreeSymbol.HasSym(_, _):
            return true;
        default:
            return false;
        }
    }

    public boolean definesSymbol() {
        switch (symbol) {
        case TreeSymbol.HasSym(_, true):
            return true;
        default:
            return false;
        }
    }

    public TreeField getSymbol() {
        switch (symbol) {
        case TreeSymbol.HasSym(TreeField field, _):
            return field;
        default:
            return null;
        }
    }

    public boolean hasLinkedFields() {
        for (int i = 0; i < fields.length; i++)
            if (fields[i].link != null) return true;
        return false;
    }

    public TreeField[] getFields(boolean withoutLinkedFields) {
        if (fields == null || !withoutLinkedFields) return fields;
        int count = 0;;
        for (int i = 0; i < fields.length; i++)
            if (fields[i].link == null) count++;
        TreeField[] array = new TreeField[count];
        for (int i = 0, j = 0; i < fields.length; i++)
            if (fields[i].link == null) array[j++] = fields[i];
        return array;
    }

    public String toString() {
        return name;
    }

    //########################################################################
    // Public Methods - Printing

    public JavaWriter printCase(JavaWriter writer, boolean withWildcards) {
        writer.print("case ");
        if (fields != null && withWildcards) {
            writer.print(name).print('(');
            for (int i = 0; i < fields.length; i++) {
                if (i > 0) writer.print(", ");
                writer.print("_");
            }
            writer.print(')');
        } else {
            printDecl(writer, null, false);
        }
        return writer.print(":").space();
    }

    public JavaWriter printNew(JavaWriter writer, boolean withSymbol) {
        writer.print("new ");
        if (hasExtClass()) writer.print("Ext");
        return printCall(writer, null, withSymbol);
    }

    public JavaWriter printMethod(JavaWriter writer, String prefix,
        boolean withSymbol)
    {
        return printMethod(writer, prefix, withSymbol, withSymbol);
    }

    public JavaWriter printMethod(JavaWriter writer, String prefix,
        boolean withSymbol, boolean withoutLinkedFields)
    {
        writer.print("public ").print(name).space();
        return printDecl(writer, prefix, withSymbol, withoutLinkedFields);
    }

    public JavaWriter printDecl(JavaWriter writer, String prefix,
        boolean withSymbol)
    {
        return printDecl(writer, prefix, withSymbol, withSymbol);
    }

    public JavaWriter printDecl(JavaWriter writer, String prefix,
        boolean withSymbol, boolean withoutLinkedFields)
    {
        return printPattern(writer,true,prefix,withSymbol,withoutLinkedFields);
    }

    public JavaWriter printCall(JavaWriter writer, String prefix,
        boolean withSymbol)
    {
        return printCall(writer, prefix, withSymbol, withSymbol);
    }

    public JavaWriter printCall(JavaWriter writer, String prefix,
        boolean withSymbol, boolean withoutLinkedFields)
    {
        return printPattern(
            writer, false, prefix, withSymbol, withoutLinkedFields);
    }

    public JavaWriter printPattern(JavaWriter writer, boolean withType,
        String prefix, boolean withSymbol, boolean withoutLinkedFields)
    {
        writer.print(name);
        if (fields != null || prefix != null || withSymbol) {
            writer.print('(');
            printFields(writer,withType,prefix,withSymbol,withoutLinkedFields);
            writer.print(')');
        }
        return writer;
    }

    public JavaWriter printFields(JavaWriter writer, boolean withType,
        String prefix, boolean withSymbol, boolean withoutLinkedFields)
    {
        TreeField[] fields = getFields(withoutLinkedFields);
        if (prefix != null) {
            writer.print(prefix);
            if (withSymbol || (fields != null && fields.length > 0))
                writer.print(", ");
        }
        if (withSymbol) {
            getSymbol().print(writer, withType);
            if (fields != null && fields.length > 0) writer.print(", ");
        }
        TreeField.print(writer, fields, withType);
        return writer;
    }

    //########################################################################
}
