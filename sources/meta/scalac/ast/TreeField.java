/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.ast;

import meta.java.Type;
import meta.java.JavaWriter;

/** This class describes a tree node field. */
public class TreeField {

    //########################################################################
    // Public Fields

    public final Type type;
    public final String name;
    public final TreeFieldLink link;

    //########################################################################
    // Public Constructors

    public TreeField(Type type, String name) {
        this(type, name, null);
    }

    public TreeField(Type type, String name, TreeFieldLink link) {
        this.type = type;
        this.name = name;
        this.link = link;
    }

    //########################################################################
    // Public Function

    public static JavaWriter print(JavaWriter writer, TreeField[] fields,
        boolean withType)
    {
        for (int i = 0; i < fields.length; i++) {
            if (i > 0) writer.print(", ");
            fields[i].print(writer, withType);
        }
        return writer;
    }

    //########################################################################
    // Public Methods

    public JavaWriter print(JavaWriter writer, boolean withType) {
        if (withType) writer.print(type).space();
        return writer.print(name);
    }

    public String toString() {
        return name;
    }

    //########################################################################
}
