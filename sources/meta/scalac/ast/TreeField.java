/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.ast;

import meta.java.Type;

/** This class describes a tree node field. */
public class TreeField {

    //########################################################################
    // Public Fields

    public final Type type;
    public final String name;
    public final TreeFieldLink link;

    //########################################################################
    // Public Constructors

    public TreeField(Type type, String name, TreeFieldLink link) {
        this.type = type;
        this.name = name;
        this.link = link;
    }

    //########################################################################
    // Public Methods

    public String toString() {
        return name;
    }

    //########################################################################
}
