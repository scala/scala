/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.ast;

import meta.java.AbstractJavaExpander;

/** A base class for expanders that handle tree nodes. */
public abstract class AbstractTreeExpander extends AbstractJavaExpander {

    //########################################################################
    // Public Fields

    public final Tree tree;

    //########################################################################
    // Public Constructors

    public AbstractTreeExpander() {
        this.tree = new Tree();
        writer.importFrom(tree.t_Tree);
        for (int i = 0; i < tree.nodes.length; i++)
            if (tree.nodes[i].fields != null)
                for (int j = 0; j < tree.nodes[i].fields.length; j++)
                    writer.importType(tree.nodes[i].fields[j].type);
    }

    //########################################################################
}
