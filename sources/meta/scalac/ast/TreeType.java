/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.ast;

import meta.java.Type;

/** This class describes types used in tree nodes. */
public class TreeType extends Type {

    //########################################################################
    // Private Constants

    private static final String NAME_PACKAGE  = "scalac.util";
    private static final String NAME_NAME     = "Name";
    private static final String NAME_FULLNAME = NAME_PACKAGE + "." + NAME_NAME;
    private static final String TREE_PACKAGE  = "scalac.ast";
    private static final String TREE_NAME     = "Tree";
    private static final String TREE_FULLNAME = TREE_PACKAGE + "." + TREE_NAME;

    //########################################################################
    // Public Cases

    public case Name(TreeKind kind);
    public case Tree(TreeKind kind);
    public case Node(TreeNode node);

    //########################################################################
    // Public Methods

    /** Returns the type's (possibly fully qualified) name. */
    public String getName(boolean qualified) {
        switch (this) {
        case Name(_):
            return qualified ? NAME_FULLNAME : NAME_NAME;
        case Tree(_):
            return qualified ? TREE_FULLNAME : TREE_NAME;
        case Node(TreeNode node):
            return qualified ? TREE_FULLNAME + "." + node.name : node.name;
        default:
            return super.getName(qualified);
        }
    }

    /** Returns the type's owner (its package or enclosing type). */
    public String getOwner() {
        switch (this) {
        case Name(_):
            return NAME_PACKAGE;
        case Tree(_):
            return TREE_PACKAGE;
        case Node(TreeNode node):
            return TREE_FULLNAME;
        default:
            return super.getOwner();
        }
    }

    //########################################################################
}
