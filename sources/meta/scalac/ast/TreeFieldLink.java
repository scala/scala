/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.ast;

/**
 * This class describes the possible links between a given field of a
 * tree and the symbol of that tree.
 */
public class TreeFieldLink {

    //########################################################################
    // Public Cases

    /** Field is linked to the symbol's flags */
    public case SymFlags;

    /** Field is linked to the symbol's name */
    public case SymName;

    //########################################################################
    // Public Methods

    /** Returns the field or method to invoke to get the linked value. */
    public String getLink() {
        switch (this) {
        case SymFlags:
            return "flags";
        case SymName:
            return "name";
        default:
            throw new Error("unknown case: " + this);
        }
    }

    /** Returns the name of this link. */
    public String toString() {
        switch (this) {
        case SymFlags:
            return "flags";
        case SymName:
            return "name";
        default:
            throw new Error("unknown case: " + this);
        }
    }

    //########################################################################
}
