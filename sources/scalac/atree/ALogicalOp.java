/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.util.Debug;

/** This class represents a logical operation. */
public class ALogicalOp {

    //########################################################################
    // Public Cases

    /** A bitwise AND operation */
    public case AND;

    /** A bitwise OR operation */
    public case OR;

    /** A bitwise XOR operation */
    public case XOR;

    //########################################################################
    // Public Methods

    /** Returns a string representation of this operation. */
    public String toString() {
        switch (this) {
        case AND: return "AND";
        case OR : return "OR";
        case XOR: return "XOR";
        default : throw Debug.abort("unknown case", this);
        }
    }

    //########################################################################
}
