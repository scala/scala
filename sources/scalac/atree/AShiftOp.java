/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.util.Debug;

/** This class represents a shift operation. */
public class AShiftOp {

    //########################################################################
    // Public Cases

    /** An arithmetic shift to the left */
    public case ASL;

    /** An arithmetic shift to the right */
    public case ASR;

    /** A logical shift to the right */
    public case LSR;

    //########################################################################
    // Public Methods

    /** Returns a string representation of this operation. */
    public String toString() {
        switch (this) {
        case ASL: return "ASL";
        case ASR: return "ASR";
        case LSR: return "LSR";
        default: throw Debug.abort("unknown case", this);
        }
    }

    //########################################################################
}
