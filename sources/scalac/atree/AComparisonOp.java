/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.util.Debug;

/** This class represents a comparison operation. */
public class AComparisonOp {

    //########################################################################
    // Public Cases

    /** A comparison operation with -1 default for NaNs */
    public case CMPL;

    /** A comparison operation with no default for NaNs */
    public case CMP;

    /** A comparison operation with +1 default for NaNs */
    public case CMPG;

    //########################################################################
    // Public Methods

    /** Returns a string representation of this operation. */
    public String toString() {
        switch (this) {
        case CMPL: return "CMPL";
        case CMP : return "CMP";
        case CMPG: return "CMPG";
        default  : throw Debug.abort("unknown case", this);
        }
    }

    //########################################################################
}
