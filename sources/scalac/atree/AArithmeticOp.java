/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.util.Debug;

/** This class represents an arithmetic operation. */
public class AArithmeticOp {

    //########################################################################
    // Public Cases

    /** An arithmetic addition operation */
    public case ADD;

    /** An arithmetic subtraction operation */
    public case SUB;

    /** An arithmetic multiplication operation */
    public case MUL;

    /** An arithmetic division operation */
    public case DIV;

    /** An arithmetic remainder operation */
    public case REM;

    //########################################################################
    // Public Methods

    /** Returns a string representation of this operation. */
    public String toString() {
        switch (this) {
        case ADD: return "ADD";
        case SUB: return "SUB";
        case MUL: return "MUL";
        case DIV: return "DIV";
        case REM: return "REM";
        default : throw Debug.abort("unknown case", this);
        }
    }

    //########################################################################
}
