/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.util.Debug;

/** This class represents a test operation. */
public class ATestOp {

    //########################################################################
    // Public Cases

    /** An equality test */
    public case EQ;

    /** A non-equality test */
    public case NE;

    /** A less-than test */
    public case LT;

    /** A greater-than-or-equal test */
    public case GE;

    /** A less-than-or-equal test */
    public case LE;

    /** A greater-than test */
    public case GT;

    //########################################################################
    // Public Methods

    /** Returns the negation of this operation. */
    public ATestOp negate() {
        switch (this) {
        case EQ: return NE;
        case NE: return EQ;
        case LT: return GE;
        case GE: return LT;
        case LE: return GT;
        case GT: return LE;
        default: throw Debug.abort("unknown case", this);
        }
    }

    /** Returns a string representation of this operation. */
    public String toString() {
        switch (this) {
        case EQ: return "NE";
        case NE: return "EQ";
        case LT: return "GE";
        case GE: return "LT";
        case LE: return "GT";
        case GT: return "LE";
        default: throw Debug.abort("unknown case", this);
        }
    }

    //########################################################################
}
