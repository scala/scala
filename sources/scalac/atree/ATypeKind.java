/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.util.Debug;

/** This class represents a type kind. */
public class ATypeKind {

    //########################################################################
    // Public Cases

    /** The unit value */
    public case UNIT;

    /** A boolean value */
    public case BOOL;

    /** A 1-byte unsigned integer */
    public case U1;

    /** A 2-byte unsigned integer */
    public case U2;

    /** A 4-byte unsigned integer */
    public case U4;

    /** An 8-byte unsigned integer */
    public case U8;

    /** A 1-byte signed integer */
    public case I1;

    /** A 2-byte signed integer */
    public case I2;

    /** A 4-byte signed integer */
    public case I4;

    /** An 8-byte signed integer */
    public case I8;

    /** A 4-byte floating point number */
    public case R4;

    /** An 8-byte floating point number */
    public case R8;

    /** An object reference */
    public case REF;

    /** A string reference */
    public case STR;

    /** The null reference */
    public case NULL;

    /** The zero value */
    public case ZERO;

    //########################################################################
    // Public Methods

    /** Returns a string representation of this type kind. */
    public String toString() {
        switch (this) {
        case UNIT: return "UNIT";
        case BOOL: return "BOOL";
        case U1  : return "U1";
        case U2  : return "U2";
        case U4  : return "U4";
        case U8  : return "U8";
        case I1  : return "I1";
        case I2  : return "I2";
        case I4  : return "I4";
        case I8  : return "I8";
        case R4  : return "R4";
        case R8  : return "R8";
        case REF : return "REF";
        case STR : return "STR";
        case NULL: return "NULL";
        case ZERO: return "ZERO";
        default  : throw Debug.abort("unknown case", this);
        }
    }

    //########################################################################
}
