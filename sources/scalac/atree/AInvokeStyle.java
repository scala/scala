/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.util.Debug;

/** This class represents a method invocation style. */
public class AInvokeStyle {

    //########################################################################
    // Public Cases

    public case New;
    public case Dynamic;
    public case Static(boolean onInstance);

    //########################################################################
    // Public Constants

    public static final AInvokeStyle StaticClass    = Static(false);
    public static final AInvokeStyle StaticInstance = Static(true);

    //########################################################################
    // Public Methods

    /** Returns a string representation of this style. */
    public String toString() {
        switch (this) {
        case New:
            return "new";
        case Dynamic:
            return "dynamic";
        case Static(false):
            return "static-class";
        case Static(true):
            return "static-instance";
        default:
            throw Debug.abort("unknown case", this);
        }
    }

    //########################################################################
}
