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
    public case Static;
    public case Dynamic;

    //########################################################################
    // Public Methods

    /** Returns a string representation of this style. */
    public String toString() {
        switch (this) {
        case New:
            return "new";
        case Static:
            return "static";
        case Dynamic:
            return "dynamic";
        default:
            throw Debug.abort("unknown case", this);
        }
    }

    //########################################################################
}
