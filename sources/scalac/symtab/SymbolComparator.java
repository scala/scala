/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab;

import java.util.Comparator;

/** This class implements a symbol comparator. */
public class SymbolComparator implements Comparator {

    //########################################################################
    // Public Constants

    /** The unique instance of this class */
    public static final Comparator instance = new SymbolComparator();

    //########################################################################
    // Private Constructors

    /** Initializes this instance. */
    private SymbolComparator() {}

    //########################################################################
    // Public Methods

    /** Compares the two arguments which must inherit from Symbol. */
    public int compare(Object lf, Object rg) {
        return lf == rg ? 0 : ((Symbol)lf).isLess((Symbol)rg) ? -1 : 1;
    }

    //########################################################################
}
