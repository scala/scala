/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.symtab.Symbol;

/** This class represents an attributed value location. */
public class ALocation {

    //########################################################################
    // Public Cases

    public case Module(Symbol module); // !!! remove ?
    public case Field(ACode object, Symbol field, boolean isStatic);
    public case Local(Symbol local, boolean isArgument);
    public case ArrayItem(ACode array, ACode index);

    //########################################################################
    // Public Methods

    /** Returns a string representation of this location. */
    public String toString() {
        return new ATreePrinter().printLocation(this).toString();
    }

    //########################################################################
}
