/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.symtab.Symbol;

/** This class represents an attributed function reference. */
public class AFunction {

    //########################################################################
    // Public Cases

    public case Method(ACode object, Symbol method, AInvokeStyle style);
    public case Primitive(APrimitive primitive);

    //########################################################################
    // Public Methods

    /** Returns a string representation of this function. */
    public String toString() {
        return new ATreePrinter().printFunction(this).toString();
    }

    //########################################################################
}
