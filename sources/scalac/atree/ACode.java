/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.symtab.Symbol;

/** This class represents attributed code. */
public class ACode {

    //########################################################################
    // Public Cases

    public case Void;
    public case This(Symbol clasz);
    public case Constant(AConstant constant);

    //########################################################################
    // Public Fields

    /** The source file position */
    public int pos;

    //########################################################################
    // Public Methods

    /** Returns a string representation of this code. */
    public String toString() {
        return new ATreePrinter().printCode(this).toString();
    }

    //########################################################################
}
