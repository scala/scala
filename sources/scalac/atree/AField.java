/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.symtab.Symbol;
import scalac.symtab.Type;

/** This class represents an attributed field. */
public class AField extends AMember {

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public AField(Symbol symbol, boolean isStatic) {
        super(symbol, isStatic);
    }

    //########################################################################
    // Public Methods

    /** Is this field final? */
    public boolean isFinal() {
        return false; // !!!
    }

    /** Is this field volatile? */
    public boolean isVolatile() {
        return false; // !!!
    }

    /** Is this field transient? */
    public boolean isTransient() {
        return false; // !!!
    }

    /** Returns the type of this field. */
    public Type type() {
        return symbol().type();
    }

    /** Returns a string representation of this field. */
    public String toString() {
        return new ATreePrinter().printField(this).toString();
    }

    //########################################################################
}
