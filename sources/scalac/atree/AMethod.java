/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.symtab.Symbol;
import scalac.symtab.Type;

/** This class represents an attributed method. */
public class AMethod extends AMember {

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public AMethod(Symbol symbol, boolean isStatic) {
        super(symbol, isStatic);
    }

    //########################################################################
    // Public Methods

    /** Is this method final? */
    public boolean isFinal() {
        return symbol().isMethodFinal();
    }

    /** Is this method synchronized? */
    public boolean isSynchronized() {
        return false; // !!!
    }

    /** Is this method native? */
    public boolean isNative() {
        return false; // !!!
    }

    /** Is this method abstract? */
    public boolean isAbstract() {
        return symbol().isDeferred();
    }

    /** Is this method FP-strict? */
    public boolean isStrictFP() {
        return false; // !!!
    }

    /** Returns the type parameters of this method. */
    public Symbol[] tparams() {
        return symbol().type().typeParams();
    }

    /** Returns the value parameters of this method. */
    public Symbol[] vparams() {
        return symbol().type().valueParams();
    }

    /** Returns the result type of this method. */
    public Type result() {
        return symbol().type().resultType();
    }

    /** Returns a string representation of this method. */
    public String toString() {
        return new ATreePrinter().printMethod(this).toString();
    }

    //########################################################################
}
