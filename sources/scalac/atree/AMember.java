/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.symtab.Symbol;

/** This class represents an attributed class member. */
public abstract class AMember {

    //########################################################################
    // Private Fields

    /** The member symbol */
    private final Symbol symbol;

    /** The static flag */
    private final boolean isStatic;

    /** The member code */
    private ACode code;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public AMember(Symbol symbol, boolean isStatic) {
        this.symbol = symbol;
        this.isStatic = isStatic;
        this.code = ACode.Void;
    }

    //########################################################################
    // Public Methods

    /** Returns the symbol of this member. */
    public Symbol symbol() {
        return symbol;
    }

    /** Is this member public? */
    public boolean isPublic() {
        return symbol().isPublic();
    }

    /** Is this member private? */
    public boolean isPrivate() {
        return symbol().isPrivate();
    }

    /** Is this member protected? */
    public boolean isProtected() {
        return symbol().isProtected();
    }

    /** Is this member static? */
    public boolean isStatic() {
        return isStatic;
    }

    /** Is this member deprecated? */
    public boolean isDeprecated() {
        return false; // !!!
    }

    /** Is this member synthetic? */
    public boolean isSynthetic() {
        return symbol().isSynthetic();
    }

    /** Returns the member code. */
    public ACode code() {
        return code;
    }

    /** Sets the member code. */
    public void setCode(ACode code) {
        this.code = code;
    }

    //########################################################################
}
