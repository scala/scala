/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.util;

/** Instances of this class represent type names. */
public final class TypeName extends Name {

    //########################################################################
    // Protected Constructors

    /** Initializes this instance. */
    protected TypeName(TermName term) {
        super(term.index + 1, term);
    }

    //########################################################################
    // Public Factories

    /** Returns the type name with given ASCII representation. */
    public static TypeName fromAscii(byte[] bytes, int start, int count) {
        return TermName.fromAscii(bytes, start, count).toTypeName();
    }

    /** Returns the type name with given string representation. */
    public static TypeName fromString(String string) {
        return TermName.fromString(string).toTypeName();
    }

    //########################################################################
}
