/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.ast;

/** This class enumerates the different kinds of tree nodes. */
public class TreeKind {

    //########################################################################
    // Public Cases

    /** Designates a type, a term or anything else. */
    public case Any;

    /** Designates a type. */
    public case Type;

    /** Designates a term. */
    public case Term;

    /** Designates either a type or a term. */
    public case Dual;

    /** Designates either a type or a term (a test may indicate which one). */
    public case Test;

    /** Designates neither a type nor a term. */
    public case None;

    //########################################################################
    // Public Method

    public boolean isA(TreeKind that) {
        switch (this) {
        case Any : return true;
        case Type: return that == Type;
        case Term: return that == Term;
        case Dual: return that == Type || that == Term || that == Dual;
        case Test: return that == Type || that == Term || that == Dual;
        case None: return that == None;
        default  : throw new Error();
        }
    }

    public String toString() {
        switch(this) {
        case Any : return "Any";
        case Type: return "Type";
        case Term: return "Term";
        case Dual: return "Dual";
        case Test: return "Test";
        case None: return "None";
        default  : throw new Error();
        }
    }

    //########################################################################
}
