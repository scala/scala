/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: Function.java,v 1.5 2002/07/01 13:16:39 paltherr Exp $
// $Id$

package scalai;

import java.lang.reflect.Method;
import java.lang.reflect.Constructor;

import scalac.symtab.Symbol;
import scalac.util.Debug;

public class Function {

    //########################################################################
    // Public Cases

    public case Global(CodePromise code);
    public case Member(Symbol symbol);
    public case Label(Symbol symbol);

    public case JavaConstructor(Constructor constructor);
    public case JavaMethod(Method method);

    public case Pos;
    public case Neg;
    public case Throw;
    public case StringPlus;
    public case EqEq;
    public case BangEq;
    public case HashCode;
    public case ToString;

    //########################################################################
    // Public Methods

    public String toString() {
        switch (this) {

        case Global(CodePromise code):
            return "Global(" + code + ")";

        case Member(Symbol symbol):
            return "Member(" + Debug.show(symbol) + ")";

        case Label(Symbol symbol):
            return "Label(" + Debug.show(symbol) + ")";

        case JavaMethod(Method method):
            return "JavaMethod(" + method + ")";

        case JavaConstructor(Constructor constructor):
            return "JavaConstructor(" + constructor + ")";

        case Pos:
            return "Pos";

        case Neg:
            return "Neg";

        case Throw:
            return "Throw";

        case StringPlus:
            return "StringPlus";

        case EqEq:
            return "EqEq";

        case BangEq:
            return "BangEq";

        case HashCode:
            return "HashCode";

        case ToString:
            return "ToString";

        default:
            throw Debug.abort("illegal function", this);
        }
    }

    //########################################################################
}
