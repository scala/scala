/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: Variable.java,v 1.4 2002/06/17 12:33:38 paltherr Exp $
// $Id$

package scalai;

import java.lang.reflect.Field;

import scalac.util.Debug;

public class Variable {

    //########################################################################
    // Public Cases

    public case Global(Object value);
    public case Module(CodePromise body, Object value);
    public case Member(int index);
    public case Argument(int index);
    public case Local(int index);
    public case JavaField(Field field);

    //########################################################################
    // Public Methods

    public String toString() {
        switch (this) {

        case Global(Object value):
            return "Global(" + Debug.show(value) + ")";

        case Module(CodePromise body, Object value):
            return "Module(" + body + "," + Debug.show(value) + ")";

        case Member(int index):
            return "Member(" + index + ")";

        case Argument(int index):
            return "Context(" + index + ")";

        case Local(int index):
            return "Variable(" + index + ")";

        case JavaField(Field field):
            return "Java(" + field + ")";

        default:
            throw Debug.abort("illegal variable", this);
        }
    }

    //########################################################################
}
