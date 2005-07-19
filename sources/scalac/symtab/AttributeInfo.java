/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2005, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab;

import scalac.atree.AConstant;
import scalac.util.Debug;

public class AttributeInfo {

    public final Symbol constr;
    public final AConstant[] args;
    public final AttributeInfo next;

    public AttributeInfo(Symbol constr, AConstant[] args, AttributeInfo next) {
        this.constr = constr;
        this.args = args;
        this.next = next;
        assert constr.isConstructor() : Debug.show(constr);
    }

    public AConstant[] getAttrArguments(Symbol sym) {
        for (AttributeInfo attr = this; attr != null; attr = attr.next)
            if (attr.constr == sym)
                return attr.args;
        return null;
    }

    public String toString() {
        StringBuffer str = new StringBuffer("[");
        for (AttributeInfo attr = this; attr != null; attr = attr.next) {
            str.append(Debug.show(attr.constr.constructorClass()));
            int n = attr.args.length;
            if (n > 0) {
                str.append('(');
                for (int i = 0; i < n; i++) {
                    if (i > 0) str.append(", ");
                    str.append(attr.args[i]);
                }
                str.append(')');
            }
            if (attr.next != null) {
                str.append(", ");
            }
        }
        str.append(']');
        return str.toString();
    }

}
