/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: Override.java,v 1.2 2002/07/11 09:20:41 paltherr Exp $
// $Id$

package scalai;

import java.lang.reflect.Method;
import java.util.Set;
import java.util.HashSet;
import java.util.Iterator;

import scalac.symtab.Symbol;
import scalac.util.Debug;

public class Override {

    //########################################################################
    // Private Fields

    private final Set/*<Method|Symbol>*/ methods;

    //########################################################################
    // Public Constructors

    private Override() {
        this.methods = new HashSet();
    }

    //########################################################################
    // Public Methods

    public static Override empty() {
        return new Override();
    }

    public Override insert(Method method) {
        methods.add(method);
        return this;
    }

    public Override insert(Symbol symbol) {
        methods.add(symbol);
        return this;
    }

    public Override insert(Override override) {
        methods.addAll(override.methods);
        return this;
    }

    public Iterator iterator() {
        return methods.iterator();
    }

    public String toString() {
        return "Override(" + Debug.show(methods.toArray()) + ")";
    }

    //########################################################################
}
