/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: ScalaObject.java,v 1.7 2002/07/11 11:46:53 paltherr Exp $
// $Id$

package scalai;

import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.lang.reflect.InvocationHandler;

import scalac.symtab.Symbol;

public class ScalaObject implements InvocationHandler {

    //########################################################################
    // Public Fields

    public final ScalaTemplate template;
    public final Object[] variables;

    //########################################################################
    // Public Constructors

    public ScalaObject(ScalaTemplate template, Object[] variables) {
        this.template = template;
        this.variables = variables;
    }

    //########################################################################
    // Public Methods - ScalaObject interface

    public Object invoke(Object self, Symbol symbol, Object[] args) {
        return template.invoke(self, symbol, args);
    }

    //########################################################################
    // Public Methods - InvocationHandler interface

    public Object invoke(Object self, Method method, Object[] args)
        throws Throwable
    {
        try {
            return template.invoke(self, method, args);
        } catch (EvaluatorException exception) {
            exception.addScalaEntryPoint();
            throw exception.getCause();
        }
    }

    //########################################################################
    // Public Methods - Object interface

    public boolean equals(Object that) {
        return that instanceof Proxy && Proxy.getInvocationHandler(that)==this;
    }

    public String toString() {
        return template.getName() + "@" + Integer.toHexString(hashCode());
    }

    //########################################################################
}
