/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: ScalaTemplate.java,v 1.8 2002/07/11 11:46:53 paltherr Exp $
// $Id$

package scala.tools.scalai;

import java.lang.reflect.Method;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;

import scalac.util.Name;
import scalac.symtab.Symbol;
import scalac.util.Debug;

public class ScalaTemplate {

    //########################################################################
    // Private Fields

    private final Evaluator evaluator;
    private final Symbol symbol;
    private final Class proxy;
    private final Function constructor;
    private final Map/*<Method|Symbol,CodePromise>*/ vtable;
    private final Object[] fields;

    //########################################################################
    // Public Constructors

    public ScalaTemplate(Evaluator evaluator, Symbol symbol, Class proxy, Function constructor, Map vtable, Object[] fields) {
        this.evaluator = evaluator;
        this.symbol = symbol;
        this.proxy = proxy;
        this.constructor = constructor;
        this.vtable = vtable;
        this.fields = fields;
    }

    //########################################################################
    // Public Methods - ScalaTemplate interface

    public Name getName() {
        return symbol.fullName();
    }

    public Symbol getSymbol() {
        return symbol;
    }

    public Class getProxy() {
        return proxy;
    }

    public Function getConstructor() {
        return constructor;
    }

    public Map getMethods() {
        return new HashMap(vtable);
    }

    public List getFields() {
        List list = new ArrayList(fields.length);
        for (int i = 0; i < fields.length; i++) list.add(fields[i]);
        return list;
    }

    public CodePromise getMethod(Symbol symbol) {
        return (CodePromise)vtable.get(symbol);
    }

    public ScalaObject getHandler() {
        Object[] fields = new Object[this.fields.length];
        System.arraycopy(this.fields, 0, fields, 0, fields.length);
        return new ScalaObject(this, fields);
    }

    public Object invoke(Object self, Symbol method, Object[] args) {
        CodePromise code = (CodePromise)vtable.get(method);
        if (code == null) {
            // !!! generalize use of overridingSymbol in interpreter
            code = (CodePromise)vtable.get(method.overridingSymbol(symbol.thisType(), true));
        }
        assert code != null : Debug.show(symbol) + "->" + Debug.show(method);
        return evaluator.evaluate(code, self, args);
    }

    public Object invoke(Object self, Method method, Object[] args) {
        CodePromise code = (CodePromise)vtable.get(method);
        assert code != null : Debug.show(symbol) + "->" + Debug.show(method);
        return evaluator.evaluate(code, self, args);
    }

    //########################################################################
    // Public Methods - Object interface

    public String toString() {
        return "template " + getName();
    }

    //########################################################################
}
