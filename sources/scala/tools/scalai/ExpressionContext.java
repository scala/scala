/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: ExpressionContext.java,v 1.2 2002/06/05 09:05:56 paltherr Exp $
// $Id$

package scala.tools.scalai;

import java.util.Map;
import java.util.HashMap;

import ch.epfl.lamp.util.SourceFile;

import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.util.Debug;

public class ExpressionContext {

    //########################################################################
    // Private Fields

    private final Environment environment;
    private final Map functions;
    private final Map variables;
    private final SourceFile source;
    private final Symbol owner;

    private int current;
    private int maximum;

    //########################################################################
    // Public Constructors

    public ExpressionContext(
        Environment environment, SourceFile source, Symbol owner)
    {
        this.environment = environment;
        this.functions = new HashMap();
        this.variables = new HashMap();
        this.source = source;
        this.owner = owner;
        this.current = 0;
        this.maximum = 0;
    }

    //########################################################################
    // Public Methods - translate

    public Class getClass(Symbol symbol) {
        return environment.getClass(symbol);
    }

    public Class getClass(Type type) {
        return environment.getClass(type);
    }

    //########################################################################
    // Public Methods - insert

    public Function insertLabel(Symbol symbol) {
        Function function = Function.Label(symbol);
        assert Debug.log("insert label   : ", symbol);
        assert !functions.containsKey(symbol) : Debug.show(symbol);
        functions.put(symbol, function);
        return function;
    }

    public Variable insertVariable(Symbol symbol, Variable variable) {
        assert Debug.log("insert variable: ", symbol);
        assert !variables.containsKey(symbol) : Debug.show(symbol);
        variables.put(symbol, variable);
        return variable;
    }

    //########################################################################
    // Public Methods - lookup

    public Template lookupTemplate(Symbol symbol) {
        return environment.lookupTemplate(symbol);
    }

    public Function lookupFunction(Symbol symbol) {
        Object value = functions.get(symbol);
        if (value != null) return (Function)value;
        return environment.lookupFunction(symbol);
    }

    public Variable lookupVariable(Symbol symbol) {
        Object value = variables.get(symbol);
        if (value != null) return (Variable)value;
        return environment.lookupVariable(symbol);
    }

    //########################################################################
    // Public Methods - origin

    public SourceFile source() {
        return source;
    }

    public Symbol owner() {
        return owner;
    }

    //########################################################################
    // Public Methods - stack

    public void stacksize(int size) {
        assert size >= 0 : size;
        maximum = Math.max(current, maximum);
        current = size;
    }

    public int stacksize() {
        return current;
    }

    public int stackmax() {
        return maximum = Math.max(current, maximum);
    }

    public int push() {
        return current++;
    }

    public int pop() {
        maximum = Math.max(current, maximum);
        return --current;
    }

    //########################################################################
}
