/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: Environment.java,v 1.13 2002/10/01 16:14:07 paltherr Exp $
// $Id$

package scalai;

import java.util.Map;
import java.util.HashMap;

import scalac.ast.Tree.ClassDef;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.symtab.Scope;
import scalac.symtab.Scope.SymbolIterator;
import scalac.util.Debug;

public class Environment {

    //########################################################################
    // Private Fields

    private final Compiler compiler;
    private final JavaMirror mirror;
    private final Map/*<Symbol,ClassDef>*/ classdefs;
    private final Map/*<Symbol,Template>*/ templates;
    private final Map/*<Symbol,Function>*/ functions;
    private final Map/*<Symbol,Variable>*/ variables;
    private final Map/*<Symbol,Override>*/ overrides;

    //########################################################################
    // Public Constructors

    public Environment(Compiler compiler, JavaMirror mirror) {
        this.compiler = compiler;
        this.mirror = mirror;
        this.classdefs = new HashMap();
        this.templates = new HashMap();
        this.functions = new HashMap();
        this.variables = new HashMap();
        this.overrides = new HashMap();
    }

    //########################################################################
    // Public Methods - insert

    public ClassDef insertClassDef(Symbol symbol, ClassDef classdef) {
        assert symbol.isType() : Debug.show(symbol);
        assert Debug.log("insert classdef", symbol);
        Object value = classdefs.put(symbol, classdef);
        assert value == null : Debug.show(symbol);
        assert !templates.containsKey(symbol) : Debug.show(symbol);
        return classdef;
    }

    private Template insertTemplate(Symbol symbol, Template template) {
        assert symbol.isType() : Debug.show(symbol);
        assert Debug.log("insert template", symbol);
        Object value = templates.put(symbol, template);
        assert !classdefs.containsKey(symbol) : Debug.show(symbol);
        assert value == null : Debug.show(symbol);
        return template;
    }

    public Function insertFunction(Symbol symbol, Function function) {
        assert symbol.isTerm() : Debug.show(symbol);
        assert Debug.log("insert function", symbol);
        Object value = functions.put(symbol, function);
        assert value == null : Debug.show(symbol);
        return function;
    }

    public Variable insertVariable(Symbol symbol, Variable variable) {
        assert symbol.isTerm() : Debug.show(symbol);
        assert Debug.log("insert variable", symbol);
        Object value = variables.put(symbol, variable);
        assert value == null : Debug.show(symbol);
        return variable;
    }

    public Override insertOverride(Symbol symbol, Override override) {
        assert symbol.isTerm() : Debug.show(symbol);
        assert Debug.log("insert override", symbol);
        Object value = overrides.put(symbol, override);
        assert value == null : Debug.show(symbol);
        return override;
    }

    //########################################################################
    // Public Methods - lookup

    public Template lookupTemplate(Symbol symbol) {
        assert symbol.isType() : Debug.show(symbol);
        Object value = templates.get(symbol);
        if (value != null) return (Template)value;
        if (symbol.isJava()) {
            Template template = Template.JavaClass(mirror.getClass(symbol));
            return insertTemplate(symbol, template);
        } else {
            return loadTemplate(symbol);
        }
    }

    public Function lookupFunction(Symbol symbol) {
        assert symbol.isTerm() : Debug.show(symbol);
        Object value = functions.get(symbol);
        if (value != null) return (Function)value;
        if (symbol.isJava()) {
            Function function = symbol.isConstructor() ?
                Function.JavaConstructor(mirror.getConstructor(symbol)) :
                Function.JavaMethod(mirror.getMethod(symbol));
            return insertFunction(symbol, function);
        } else {
            return (Function)loadOwnerThenGet("function", symbol, functions);
        }
    }

    public Variable lookupVariable(Symbol symbol) {
        assert symbol.isTerm() : Debug.show(symbol);
        Object value = variables.get(symbol);
        if (value != null) return (Variable)value;
        if (symbol.isJava()) {
            Variable variable = Variable.JavaField(mirror.getField(symbol));
            return insertVariable(symbol, variable);
        } else {
            return (Variable)loadOwnerThenGet("variable", symbol, variables);
        }
    }

    public Override lookupOverride(Symbol symbol) {
        assert symbol.isTerm() : Debug.show(symbol);
        Object value = overrides.get(symbol);
        if (value != null) return (Override)value;
        return loadOwnerOverridesThenGet(symbol);
    }

    //########################################################################
    // Private Methods - template loading

    private Object loadOwnerThenGet(String what, Symbol symbol, Map table) {
        loadOwner(what, symbol);
        Object value = table.get(symbol);
        assert value != null : Debug.show(symbol);
        return value;
    }

    private void loadOwner(String what, Symbol symbol) {
        assert Debug.log("search " + what, symbol);
        assert symbol.owner().isType() : Debug.show(symbol);
        assert!symbol.owner().isJava() : Debug.show(symbol);
        loadTemplate(symbol.owner());
    }

    private Template loadTemplate(Symbol symbol) {
        Object test1 = classdefs.remove(symbol);
        if (test1 != null) return loadTemplate(symbol, (ClassDef)test1);
        loadOwner("template", symbol);
        Object test2 = classdefs.remove(symbol);
        if (test2 != null) return loadTemplate(symbol, (ClassDef)test2);
        Object value = templates.get(symbol);
        assert value != null : Debug.show(symbol);
        return (Template)value;
    }

    private Template loadTemplate(Symbol symbol, ClassDef classdef) {
        Template template = Template.Global(compiler.load(symbol, classdef));
        return insertTemplate(symbol, template);
    }

    //########################################################################
    // Private Methods - override loading

    private Override loadOwnerOverridesThenGet(Symbol symbol) {
        assert Debug.log("search override", symbol);
        assert symbol.owner().isType() : Debug.show(symbol);
        loadTemplateOverrides(symbol.owner());
        Object value = overrides.get(symbol);
        assert value != null : Debug.show(symbol);
        return (Override)value;
    }

    private void loadTemplateOverrides(Symbol symbol) {
        Type[] bases = symbol.parents();
        SymbolIterator i = new Scope.UnloadIterator(symbol.members().iterator());
        while (i.hasNext()) loadMethodOverride(bases, i.next());
    }

    private void loadMethodOverride(Type[] bases, Symbol symbol) {
        if (!symbol.isMethod()) return;
        Override override = Override.empty().insert(symbol);
        if (symbol.isJava()) override.insert(mirror.getMethod(symbol));
        for (int i = 0; i < bases.length; i++) {
            Symbol overridden = symbol.overriddenSymbol(bases[i]);
            if (overridden == Symbol.NONE) continue;
            override.insert(lookupOverride(overridden));
        }
        insertOverride(symbol, override);
    }

    //########################################################################
}
