/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: Interpreter.java,v 1.63 2002/09/13 01:50:30 paltherr Exp $
// $Id$

package scala.tools.scalai;

import java.util.Map;
import java.util.HashMap;

import scalac.Global;
import scalac.Phase;
import scalac.symtab.Definitions;
import scalac.symtab.Symbol;
import scalac.symtab.TermSymbol;
import scalac.symtab.Type;
import scalac.symtab.Modifiers;
import scalac.util.Name;

import scala.runtime.InterpreterSupport;
import scala.runtime.InterpreterSupport.EvaluationResult;

public class Interpreter {

    //########################################################################
    // Public Constants

    public static final Name MAIN_N = Name.fromString("main");
    public static final Name ARGS_N = Name.fromString("args");

    //########################################################################
    // Private Fields

    private final Global global;
    private final Compiler compiler;
    private final Evaluator evaluator;

    //########################################################################
    // Public Constructors

    public Interpreter(Global global) {
        this.global = global;
        Map templates = new HashMap();
        this.evaluator = new Evaluator(templates);
        this.compiler = new Compiler(global, templates, evaluator); // !!!
    }

    //########################################################################
    // Public Methods

    public EvaluatorResult invoke(String main, String[]args) {
        Symbol module = getMainModule(main);
        if (module == Symbol.NONE) return EvaluatorResult.Void;
        Symbol method = getMainMethod(main, module);
        if (method == Symbol.NONE) return EvaluatorResult.Void;
        Variable variable = compiler.getModule(module);
        Function function = compiler.getMethod(method);
        try {
            evaluator.evaluate(variable, function, new Object[] {args});
            return EvaluatorResult.Void;
        } catch (EvaluatorException exception) {
            return EvaluatorResult.Error(exception);
        }
    }

    public EvaluatorResult interpret(String input, boolean interactive) {
        if (input.trim().length() == 0) return EvaluatorResult.Void;
        global.compile("<console>", input + ";", interactive);
        return interpret(interactive);
    }

    public EvaluatorResult interpret(String[] files, boolean interactive) {
        if (files.length == 0) return EvaluatorResult.Void;
        global.compile(files, interactive);
        return interpret(interactive);
    }

    public EvaluatorResult toString(Object object, String type) {
        try {
            return EvaluatorResult.Value(evaluator.toString(object), type);
        } catch (EvaluatorException exception) {
            return EvaluatorResult.Error(exception);
        }
    }

    //########################################################################
    // Private Methods

    private EvaluatorResult interpret(boolean interactive) {
        try {
            if (global.reporter.errors() != 0) return EvaluatorResult.Void;
            compiler.compile(global.units);
            if (interactive) {
                Variable console = compiler.getModule(global.console);
                evaluator.evaluate(console);
            }
            EvaluationResult result =
                InterpreterSupport.getAndResetEvaluationResult();
            if (result == null) return EvaluatorResult.Void;
            return EvaluatorResult.Value(result.value, result.type);
        } catch (EvaluatorException exception) {
            return EvaluatorResult.Error(exception);
        }
    }

    //########################################################################
    // Private Methods - Finding main module

    private Symbol getMainModule(String main) {
        String names = main.replace('/', '.') + (main.length() > 0 ? "." : "");
        if (names.length() > 0 && names.charAt(0) == '.') {
            error("illegal module name '" + main + "'");
            return Symbol.NONE;
        }
        Symbol module = global.definitions.ROOT_CLASS;
        for (int i = 0, j; (j = names.indexOf('.', i)) >= 0; i = j + 1) {
            Name name = Name.fromString(names.substring(i, j));
            module = getModule(module, name);
            if (module == Symbol.NONE) {
                error("could not find module '" + main.substring(0, j) + "'");
                return Symbol.NONE;
            }
            if (module == Symbol.ERROR) {
                error("term '" + main.substring(0, j) + "' is not a module");
                return Symbol.NONE;
            }
        }
        return module;
    }

    private Symbol getModule(Symbol owner, Name name) {
        Symbol symbol = owner.lookup(name);
        if (symbol == Symbol.NONE || symbol.isModule()) return symbol;
        switch (symbol.type()) {
        case OverloadedType(Symbol[] alts, _):
            for (int k = 0; k < alts.length; k++)
                if (alts[k].isModule()) return alts[k];
        }
        return Symbol.ERROR;
    }

    //########################################################################
    // Private Methods - Finding main method

    private Type getMainMethodType(boolean erased) {
        Phase current = global.currentPhase;
        if (!erased) global.currentPhase = global.PHASE.ANALYZER.phase();
        Definitions definitions = global.definitions;
        Type argument = definitions.ARRAY_TYPE(definitions.STRING_TYPE());
        Type result = definitions.UNIT_TYPE();
        Symbol formal = new TermSymbol(0, ARGS_N, Symbol.NONE,Modifiers.PARAM);
        formal.setInfo(argument);
        global.currentPhase = current;
        return Type.MethodType(new Symbol[] {formal}, result);
    }

    private Symbol getMainMethod(String main, Symbol module) {
        Symbol method = getMethod(module, MAIN_N, getMainMethodType(true));
        if (method == Symbol.NONE) {
            error("module '" + main + "' has no method '" + MAIN_N + "'");
            return Symbol.NONE;
        }
        if (method == Symbol.ERROR) {
            error("module '" + main + "' has no method '" + MAIN_N +
                "' with type '" + getMainMethodType(false) + "'");
            return Symbol.NONE;
        }
        return method;
    }

    private Symbol getMethod(Symbol module, Name name, Type type) {
        Symbol symbol = module.moduleClass().lookup(name);
        if (symbol == Symbol.NONE || isMethod(symbol, type)) return symbol;
        switch (symbol.type()) {
        case OverloadedType(Symbol[] alts, _):
            for (int k = 0; k < alts.length; k++)
                if (isMethod(alts[k], type)) return alts[k];
        }
        return Symbol.ERROR;
    }

    private boolean isMethod(Symbol symbol, Type type) {
        return symbol.isMethod() && symbol.type().equals(type);
    }

    //########################################################################
    // Private Methods - Signaling errors

    private void error(String message) {
        global.reporter.error(null, message);
    }

    //########################################################################
}
