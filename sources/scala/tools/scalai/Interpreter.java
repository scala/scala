/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: Interpreter.java,v 1.63 2002/09/13 01:50:30 paltherr Exp $
// $Id$

package scalai;

import scalac.Global;

import scala.runtime.InterpreterSupport;
import scala.runtime.InterpreterSupport.EvaluationResult;

public class Interpreter {

    //########################################################################
    // Private Fields

    private final Global global;
    private final Compiler compiler;
    private final Evaluator evaluator;

    //########################################################################
    // Public Constructors

    public Interpreter(Global global) {
        this.global = global;
        this.evaluator = new Evaluator();
        this.compiler = new Compiler(global, evaluator); // !!!
    }

    //########################################################################
    // Public Methods

    public EvaluatorResult interpret(String input, boolean interactive) {
        if (input.trim().length() == 0) return EvaluatorResult.Void;
        global.compile(input + ";", interactive);
        return interpret(interactive);
    }

    public EvaluatorResult interpret(String[] files, boolean interactive) {
        if (files.length == 0) return EvaluatorResult.Void;
        global.compile(files, interactive);
        return interpret(interactive);
    }

    public EvaluatorResult interpret(boolean interactive) {
        try {
            if (global.reporter.errors() != 0) return EvaluatorResult.Void;
            CodeContainer code = compiler.compile(global.units, interactive);
            evaluator.evaluate(code);
            EvaluationResult result =
                InterpreterSupport.getAndResetEvaluationResult();
            if (result == null) return EvaluatorResult.Void;
            return EvaluatorResult.Value(result.value, result.type);
        } catch (EvaluatorException exception) {
            return EvaluatorResult.Error(exception);
        }
    }

    public EvaluatorResult toString(Object object, String type) {
        try {
            return EvaluatorResult.Value(evaluator.toString(object), type);
        } catch (EvaluatorException exception) {
            return EvaluatorResult.Error(exception);
        }
    }

    //########################################################################
}
