/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scalai;

import java.io.PrintWriter;

import scala.runtime.InterpreterSupport.DefinitionPrinter;

import scalac.util.Debug;

public class InterpreterPrinter implements DefinitionPrinter {

    //########################################################################
    // Private Fields

    private final Interpreter interpreter;
    private final PrintWriter writer;

    //########################################################################
    // Public Constructors

    public InterpreterPrinter(Interpreter interpreter, PrintWriter writer) {
        this.interpreter = interpreter;
        this.writer = writer;
    }

    //########################################################################
    // Public Methods

    public void showDefinition(String signature) {
        writer.println(signature);
    }

    public void showValueDefinition(String signature, Object value) {
        EvaluatorResult result = interpreter.toString(value, null);
        switch (result) {
        case Value(Object string, _):
            writer.println(signature + " = " + string);
            writer.flush();
            return;
        case Error(EvaluatorException exception):
            writer.print(signature + " = ");
            writer.print(exception.getScalaErrorMessage(true));
            writer.flush();
            return;
        default:
            throw Debug.abort("illegal case", result);
        }
    }

    public void showResult(EvaluatorResult result, boolean interactive) {
        switch (result) {
        case Void:
            return;
        case Value(Object value, String type):
            if (interactive)
                if (value instanceof String)
                    writer.println(value + ": " + type);
                else
                    showResult(interpreter.toString(value, type), interactive);
            writer.flush();
            return;
        case Error(EvaluatorException exception):
            String name = Thread.currentThread().getName();
            writer.print("Exception in thread \"" + name + "\" ");
            writer.print(exception.getScalaErrorMessage(true));
            writer.flush();
            return;
        default:
            throw Debug.abort("illegal case", result);
        }
    }

    //########################################################################
}
