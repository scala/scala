/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime;

/** This class provides support methods for the interpreter. */
public class InterpreterSupport {

    //########################################################################
    // Public classes

    /** This interface provides method to show definitions. */
    public static interface DefinitionPrinter {

        /** This method is invoked for each non-value definition. */
        public void showDefinition(String signature);

        /** This method is invoked for each value definition. */
        public void showValueDefinition(String signature, Object value);

    }

    /** This class describes an evaluation result. */
    public static class EvaluationResult {

        /** The value of the result */
        public final Object value;

        /** The type of the result */
        public final String type;

        /** Creates a new instance */
        public EvaluationResult(Object value, String type) {
            this.value = value;
            this.type = type;
        }
    }

    //########################################################################
    // Private Variables

    private static final ThreadLocal printer = new ThreadLocal();
    private static final ThreadLocal result  = new ThreadLocal();

    //########################################################################
    // Public Functions

    /** Sets the definition printer of the current thread. */
    public static void setDefinitionPrinter(DefinitionPrinter object) {
        printer.set(object);
    }

    /** Returns the definition printer of the current thread. */
    public static DefinitionPrinter getDefinitionPrinter() {
        return (DefinitionPrinter)printer.get();
    }

    /**
     * This function is invoked for each non-value definition. It
     * forwards the call to the current thread's definition printer.
     *
     * @meta method (java.lang.String, scala.Any) scala.Unit;
     */
    public static void showDefinition(String signature) {
        DefinitionPrinter printer = getDefinitionPrinter();
        if (printer != null) printer.showDefinition(signature);
    }

    /**
     * This method is invoked for each value definition.  It forwards
     * the call to the current thread's definition printer.
     *
     * @meta method (java.lang.String, scala.Any) scala.Unit;
     */
    public static void showValueDefinition(String signature, Object value) {
        DefinitionPrinter printer = getDefinitionPrinter();
        if (printer != null) printer.showValueDefinition(signature, value);
    }

    /**
     * Sets the evaluation result of the current thread.
     *
     * @meta method (scala.Any, java.lang.String) scala.Unit;
     */
    public static void setEvaluationResult(Object value, String type) {
        result.set(new EvaluationResult(value, type));
    }

    /**
     * Returns and resets the evaluation result of the current
     * thread. A null value indicates that the last evaluation had no
     * result (only definitions).
     */
    public static EvaluationResult getAndResetEvaluationResult() {
        Object object = result.get();
        result.set(null);
        return (EvaluationResult)object;
    }

    //########################################################################
}
