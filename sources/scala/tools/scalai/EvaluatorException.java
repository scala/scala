/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scalai;

import java.io.StringWriter;
import java.io.PrintWriter;
import java.util.List;
import java.util.ArrayList;

import scalac.Global;
import scalac.symtab.Symbol;

public class EvaluatorException extends RuntimeException {

    //########################################################################
    // Private Fields

    private final List stack;

    private Throwable cause;
    private StackTraceElement[] trace;
    private int entry;

    //########################################################################
    // Public Constructors

    public EvaluatorException() {
        this.stack = new ArrayList();
    }

    //########################################################################
    // Public Methods

    public void reset(Throwable cause) {
        this.stack.clear();
        this.cause = cause;
        this.trace = cause.getStackTrace();
        this.entry = 0;
    }

    public Throwable getCause() {
        return cause;
    }

    public void addScalaCall(String location) {
        stack.add(location);
    }

    public void addScalaEntryPoint() {
        // save new stack trace elements
        this.trace = getCurrentTrace();
        // skip calls through interpreter
        while (traceAtStartsWith(entry, "scalai.")) entry++;
        // skip calls through proxy class
        while (traceAtStartsWith(entry, "$Proxy")) entry++;
    }

    public void addScalaLeavePoint(String clasz, String method) {
        // find leave point
        int leave = entry;
        while (leave < trace.length && !traceAtEquals(leave, clasz, method))
            leave++;
        if (leave < trace.length) {
            // skip calls through reflection
            while (traceAtStartsWith(leave - 1, "java.lang.reflect.")) leave--;
            while (traceAtStartsWith(leave - 1, "sun.reflect.")) leave--;
        }
        // complete stack with java trace
        for (int i = entry; i < leave; i++) stack.add(trace[i]);
        if (leave == trace.length) stack.add("...");
        // free memory
        this.trace = null;
        this.entry = 0;
    }

    public Object[] getScalaStackTrace() {
        return stack.toArray();
    }

    public String getScalaErrorMessage(boolean withTrace) {
        StringWriter buffer = new StringWriter();
        PrintWriter writer = new PrintWriter(buffer);
        Throwable exception = getCause();
        writer.print(exception.toString());
        if (withTrace) {
            writer.println();
            for (int i = 0; i < stack.size(); i++) {
                writer.print("        at ");
                writer.println(stack.get(i));
            }
            Throwable cause = exception.getCause();
            if (cause != null) {
                writer.print("Caused by ");
                cause.printStackTrace(writer);
            }
        }
        writer.close();
        return buffer.toString();
    }

    //########################################################################
    // Private Methods

    private boolean traceAtEquals(int index, String clasz, String method) {
        if (index < 0 || trace.length <= index) return false;
        if (!trace[index].getClassName().equals(clasz)) return false;
        return method == null || trace[index].getMethodName().equals(method);
    }

    private boolean traceAtStartsWith(int index, String prefix) {
        if (index < 0 || trace.length <= index) return false;
        return trace[index].getClassName().startsWith(prefix);
    }

    private StackTraceElement[] getCurrentTrace() {
        return new RuntimeException().getStackTrace();
    }

    //########################################################################
}
