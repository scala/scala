/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalai;

import java.util.List;
import java.util.ArrayList;

import ch.epfl.lamp.util.Position;

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

    public void addScalaCall(Symbol method, int pos) {
        StringBuffer buffer = new StringBuffer();
        buffer.append(method.owner().fullNameString());
        buffer.append('.');
        buffer.append(method.nameString());
        buffer.append('(');
        buffer.append(Position.file(pos));
        buffer.append(':');
        buffer.append(Position.line(pos));
        buffer.append(")");
        stack.add(buffer);
    }

    public void addScalaEntryPoint() {
        // save new stack trace elements
        this.trace = getCurrentTrace();
        // skip calls through interpreter
        while (traceAtStartsWith(entry, "scalai.")) entry++;
        // skip calls through proxy class
        while (traceAtStartsWith(entry, "$Proxy")) entry++;
    }

    public void addScalaLeavePoint() {
        // find leave point
        int leave = entry;
        while (leave < trace.length && !traceAtStartsWith(leave, "scalai."))
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
        String line = System.getProperty("line.separator", "\n");
        StringBuffer buffer = new StringBuffer();
        buffer.append(getCause().toString());
        if (withTrace)
            for (int i = 0; i < stack.size(); i++)
                buffer.append(line).append("        at ").append(stack.get(i));
        return buffer.toString();
    }

    //########################################################################
    // Private Methods

    private boolean traceAtStartsWith(int index, String prefix) {
        if (index < 0 || trace.length <= index) return false;
        return trace[index].getClassName().startsWith(prefix);
    }

    private StackTraceElement[] getCurrentTrace() {
        return new RuntimeException().getStackTrace();
    }

    //########################################################################
}
