/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalai;

import java.util.List;
import java.util.ArrayList;

import scalac.Global;
import scalac.ast.parser.Sourcefile;
import scalac.symtab.Symbol;
import scalac.util.Position;

public class EvaluatorException extends RuntimeException {

    //########################################################################
    // Private Fields

    private final StackTraceElement[] trace;
    private final List stack;

    private int stop;

    //########################################################################
    // Public Constructors

    public EvaluatorException(Throwable cause) {
        super(cause);
        this.trace = getTraceOf(cause);
        this.stack = new ArrayList();
        this.stop = trace.length;
    }

    //########################################################################
    // Public Methods

    public void addScalaCall(Symbol method, int pos) {
        StringBuffer buffer = new StringBuffer();
        buffer.append(method.owner().fullNameString());
        buffer.append('.');
        buffer.append(method.nameString());
        buffer.append('(');
        buffer.append(Sourcefile.files[Position.file(pos)]);
        buffer.append(':');
        buffer.append(Position.line(pos));
        buffer.append(")");
        stack.add(buffer.toString());
    }

    public void addScalaEntryPoint() {
        StackTraceElement[] current = getCurrentTrace();
        // find entry point
        int length = Math.min(trace.length, current.length);
        int entry = 0;
        while (entry < length && trace[entry].equals(current[entry])) entry++;
        assert entry <= stop : "entry = " + entry + " > " + stop + " = stop";
        // skip calls through proxy class
        while (entry > 0 && traceAtStartsWith(entry - 1, "$Proxy")) entry--;
        // save new trace end
        stop = entry;
    }

    public void addScalaLeavePoint() {
        StackTraceElement[] current = getCurrentTrace();
        // find leave point
        int length = Math.min(trace.length, current.length);
        int leave = 0;
        while (leave < length && trace[leave].equals(current[leave])) leave++;
        // skip calls through interpreter & reflection
        int start = leave;
        while (traceAtStartsWith(start, "scalai.")) start++;
        while (traceAtStartsWith(start, "java.lang.reflect.")) start++;
        while (traceAtStartsWith(start, "sun.reflect.")) start++;
        // complete stack with java trace
        for (int i = stop - 1; start <= i; i--) stack.add(trace[i]);
        // save new trace end
        stop = leave;
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
        if (trace.length <= index) return false;
        return trace[index].getClassName().startsWith(prefix);
    }

    private StackTraceElement[] getCurrentTrace() {
        return getTraceOf(new Error());
    }

    private StackTraceElement[] getTraceOf(Throwable exception) {
        StackTraceElement[] trace = exception.getStackTrace();
        for (int i = 0; i < trace.length / 2; i++) {
            StackTraceElement element = trace[i];
            trace[i] = trace[trace.length - i - 1];
            trace[trace.length - i - 1] = element;
        }
        return trace;
    }

    //########################################################################
}
