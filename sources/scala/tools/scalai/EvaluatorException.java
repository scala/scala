/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalai;

import java.util.Stack;

import scalac.Global;
import scalac.ast.parser.Sourcefile;
import scalac.symtab.Symbol;
import scalac.util.Position;

public class EvaluatorException extends RuntimeException {

    //########################################################################
    // Private Fields

    private final Throwable exception;
    private final Throwable cut;
    private final Stack stack; // !!! rename ?
    private final Stack symbols;

    //########################################################################
    // Public Constructors

    public EvaluatorException(Throwable exception) {
        this(exception, null);
    }

    public EvaluatorException(Throwable exception, Throwable cut) {
        this.exception = exception;
        this.cut = cut;
        this.stack = new Stack();
        this.symbols = new Stack();
    }

    //########################################################################
    // Public Methods

    public void addCall(int pos, Symbol symbol) {
        stack.push(new Integer(pos));
        symbols.push(symbol);
    }

    public String mkString(Global global) {
        StringBuffer buffer = new StringBuffer();
        // !!! if (exception != null)buffer.append(Strings.stackTrace(exception));
        buffer.append(exception.toString());
        for (int i = 0; i < stack.size(); i++) {
            buffer.append('\n');
            int pos = ((Integer)stack.elementAt(i)).intValue();
            buffer.append("        at " +
                context(global, (Symbol)symbols.elementAt(i)) +
                "(" + Sourcefile.files[Position.file(pos)] + ":" +
                Position.line(pos) + ")");
        }
        return buffer.toString();
    }

    // !!! remove ? modify ?
    public String toString() {
        return getClass().getName() + ": " + exception;
    }

    //########################################################################
    // Private Methods

    private String context(Global global, Symbol symbol) {
        if (symbol == null) return "<top-level>";
        assert !symbol.isClass() : symbol.defString();
        Symbol owner = symbol.owner();
        if (owner == global.definitions.ROOT_CLASS
            || symbol.owner() == Symbol.NONE) // !!! || ...
        {
            return symbol.name.toString();
        }
        return owner.fullName() + "." + symbol.name;
    }

    //########################################################################
}
