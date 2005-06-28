/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.util;

import scala.tools.util.debug.Debugger;
import scala.tools.util.debug.ToStringDebugger;

import scalac.Global;
import scalac.ast.Tree;
import scalac.symtab.Scope;
import scalac.symtab.Symbol;
import scalac.symtab.Type;

/**
 * Debugging class, used e.g. to obtain string representations of
 * compiler data structures that are not "pretty-printed" and thus
 * easier to relate to the source code.
 *
 * All methods are static to be easily useable in any context.
 *
 * @author Michel Schinz
 * @version 1.0
 */
public class Debug extends scala.tools.util.debug.Debug {

    //########################################################################
    // Private Initialization

    /**
     * Forces the initialization of this class. Returns the boolean
     * value true so that it can be invoked from an assert statement.
     */
    public static boolean initialize() {
        // nothing to do, everything is done in the static initializer
        return true;
    }

    static {
        addDebugger(new ToStringDebugger(Tree.class));
        addDebugger(new ToStringDebugger(Type.class));
        addDebugger(SymbolDebugger.object);
        addDebugger(ScopeDebugger.object);
    }

    //########################################################################
    // Public Methods - Logging

    public static boolean log(Object a) {
        return logAll(new Object[] {a});
    }

    public static boolean log(Object a, Object b) {
        return logAll(new Object[] {a, b});
    }

    public static boolean log(Object a, Object b, Object c) {
        return logAll(new Object[] {a, b, c});
    }

    public static boolean log(Object a, Object b, Object c, Object d) {
        return logAll(new Object[] {a, b, c, d});
    }

    public static boolean logAll(Object[] args) {
        return Global.instance.log(showAll(args, null));
    }

    //########################################################################
}

/** This class implements a debugger for symbols. */
public class SymbolDebugger implements Debugger {

    //########################################################################
    // Public Constants

    /** The unique instance of this class. */
    public static final SymbolDebugger object = new SymbolDebugger();

    //########################################################################
    // Protected Constructors

    /** Initializes this instance. */
    protected SymbolDebugger() {}

    //########################################################################
    // Public Methods

    public boolean canAppend(Object object) {
        return object instanceof Symbol;
    }

    public void append(StringBuffer buffer, Object object) {
        Symbol symbol = (Symbol)object;
        if (!symbol.isNone() && !symbol.owner().isRoot() && !symbol.isRoot()) {
            Debug.append(buffer, symbol.owner());
            buffer.append(".");
        }
        buffer.append(symbol.name);
        if (Global.instance.uniqid) {
            buffer.append('#');
            buffer.append(symbol.id);
        }
        if (symbol.isConstructor()) {
            buffer.append('(');
            buffer.append(symbol.constructorClass().name);
            buffer.append(')');
        }
    }

    //########################################################################
}

/** This class implements a debugger for scopes. */
public class ScopeDebugger implements Debugger {

    //########################################################################
    // Public Constants

    /** The unique instance of this class. */
    public static final ScopeDebugger object = new ScopeDebugger();

    //########################################################################
    // Protected Constructors

    /** Initializes this instance. */
    protected ScopeDebugger() {}

    //########################################################################
    // Public Methods

    public boolean canAppend(Object object) {
        return object instanceof Scope;
    }

    public void append(StringBuffer buffer, Object object) {
        Scope scope = (Scope)object;
        buffer.append('{');
        for (Scope.SymbolIterator i = scope.iterator(); i.hasNext();) {
            Debug.append(buffer, i.next());
            if (i.hasNext()) buffer.append(',');
        }
        buffer.append('}');
    }

    //########################################################################
}
