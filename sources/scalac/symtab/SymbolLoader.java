/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab;

import java.io.IOException;

import scalac.Global;
import scalac.Phase;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.util.Debug;

/**
 * This class implements common behaviors of lazy types used to load
 * symbols from external sources (containing source or compiled code).
 */
public abstract class SymbolLoader extends Type.LazyType {

    //########################################################################
    // Public Fields

    /** The global environment */
    public final Global global;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public SymbolLoader(Global global) {
        this.global = global;
    }

    //########################################################################
    // Public Methods

    /**
     * Completes the specified symbol. More precisely, it completes
     * all symbols related to the root symbol of the specified
     * symbol. It is guaranteed that after this method call all these
     * symbols are initialized (or at least that their info does no
     * longer contain this lazy type).
     *
     * The root symbol of a symbol is:
     * - the root symbol of the constructed class, if it's a
     *   constructor,
     * - the root symbol of the source module, if it's a module class,
     * - the linked class, if it's a module with a linked class,
     * - itself if it's a class or a module with no linked class,
     * - undefined otherwise.
     *
     * The symbols related to a symbol include:
     * - the symbol itself,
     * - its constructor (allConstructors()), if it's a class,
     * - the symbols related to its linked module, if there is one.
     * - the symbols related to its module class, if it's a module,
     */
    public final void complete(Symbol symbol) {
        Symbol root = getRootSymbol(symbol);
        try {
            Phase phase = global.currentPhase;
            global.currentPhase = global.PHASE.ANALYZER.phase();
            global.timer.start();
            String source = doComplete(root);
            global.timer.stop("loaded " + source);
            global.currentPhase = phase;
            checkValidity(root, source);
        } catch (IOException exception) {
            global.timer.drop();
	    if (global.debug) exception.printStackTrace();
            String error = "error while loading " + symbol;
            String message = exception.getMessage();
            error = message != null ? error + ", " + message : "i/o " + error;
            global.error(error);
        }
        initializeRoot(root);
    }

    //########################################################################
    // Protected Methods

    /**
     * Performs the actual loading and returns the name of the
     * external source. It is guaranteed that the argument of this
     * method is always a root symbol
     *
     * @see complete(Symbol)
     */
    protected abstract String doComplete(Symbol root) throws IOException;

    //########################################################################
    // Private Methods

    /**
     * Returns the root symbol of the specified symbol.
     *
     * @see complete(Symbol)
     */
    private Symbol getRootSymbol(Symbol symbol) {
        if (symbol.isConstructor())
            return getRootSymbol(symbol.constructorClass());
        if (symbol.isModuleClass())
            return getRootSymbol(symbol.sourceModule());
        if (symbol.isModule() && symbol.linkedClass() != null)
            return symbol.linkedClass();
        assert symbol.isClassType() || symbol.isModule(): Debug.show(symbol);
        return symbol;
    }

    /**
     * Checks that at least the specified root symbol or its linked
     * module, if any, has been initialized and signals an error
     * otherwise.
     *
     * @see complete(Symbol)
     */
    private void checkValidity(Symbol root, String source) {
        if (root.rawInfo() != this) return;
        String what;
        if (!root.isClassType() || root.linkedModule() == null) {
            what = "does not define " + root;
        } else {
            if (root.linkedModule().moduleClass().rawInfo() != this) return;
            what = "defines neither " + root + " nor " + root.linkedModule();
        }
        global.error(source + " " + what);
    }

    /**
     * Initializes all symbols related to the specified root symbol
     * and whose info is this instance.
     *
     * @see complete(Symbol)
     */
    private void initializeRoot(Symbol root) {
        if (root.isClassType()) {
            initializeClass(root);
            if (root.linkedModule() != null)
                initializeRoot(root.linkedModule());
        } else {
            initializeSymbol(root);
            if (root.isModule()) initializeClass(root.moduleClass());
        }
    }

    /**
     * Initializes the specified class and its constructor if their
     * info is this instance.
     */
    private void initializeClass(Symbol clasz) {
        initializeSymbol(clasz);
        initializeSymbol(clasz.allConstructors());
    }

    /** Initializes the symbol if its info is this instance. */
    private void initializeSymbol(Symbol symbol) {
        if (symbol.rawInfo() != this) return;
        symbol.setInfo(symbol.isModule() ? Type.NoType : Type.ErrorType);
        if (symbol.isConstructor()) symbol.flags |= Modifiers.PRIVATE;
    }

    //########################################################################
}
