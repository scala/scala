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
     * Completes the symbol. More precisely, it completes all related
     * symbols of the main class of the symbol. It is guaranteed that
     * after this method call all these symbols are initialized (or at
     * least that their info does not contain this lazy type).
     *
     * The main class of a symbol is:
     * - the main class of the constructed class, if it's a
     *   constructor,
     * - the main class of the module class, if it's a module,
     * - the dual class, if it's a dual module class,
     * - itself if it's a non-dual class or a non-module class,
     * - undefined otherwise.
     *
     * The related symbols of a class include:
     * - the class itself,
     * - its constructor (symbol returned by allConstructors()),
     * - its module, if it has one,
     * - the related symbols of its dual class, if it's dual
     *   non-module class.
     */
    public final void complete(Symbol symbol) {
        Symbol clasz = getMainClass(symbol);
        try {
            long start = System.currentTimeMillis();
            Phase phase = global.currentPhase;
            global.currentPhase = global.PHASE.ANALYZER.phase();
            String source = doComplete(clasz);
            global.currentPhase = phase;
            long end = System.currentTimeMillis();
            global.operation("loaded " + source + " in " + (end-start) + "ms");
            checkValidity(clasz, source, symbol);
        } catch (IOException exception) {
	    if (global.debug) exception.printStackTrace();
            String error = "error while loading " + symbol;
            String message = exception.getMessage();
            error = message != null ? error + ", " + message : "i/o " + error;
            global.error(error);
        }
        initializeAll(clasz);
    }

    //########################################################################
    // Protected Methods

    /**
     * Performs the actual loading and returns the name of the
     * external source. It is guaranteed that the argument of this
     * method is always a main class (see also method complete).
     */
    protected abstract String doComplete(Symbol clasz) throws IOException;

    //########################################################################
    // Private Methods

    /** Returns the main class of the symbol (see method complete). */
    private Symbol getMainClass(Symbol symbol) {
        if (symbol.isConstructor())
            return getMainClass(symbol.constructorClass());
        if (symbol.isModule())
            return getMainClass(symbol.moduleClass());
        assert symbol.isClassType(): Debug.show(symbol);
        if (!symbol.isModuleClass()) return symbol;
        return symbol.dualClass().isNone() ? symbol : symbol.dualClass();
    }

    /**
     * Checks that at least the class or its dual class have been
     * initialized and signals an error otherwise.
     */
    private void checkValidity(Symbol clasz, String source, Symbol s) {
        if (clasz.rawInfo() != this) return;
        String what;
        if (clasz.dualClass().isNone()) {
            what = "does not define " + clasz;
        } else {
            if (clasz.dualClass().rawInfo() != this) return;
            Symbol module = clasz.dualClass().module();
            what = "defines neither " + clasz + " nor " + module;
        }
        global.error(source + " " + what);
    }

    /**
     * Initializes all related symbols of the class whose info is this
     * instance (see also method complete).
     */
    private void initializeAll(Symbol clasz) {
        initializeOne(clasz);
        initializeOne(clasz.allConstructors());
        if (clasz.isModuleClass()) initializeOne(clasz.module());
        else if (!clasz.dualClass().isNone()) initializeAll(clasz.dualClass());
    }

    /** Initializes the symbol if its info is this instance. */
    private void initializeOne(Symbol symbol) {
        if (symbol.rawInfo() != this) return;
        symbol.setInfo(symbol.isModule() ? Type.NoType : Type.ErrorType);
        if (symbol.isConstructor()) symbol.flags |= Modifiers.PRIVATE;
    }

    //########################################################################
}
