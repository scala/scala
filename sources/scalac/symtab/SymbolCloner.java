/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab;

import java.util.Map;
import java.util.HashMap;

import scalac.util.FreshNameCreator;
import scalac.util.Debug;

/**
 * This class implements a symbol cloner. It automatically determines
 * the new owner of cloned symbols, clones their type and keeps track
 * of all cloned symbols. Clone a type means clone all symbol declared
 * in that type (for example parameters of a MethodType).
 * !!! The type cloning is not yet enabled.
 */
public class SymbolCloner {

    //########################################################################
    // Public Fields

    /** The fresh name generator used to rename cloned symbols */
    public final FreshNameCreator renamer;

    /** A table that maps owners of symbols to owners of cloned symbols */
    public final Map/*<Symbol,Symbol*/ owners;

    /** A map<cloned,clone> into which cloned symbols are stored */
    public final Map/*<Symbol,Symbol*/ clones;

    //########################################################################
    // Public Constructor

    /** Initializes a new instance. */
    public SymbolCloner(FreshNameCreator renamer) {
        this(renamer, new HashMap());
    }

    /** Initializes a new instance. */
    public SymbolCloner(FreshNameCreator renamer, Map owners) {
        this(renamer, owners, new HashMap());
    }

    /** Initializes a new instance. */
    public SymbolCloner(FreshNameCreator renamer, Map owners, Map clones) {
        this.renamer = renamer;
        this.owners = owners;
        this.clones = clones;
    }

    //########################################################################
    // Public Methods - Cloning symbols

    /**
     * Returns the owner for the clone of the given symbol. The
     * default implementation returns the clone of the symbol's owner
     * if that owner has been cloned or else returns the owner
     * associated to the symbol's owner in the owner table.
     */
    public Symbol getOwnerFor(Symbol symbol) {
        Symbol oldowner = symbol.owner();
        Object newowner = clones.get(oldowner);
        if (newowner == null) newowner = owners.get(oldowner);
        assert newowner != null : Debug.show(symbol);
        return (Symbol)newowner;
    }

    /** Clones the given symbol. */
    public Symbol cloneSymbol(Symbol symbol) {
        return cloneSymbol(symbol, false);
    }

    /** Clones the given symbol and renames it if rename is true. */
    public Symbol cloneSymbol(Symbol symbol, boolean rename) {
        assert !symbol.isPrimaryConstructor() : Debug.show(symbol);
        assert !symbol.isModuleClass() : Debug.show(symbol);
        assert !symbol.isClass() : Debug.show(symbol);
        assert !symbol.isModule() : Debug.show(symbol);
        assert !owners.containsKey(symbol) : Debug.show(symbol);
        assert !clones.containsKey(symbol) :
            Debug.show(symbol) + " -> " + Debug.show(clones.get(symbol));
        Symbol clone = symbol.cloneSymbol(getOwnerFor(symbol));
        if (rename) {
	    assert !symbol.isConstructor() : Debug.show(symbol);
	    clone.name = renamer.newName(symbol.name);
	}
        clones.put(symbol, clone);
        // !!! clone.setType(cloneType(clone.info()));
        return clone;
    }

    /** Clones the given symbols. */
    public Symbol[] cloneSymbols(Symbol[] symbols) {
        return cloneSymbols(symbols, false);
    }

    /** Clones the given symbols and renames them if rename is true. */
    public Symbol[] cloneSymbols(Symbol[] symbols, boolean rename) {
        if (symbols.length == 0) return Symbol.EMPTY_ARRAY;
        Symbol[] clones = new Symbol[symbols.length];
        for (int i = 0; i < clones.length; i++)
            clones[i] = cloneSymbol(symbols[i], rename);
        return clones;
    }

    /** Clones the given type. */
    public Type cloneType(Type type) {
        switch (type) {
        case PolyType(Symbol[] tparams, Type result):
            Symbol[] clones = cloneSymbols(tparams);
            Type clone = Type.PolyType(clones, cloneType(result));
            return Type.getSubst(tparams, clones).applyParams(clone);
        case MethodType(Symbol[] vparams, Type result):
            return Type.MethodType(cloneSymbols(vparams), cloneType(result));
        default:
            return type;
        }
    }

    //########################################################################
}
