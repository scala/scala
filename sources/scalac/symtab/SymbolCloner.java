/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab;

import java.util.Map;
import java.util.HashMap;

import scalac.util.Debug;

/**
 * This class implements a symbol cloner. It automatically determines
 * the new owner of cloned symbols, clones their type and keeps track
 * of all cloned symbols. Clone a type means clone all symbol declared
 * in that type (for example parameters of a MethodType).
 */
public class SymbolCloner {

    //########################################################################
    // Public Fields

    /** A table that maps owners of symbols to owners of cloned symbols */
    public final Map/*<Symbol,Symbol*/ owners;

    /** A map<cloned,clone> into which cloned symbols are stored */
    public final Map/*<Symbol,Symbol*/ clones;

    //########################################################################
    // Public Constructor

    /** Initializes a new instance. */
    public SymbolCloner() {
        this(new HashMap());
    }

    /** Initializes a new instance. */
    public SymbolCloner(Map owners) {
        this(owners, new HashMap());
    }

    /** Initializes a new instance. */
    public SymbolCloner(Map owners, Map clones) {
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

    /** Clones the given symbol but not its type. */
    public Symbol cloneSymbolWithoutType(Symbol symbol) {
        assert !symbol.isPrimaryConstructor(): Debug.show(symbol);
        assert !symbol.isClassType() || symbol.isCompoundSym(): Debug.show(symbol); // !!! isCompoundSym()
        assert !owners.containsKey(symbol): Debug.show(symbol);
        assert !clones.containsKey(symbol):
            Debug.show(symbol) + " -> " + Debug.show(clones.get(symbol));
        Symbol clone = symbol.cloneSymbol(getOwnerFor(symbol));
        clones.put(symbol, clone);
        return clone;
    }

    /** Clones the given symbols but not their types. */
    public Symbol[] cloneSymbolsWithoutTypes(Symbol[] symbols) {
        if (symbols.length == 0) return Symbol.EMPTY_ARRAY;
        Symbol[] clones = new Symbol[symbols.length];
        for (int i = 0; i < clones.length; i++)
            clones[i] = cloneSymbolWithoutType(symbols[i]);
        return clones;
    }

    /** Clones the given scope but not the type of its members. */
    public Scope cloneScopeWithoutTypes(Scope scope) {
        Scope clone = new Scope();
        for (Scope.SymbolIterator i = scope.iterator(true); i.hasNext(); ) {
            clone.enterOrOverload(cloneSymbolWithoutType(i.next()));
        }
        return clone;
    }

    /** Clones the given symbol and its type. */
    public Symbol cloneSymbol(Symbol symbol) {
        Symbol clone = cloneSymbolWithoutType(symbol);
        clone.setType(cloneType(symbol.info()));
        return clone;
    }

    /** Clones the given symbols and their types. */
    public Symbol[] cloneSymbols(Symbol[] symbols) {
        Symbol[] clones = cloneSymbolsWithoutTypes(symbols);
        for (int i = 0; i < clones.length; i++)
            clones[i].setType(cloneType(symbols[i].info()));
        return clones;
    }

    /** Clones the given scope and the type of its members. */
    public Scope cloneScope(Scope scope) {
        Scope clone = cloneScopeWithoutTypes(scope);
        for (Scope.SymbolIterator i = scope.iterator(true); i.hasNext(); ) {
            Symbol member = i.next();
            member.setType(cloneType(member.info()));
        }
        return clone;
    }

    /** Clones the given type. */
    public Type cloneType(Type type) {
        return cloner.apply(type);
    }

    //########################################################################
    // Public Methods - Mapping symbols

    /**
     * Returns the clone of the specified symbol if it has been cloned
     * and the specified symbol otherwise.
     */
    public Symbol mapSymbol(Symbol symbol) {
        Object clone = clones.get(symbol);
        return clone != null ? (Symbol)clone : symbol;
    }

    /** Replaces all cloned symbols by clones in given type. */
    public Type mapType(Type type) {
        return mapper.apply(type);
    }

    //########################################################################
    // Private Method

    private Symbol getCompoundClone(Symbol symbol) {
        assert symbol.isCompoundSym(): Debug.show(symbol);
        assert !owners.containsKey(symbol): Debug.show(symbol);
        assert !clones.containsKey(symbol):
            Debug.show(symbol) + " -> " + Debug.show(clones.get(symbol));
        Symbol owner = (Symbol)clones.get(symbol.owner());
        if (owner == null) owner = (Symbol)owners.get(symbol.owner());
        if (owner == null) owner = symbol.owner();
        Symbol clone = symbol.cloneSymbol(owner);
        clones.put(symbol, clone);
        return clone;
    }

    //########################################################################
    // Private Class - Type mapper

    /** The type mapper Type.Map */
    private final Type.Map mapper = new TypeMapper();
    private class TypeMapper extends Type.Map { public Type apply(Type type) {
        switch (type) {
        case ErrorType:
        case NoType:
        case NoPrefix:
            return type;
        case ThisType(Symbol symbol):
            Symbol clone = (Symbol)clones.get(symbol);
            if (clone == null) return type;
            return Type.ThisType(clone);
        case SingleType(Type prefix, Symbol symbol):
            Symbol clone = (Symbol)clones.get(symbol);
            if (clone == null) return map(type);
            return Type.singleType(apply(prefix), clone);
        case ConstantType(_, _):
            return map(type);
        case TypeRef(Type prefix, Symbol symbol, Type[] args):
            Symbol clone = (Symbol)clones.get(symbol);
            if (clone == null) return map(type);
            return Type.typeRef(apply(prefix), clone, map(args));
        case CompoundType(Type[] parts, Scope members):
            Symbol clone = (Symbol)clones.get(type.symbol());
            // !!! if (clone == null) return map(type);
            if (clone == null) clone = type.symbol();
            return Type.compoundType(map(parts), members, clone);
        case MethodType(Symbol[] vparams, Type result):
            return Type.MethodType(vparams, apply(result));
        case PolyType(Symbol[] tparams, Type result):
            return Type.PolyType(tparams, apply(result));
        case UnboxedType(_):
            return type;
        case UnboxedArrayType(_):
            return map(type);
        default:
            throw Debug.abort("illegal case", type);
        }
    }}

    //########################################################################
    // Private Class - Type cloner

    /** The type cloner Type.Map */
    private final Type.Map cloner = new TypeCloner();
    private class TypeCloner extends TypeMapper { public Type apply(Type type){
        switch (type) {
        case CompoundType(Type[] parts, Scope members):
            Symbol clone = /* !!! getCompoundClone */(type.symbol());
            return Type.compoundType(map(parts), /* !!! cloneScope */(members), clone);
        case MethodType(Symbol[] vparams, Type result):
            Symbol[] clones = cloneSymbols(vparams);
            return Type.MethodType(clones, apply(result));
        case PolyType(Symbol[] tparams, Type result):
            Symbol[] clones = cloneSymbols(tparams);
            return Type.PolyType(clones, apply(result));
        default:
            return super.apply(type);
        }
    }}

    //########################################################################
}
