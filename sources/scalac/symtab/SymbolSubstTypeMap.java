/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab;

import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;

import ch.epfl.lamp.util.ForwardingMap;

import scalac.util.Debug;

/** A type map that substitues symbols and types for symbols. */
public class SymbolSubstTypeMap extends Type.Map {

    //########################################################################
    // Private Fields

    /** A table containing the symbol to symbol substitutions */
    private Map/*<Symbol, Symbol>*/ symbols;

    /** A table containing the symbol to type substitutions */
    private Map/*<Symbol, Type>*/ types;

    //########################################################################
    // Public Constructors

    public SymbolSubstTypeMap() {
        this.symbols = new HashMap();
        this.types = new HashMap();
    }

    public SymbolSubstTypeMap(Map symbols, Map types) {
        this();
        insertSymbol(symbols);
        insertType(types);
    }

    public SymbolSubstTypeMap(SymbolSubstTypeMap other) {
        this();
        insertSymbol(other.symbols);
        insertType(other.types);
    }

    //########################################################################
    // Public Methods - Inserting and removing symbol to symbol substitutions

    public void insertSymbol(Symbol[] keys, Symbol[] values) {
        assert keys.length == values.length : keys.length+" != "+values.length;
        for (int i = 0; i < keys.length; i++) insertSymbol(keys[i], values[i]);
    }

    public void insertSymbol(Symbol key, Symbol value) {
        assert !symbols.containsKey(key) : Debug.show(key);
        assert !types.containsKey(key) : Debug.show(key);
        symbols.put(key, value);
    }

    public void insertSymbol(Map map) {
        assert checkLeftContainsNoKeyFromRight(symbols, map);
        assert checkLeftContainsNoKeyFromRight(types, map);
        symbols.putAll(map);
    }

    public void removeSymbol(Symbol[] keys) {
        for (int i = 0; i < keys.length; i++) removeSymbol(keys[i]);
    }

    public void removeSymbol(Symbol key) {
        symbols.remove(key);
    }

    public void removeSymbol(Set keys) {
        symbols.keySet().removeAll(keys);
    }

    public Symbol lookupSymbol(Symbol key) {
        return (Symbol)symbols.get(key);
    }

    public Map getSymbols() {
        return new ForwardingMap(symbols) {
            public Object put(Object key, Object value) {
                insertSymbol((Symbol)key, (Symbol)value);
                return null;
            }
            public void putAll(Map map) {
                insertSymbol(map);
            }
        };
    }

    //########################################################################
    // Public Methods - Inserting and removing symbol to type substitutions

    public void insertType(Symbol[] keys, Type[] values) {
        assert keys.length == values.length : keys.length+" != "+values.length;
        for (int i = 0; i < keys.length; i++) insertType(keys[i], values[i]);
    }

    public void insertType(Symbol key, Type value) {
        assert !symbols.containsKey(key) : Debug.show(key);
        assert !types.containsKey(key) : Debug.show(key);
        types.put(key, value);
    }

    public void insertType(Map map) {
        assert checkLeftContainsNoKeyFromRight(symbols, map);
        assert checkLeftContainsNoKeyFromRight(types, map);
        types.putAll(map);
    }

    public void removeType(Symbol[] keys) {
        for (int i = 0; i < keys.length; i++) removeType(keys[i]);
    }

    public void removeType(Symbol key) {
        types.remove(key);
    }

    public void removeType(Set keys) {
        types.keySet().removeAll(keys);
    }

    public Type lookupType(Symbol key) {
        return (Type)types.get(key);
    }

    public Map getTypes() {
        return new ForwardingMap(types) {
            public Object put(Object key, Object value) {
                insertType((Symbol)key, (Type)value);
                return null;
            }
            public void putAll(Map map) {
                insertType(map);
            }
        };
    }

    //########################################################################
    // Public Methods - Applying the substitutions

    public Type apply(Type type) {
        switch (type) {

        case TypeRef(NoPrefix, Symbol symbol, Type[] args):
            Object value = types.get(symbol);
            if (value != null) return (Type)value;
            value = symbols.get(symbol);
            if (value == null) return super.map(type);
            Type prefix = ((Type.TypeRef)type).pre;
            return Type.typeRef(apply(prefix), (Symbol)value, map(args));

        case SingleType(NoPrefix, Symbol symbol):
            Object value = types.get(symbol);
            if (value != null) return (Type)value;
            value = symbols.get(symbol);
            if (value == null) return super.map(type);
            Type prefix = ((Type.SingleType)type).pre;
            return Type.singleType(apply(prefix), (Symbol)value);

        case ThisType(Symbol symbol):
            Object value = symbols.get(symbol);
            if (value == null) return super.map(type);
            return Type.ThisType((Symbol)value);

        default:
            return super.map(type);
        }
    }

    //########################################################################
    // Public Methods - Printing

    public String toString() {
        return "{ symbols=" + symbols + " types=" + types + " }";
    }

    //########################################################################
    // Private Function

    private static boolean checkLeftContainsNoKeyFromRight(Map lf, Map rg) {
        for (Iterator i = rg.keySet().iterator(); i.hasNext(); ) {
            Object key = i.next();
            assert !lf.containsKey(key) : Debug.show(key);
        }
        return true;
    }

    //########################################################################
}
