/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $OldId: SymbolMapApplier.java,v 1.6 2002/04/19 16:41:41 odersky Exp $
// $Id$

package scalac.symtab;

import scalac.*;
import scalac.symtab.*;
import java.util.*;


/**
 * Apply a symbol substitution to various data (symbols and types).
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class SymbolMapApplier {
    protected final Map map;

    public SymbolMapApplier(Map map) {
        this.map = map;
    }

    public Symbol apply(Symbol sym) {
        return map.containsKey(sym) ? (Symbol)map.get(sym) : sym;
    }

    public Symbol[] apply(Symbol[] syms) {
        Symbol[] newSyms = new Symbol[syms.length];
        for (int i = 0; i < newSyms.length; ++i)
            newSyms[i] = apply(syms[i]);
        return newSyms;
    }

    public Type apply(Type type) {
        switch (type) {
        case ErrorType:
        case AnyType:
        case NoType:
            return type;

        case ThisType(Symbol sym):
            return new Type.ThisType(apply(sym));

        case TypeRef(Type prefix, Symbol sym, Type[] args):
            return new Type.TypeRef(apply(prefix), apply(sym), apply(args));

        case SingleType(Type pre, Symbol sym):
            return Type.singleType(apply(pre), apply(sym));

        case CompoundType(Type[] parts, Scope members):
            return Type.compoundType(apply(parts), members, apply(type.symbol()));

        case MethodType(Symbol[] params, Type restpe):
            return new Type.MethodType(apply(params), apply(restpe));

        case PolyType(Symbol[] tparams, Type restpe):
            return new Type.PolyType(apply(tparams), apply(restpe));

        case OverloadedType(Symbol[] alts, Type[] alttypes):
            return new Type.OverloadedType(apply(alts), apply(alttypes));

	case CovarType(Type result):
	    return Type.CovarType(apply(result));

        default:
            throw new ApplicationError("unknown type " + type);
        }
    }

    public Type[] apply(Type[] types) {
        Type[] newTypes = new Type[types.length];
        for (int i = 0; i < types.length; ++i)
            newTypes[i] = apply(types[i]);
        return newTypes;
    }

}
