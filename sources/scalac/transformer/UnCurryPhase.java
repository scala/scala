/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.transformer;

import scalac.*;
import scalac.parser.*;
import scalac.symtab.*;
import scalac.checkers.*;

public class UnCurryPhase extends Phase implements Modifiers {

    /** Initializes this instance. */
    public UnCurryPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
    }

    /** Applies this phase to the given compilation units. */
    public void apply(CompilationUnit[] units) {
        for (int i = 0; i < units.length; i++)
            new UnCurry(global, this).apply(units[i]);
    }

    /** - return symbol's transformed type,
     *  - if symbol is a def parameter with transformed type T, return () => T
     */
    public Type transformInfo(Symbol sym, Type tp0) {
	Type tp1 = uncurry(tp0);
	if (sym.isDefParameter()) return global.definitions.FUNCTION_TYPE(Type.EMPTY_ARRAY, tp1);
	else return tp1;
    }

    /** - (ps_1)...(ps_n)T ==> (ps_1,...,ps_n)T
     */
    Type uncurry(Type tp) {
	switch (tp) {
	case MethodType(Symbol[] params, Type tp1):
	    Type newtp1 = uncurry(tp1);
	    switch (newtp1) {
	    case MethodType(Symbol[] params1, Type tp2):
		Symbol[] newparams = new Symbol[params.length + params1.length];
		System.arraycopy(params, 0, newparams, 0, params.length);
		System.arraycopy(params1, 0, newparams, params.length, params1.length);
		return Type.MethodType(newparams, tp2);
	    default:
		if (newtp1 == tp1) return tp;
		else return Type.MethodType(params, newtp1);
	    }
	case PolyType(Symbol[] tparams, Type tp1):
	    Type newtp1 = uncurry(tp1);
	    switch (tp1) {
	    case MethodType(_, _):
		if (newtp1 == tp1) return tp;
		else return Type.PolyType(tparams, newtp1);
	    default:
		newtp1 = Type.MethodType(Symbol.EMPTY_ARRAY, newtp1);
		if (tparams.length == 0) return newtp1;
		else return Type.PolyType(tparams, newtp1);
	    }
	case OverloadedType(_, _):
	    return new Type.Map() {
		public Type apply(Type t) { return uncurry(t); }
	    }.map(tp);
	case ConstantType(Type base, _):
	    return base;
        case CompoundType(Type[] parents, Scope scope):
            Symbol symbol = tp.symbol();
            if (!symbol.isClass() || symbol.isCompoundSym()) return tp;
            Scope clone = new Scope();
            for (Scope.SymbolIterator i = scope.iterator(); i.hasNext();) {
                Symbol member = i.next();
                if (isUnaccessedConstant(member)) continue;
                if (member.isCaseFactory() && !member.isModule()) continue;
                clone.enterOrOverload(member);
            }
            return Type.compoundType(parents, clone, symbol);
	default:
	    return tp;
	}
    }

    boolean isUnaccessedConstant(Symbol symbol) {
        if (!symbol.isTerm()) return false;
        if ((symbol.flags & ACCESSED) != 0) return false;
        switch (symbol.type()) {
        case PolyType(Symbol[] params, ConstantType(_, _)):
            return params.length == 0;
        case ConstantType(_, _):
            return true;
        default:
            return false;
        }
    }

    public Checker[] postCheckers(Global global) {
        return new Checker[] {
            new CheckSymbols(global),
            new CheckTypes(global),
        };
    }
}
