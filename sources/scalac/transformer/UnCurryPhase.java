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
import scalac.typechecker.Infer;
import scalac.checkers.*;

public class UnCurryPhase extends Phase implements Modifiers {

    /** Initializes this instance. */
    public UnCurryPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
    }

    /** Applies this phase to the given compilation units. */
    public void apply(Unit[] units) {
        for (int i = 0; i < units.length; i++)
            new UnCurry(global, this).apply(units[i]);
    }

    /** - return symbol's transformed type,
     *  - if symbol is a def parameter with transformed type T, return () => T
     */
    public Type transformInfo(Symbol sym, Type tp0) {
	Type tp1 = uncurry(tp0);
	if (sym.isDefParameter()) return global.definitions.functionType(Type.EMPTY_ARRAY, tp1);
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
	    switch (tp1) {
	    case MethodType(_, _):
		Type newtp1 = uncurry(tp1);
		if (newtp1 == tp1) return tp;
		else return Type.PolyType(tparams, newtp1);
	    default:
		Type newtp1 = Type.MethodType(Symbol.EMPTY_ARRAY, tp1);
		if (tparams.length == 0) return newtp1;
		else return Type.PolyType(tparams, newtp1);
	    }
	case OverloadedType(_, _):
	    return new Type.Map() {
		public Type apply(Type t) { return uncurry(t); }
	    }.map(tp);
	default:
	    return tp;
	}
    }

    public Checker[] postCheckers(Global global) {
        return new Checker[] {
            new CheckSymbols(global),
            new CheckTypes(global),
            new CheckOwners(global),
	    new CheckNames(global)
        };
    }
}
