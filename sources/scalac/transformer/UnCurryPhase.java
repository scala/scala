/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.transformer;

import scalac.*;
import scalac.parser.*;
import scalac.symtab.*;
import scalac.typechecker.Infer;
import scalac.checkers.*;

public class UnCurryPhase extends PhaseDescriptor implements Modifiers {

    private Global global;

    public void initialize(Global global, int id) {
        super.initialize(global, id);
	this.global = global;
    }

    public String name () {
        return "uncurry";
    }

    public String description () {
        return "uncurry function types and applications";
    }

    public String taskDescription() {
        return "uncurried";
    }

    public Phase createPhase(Global global) {
        return new UnCurry(global, this);
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
	    Symbol[] uncurriedParams = uncurryParams(params);
	    Type uncurriedTp1 = uncurry(tp1);
	    switch (uncurriedTp1) {
	    case MethodType(Symbol[] params1, Type tp2):
		Symbol[] newparams = new Symbol[uncurriedParams.length + params1.length];
		System.arraycopy(uncurriedParams, 0, newparams, 0, uncurriedParams.length);
		System.arraycopy(params1, 0, newparams, uncurriedParams.length, params1.length);
		return Type.MethodType(newparams, tp2);
	    default:
		if (uncurriedParams == params && uncurriedTp1 == tp1) return tp;
		else return Type.MethodType(uncurriedParams, uncurriedTp1);
	    }
	case PolyType(Symbol[] tparams, Type tp1):
	    switch (tp1) {
	    case MethodType(_, _):
		Type newtp1 = uncurry(tp1);
		if (tp1 != newtp1) return Type.PolyType(tparams, newtp1);
		else return tp;
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

    Symbol[] uncurryParams(Symbol[] params) {
	Symbol[] params1 = params;
	for (int i = 0; i < params.length; i++) {
	    Symbol param = params[i];
	    Symbol param1 = param;
	    Type tp = param.info();
	    Type tp1 = transformInfo(param, tp);
	    if (tp != tp1) {
		if (params1 == params) {
		    params1 = new Symbol[params.length];
		    System.arraycopy(params, 0, params1, 0, i);
		}
		param1 = param.cloneSymbol().setType(tp1);
		param1.flags &= ~DEF;
	    }
	    params1[i] = param1;
	}
	return params1;
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
