/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $Id$

package scalac.transformer;

import scalac.*;
import scalac.util.*;
import scalac.parser.*;
import scalac.symtab.*;
import scalac.checkers.*;

public class LambdaLiftPhase extends PhaseDescriptor implements Kinds, Modifiers {

    private Global global;
    int nextPhase;

    public void initialize(Global global, int id) {
        super.initialize(global, id);
	this.global = global;
	this.nextPhase = id + 1;
    }

    public String name () {
        return "lambdalift";
    }

    public String description () {
        return "lambda lifter";
    }

    public String taskDescription() {
        return "lambda lifting";
    }

    public Phase createPhase(Global global) {
        return new LambdaLift(global, this);
    }

    public Type transformInfo(Symbol sym, Type tp) {
	Type tp1 = tp;
	if (sym != Symbol.NONE) tp1 = transform(tp, sym.owner());
	if ((sym.flags & Modifiers.CAPTURED) != 0) return refType(tp1);
	else return tp1;
    }

    /** Add proxies as type arguments for propagated type parameters.
     */
    Type transform(Type tp, Symbol owner) {
	return transformTypeMap.setOwner(owner).apply(tp);
    }

    private class TransformTypeMap extends Type.Map {
	Symbol owner;
        Type.Map setOwner(Symbol owner) { this.owner = owner; return this; }

	public Type apply(Type tp) {
	    switch (tp) {
	    case TypeRef(Type pre, Symbol sym, Type[] targs):
		switch (pre) {
		case ThisType(_):
		    if (sym.kind == CLASS && sym.constructor().isUpdated(nextPhase)) {
			Symbol[] tparams =
			    sym.constructor().infoAt(nextPhase).typeParams();
			int i = tparams.length;
			while (i > 0 && (tparams[i-1].flags & SYNTHETIC) != 0)
			    i--;
			if (i < tparams.length) {
			    Type[] targs1 = new Type[tparams.length];
			    System.arraycopy(map(targs), 0, targs1, 0, targs.length);
			    while (i < tparams.length) {
				targs1[i] = proxy(tparams[i], owner).type();
				i++;
			    }
			    return Type.TypeRef(pre, sym, targs1);
			}
		    }
		}
		break;
	    }
	    return map(tp);
	}

	/** All symbols are mapped to themselves.
	 */
	public Scope map(Scope s) { return s; }
	public Symbol map(Symbol s) { return s; }
	public Symbol[] map(Symbol[] ss) { return ss; }
    }

    private TransformTypeMap transformTypeMap = new TransformTypeMap();

    /** Return closest enclosing (type)parameter that has same name as `fv',
     *  or `fv' itself if this is the closest definition.
     */
    Symbol proxy(Symbol fv, Symbol owner) {
	if (global.debug)
	    global.log("proxy " + fv + " in " + LambdaLift.asFunction(owner));
	Symbol o = owner;
	while (o.kind != NONE) {
	    Symbol fowner = LambdaLift.asFunction(o);
	    if (fv.owner() == fowner) return fv;
	    Type ft = (fowner.isUpdated(nextPhase)) ? fowner.typeAt(nextPhase)
		: fowner.type();
	    Symbol[] ownerparams = fv.isType() ? ft.typeParams()
		: ft.firstParams();
	    for (int i = 0; i < ownerparams.length; i++) {
		if (ownerparams[i].name == fv.name)
		    return ownerparams[i];
	    }
	    assert o.owner() != o;
	    o = o.owner();

	}
	throw new ApplicationError("proxy " + fv + " in " + owner);
    }

    /** The type scala.Ref[tp]
     */
    Type refType(Type tp) {
	Symbol refClass = global.definitions.getClass(Names.scala_Ref);
	assert refClass.kind == Kinds.CLASS;
	return Type.TypeRef(global.definitions.SCALA_TYPE, refClass, new Type[]{tp});
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
