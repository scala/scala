/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.transformer;

import scalac.*;
import scalac.util.*;
import scalac.parser.*;
import scalac.symtab.*;
import scalac.checkers.*;
import java.util.ArrayList;

public class LambdaLiftPhase extends Phase implements Kinds, Modifiers {

    /** Initializes this instance. */
    public LambdaLiftPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
    }

    /** Applies this phase to the given compilation units. */
    public void apply(Unit[] units) {
        for (int i = 0; i < units.length; i++)
            new LambdaLift(global, this).apply(units[i]);
    }

    public Type transformInfo(Symbol sym, Type tp) {
	/*
        if (global.debug)
            global.log("transform info for " + sym + ":" + tp + sym.locationString());
	*/
        Type tp1 = tp;
        if (sym != Symbol.NONE) {
            switch (tp) {
            case MethodType(_, _):
            case PolyType(_, _):
                tp1 = transform(tp, sym);
                break;
            default:
                if (sym.kind == CLASS)
                    tp1 = transform(tp, sym);
                else
                    tp1 = transform(tp, sym.owner());
            }
        }
        if ((sym.flags & Modifiers.CAPTURED) != 0)
            return global.definitions.REF_TYPE(tp1);
        else return tp1;
    }

    /** Add proxies as type arguments for propagated type parameters.
     */
    Type transform(Type tp, Symbol owner) {
        return transformTypeMap.setOwner(owner).apply(tp);
    }

    /** MapOnlyTypes => All symbols are mapped to themselves.
     */
    private class TransformTypeMap extends Type.MapOnlyTypes {
        Symbol owner;
//      ArrayList/*<Symbol>*/ excluded = new ArrayList();
        Type.Map setOwner(Symbol owner) { this.owner = owner; return this; }

        public Type apply(Type tp) {
            switch (tp) {
            case TypeRef(Type pre, Symbol sym, Type[] targs):
                if (sym.kind == CLASS) {
                    switch (pre) {
                    case ThisType(Symbol s):
                        if (s == Symbol.NONE) {
                            pre = sym.owner().enclClass().thisType();
                            tp = Type.typeRef(pre, sym, targs);
                        }
                    }
                }
                switch (pre) {
                case ThisType(_):
                    if (sym.kind == CLASS &&
			sym.primaryConstructor().isUpdatedAt(LambdaLiftPhase.this)) {
                        Symbol[] tparams = sym.primaryConstructor().nextInfo().typeParams();
                        int i = tparams.length;
                        while (i > 0 && (tparams[i-1].flags & SYNTHETIC) != 0)
                            i--;
                        if (i < tparams.length) {
                            if (global.debug)
                                global.log("adding proxies for " + sym + ": " + ArrayApply.toString(tparams));

                            Type[] targs1 = new Type[tparams.length];
                            System.arraycopy(map(targs), 0, targs1, 0, targs.length);
                            while (i < tparams.length) {
                                targs1[i] = proxy(tparams[i], owner).type();
                                i++;
                            }
                            return Type.typeRef(pre, sym, targs1);
                        }
                    } else if (LambdaLift.isLocal(sym, owner)) {
                        assert targs.length == 0;
                        return proxy(sym, owner).type();
                    }
                }
                break;
/*
            case PolyType(Symbol[] tparams, _):
                if (tparams.length != 0) {
                    int len = excluded.size();
                    for (int i = 0; i < tparams.length; i++)
                        excluded.add(tparams[i]);
                    Type tp1 = map(tp);
                    for (int i = 0; i < tparams.length; i++)
                        excluded.remove(excluded.size() - 1);
                    return tp1;
                }
*/
            }
            return map(tp);
        }
    }

    private TransformTypeMap transformTypeMap = new TransformTypeMap();

    /** Return closest enclosing (type)parameter that has same name as `fv',
     *  or `fv' itself if this is the closest definition.
     */
    Symbol proxy(Symbol fv, Symbol owner) {
        if (global.debug)
            global.log("proxy " + fv + " of " + fv.owner() + " in " + LambdaLift.enclFun(owner));
        Symbol o = owner;
        while (o.kind != NONE) {
            if (global.debug)
                global.log("looking in " +  LambdaLift.enclFun(o) + " " +
                    ArrayApply.toString(o.typeParams()));
            Symbol fowner = LambdaLift.enclFun(o);
            if (fowner.isMethod()) {
                if (LambdaLift.enclFun(fv.owner()) == fowner) return fv;
                Type ft = (fowner.isUpdatedAt(this)) ? fowner.nextType()
                    : fowner.type();
                Symbol[] ownerparams = fv.isType() ? ft.typeParams()
                    : ft.firstParams();
                for (int i = 0; i < ownerparams.length; i++) {
                    if (ownerparams[i].name == fv.name)
                        return ownerparams[i];
                }
            }
            assert o.owner() != o;
            o = o.owner();
        }
        return fv;
        //throw new ApplicationError("proxy " + fv + " in " + owner);
    }

    public Checker[] postCheckers(Global global) {
        return new Checker[] {
            new CheckSymbols(global),
            new CheckTypes(global),
            new CheckOwners(global),
        };
    }
}
