/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$
// $OldId: ExplicitOuterClassesPhase.java,v 1.8 2002/08/21 14:08:18 paltherr Exp $

package scalac.transformer;

import scalac.*;
import scalac.checkers.*;
import scalac.symtab.*;
import scalac.util.*;
import java.util.HashMap;

public class ExplicitOuterClassesPhase extends Phase {
    // Mapping from class constructor symbols to owner field symbols.
    protected final HashMap/*<Symbol,Symbol>*/ outerMap = new HashMap();

    /** Initializes this instance. */
    public ExplicitOuterClassesPhase(Global global,PhaseDescriptor descriptor){
        super(global, descriptor);
    }

    /** Applies this phase to the given compilation units. */
    public void apply(Unit[] units) {
        for (int i = 0; i < units.length; i++)
            new ExplicitOuterClasses(global, this).apply(units[i]);
    }

    public Checker[] postCheckers(Global global) {
        return new Checker[] {
            new CheckSymbols(global),
            new CheckTypes(global),
            new CheckOwners(global),
	    new CheckNames(global)
        };
    }

    public Type transformInfo(Symbol sym, Type tp) {
        if (sym != Symbol.NONE
            && sym.isConstructor()
            && sym.owner().isClass()
            && !(sym.isJava() || sym.owner().isRoot())) {
            return addOuterValueParam(tp, sym);
        } else
            return tp;
    }

    /**
     * Return the symbol for the outer parameter corresponding to the
     * given constructor.
     */
    protected Symbol outerSym(Symbol constSym) {
        if (! outerMap.containsKey(constSym)) {
            Symbol ownerSym = constSym.enclClass();
            Name outerName =
                Global.instance.freshNameCreator.newName(Names.OUTER_PREFIX);
            Symbol outerSym =
                new TermSymbol(constSym.pos, outerName, constSym, 0);
            outerSym.setInfo(ownerSym.type());

            outerMap.put(constSym, outerSym);
        }
        return (Symbol)outerMap.get(constSym);
    }

    /**
     * Add the given value parameter to the type, which must be the
     * type of a method, as the first argument.
     */
    protected Type addOuterValueParam(Type oldType, Symbol constr) {
        switch (oldType) {
        case MethodType(Symbol[] vparams, Type result): {
            Symbol[] newVParams = new Symbol[vparams.length + 1];
            newVParams[0] = outerSym(constr);
            System.arraycopy(vparams, 0, newVParams, 1, vparams.length);
            return new Type.MethodType(newVParams, result);
        }

        case PolyType(Symbol[] tparams, Type result):
            return new Type.PolyType(tparams,
                                     addOuterValueParam(result, constr));

        case OverloadedType(Symbol[] alts, Type[] altTypes): {
            Type[] newAltTypes = new Type[altTypes.length];
            for (int i = 0; i < newAltTypes.length; ++i)
                newAltTypes[i] = addOuterValueParam(altTypes[i], alts[i]);
            return new Type.OverloadedType(alts, newAltTypes);
        }

        default:
            throw Global.instance.fail("invalid type", oldType);
        }
    }
}
