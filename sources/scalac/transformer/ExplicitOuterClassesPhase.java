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

public class ExplicitOuterClassesPhase extends PhaseDescriptor {
    // Mapping from class constructor symbols to owner field symbols.
    protected HashMap/*<Symbol,Symbol>*/ outerMap = new HashMap();

    public String name () {
        return "explicitouterclasses";
    }

    public String description () {
        return "make links from inner classes to enclosing one explicit";
    }

    public String taskDescription() {
        return "made outer links explicit";
    }

    public void apply(Global global) {
        new ExplicitOuterClasses(global).apply();
    }

    public void apply(Unit unit) {
        new ExplicitOuterClasses(unit.global).apply(unit);
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
            return addValueParam(tp, outerSym(sym));
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
    protected Type addValueParam(Type oldType, Symbol newValueParam) {
        switch (oldType) {
        case MethodType(Symbol[] vparams, Type result): {
            Symbol[] newVParams = new Symbol[vparams.length + 1];
            newVParams[0] = newValueParam;
            System.arraycopy(vparams, 0, newVParams, 1, vparams.length);
            return new Type.MethodType(newVParams, result);
        }

        case PolyType(Symbol[] tparams, Type result):
            return new Type.PolyType(tparams, addValueParam(result, newValueParam));

        default:
            throw Global.instance.fail("invalid type", oldType);
        }
    }
}
