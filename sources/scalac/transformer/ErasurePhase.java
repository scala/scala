/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: ErasurePhase.java,v 1.13 2002/11/14 15:58:22 schinz Exp $
// $Id$

package scalac.transformer;

//import scala.compiler.parser.Kinds;
//import scala.compiler.typechecker.*;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.checkers.Checker;
import scalac.checkers.CheckOwners;
import scalac.checkers.CheckSymbols;
import scalac.checkers.CheckTypes;
import scalac.symtab.Definitions;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.util.Name;
import scalac.util.Debug;

public class ErasurePhase extends PhaseDescriptor {

    public Definitions definitions;

    public String name () {
        return "erasure";
    }

    public String description () {
        return "type eraser";
    }

    public String taskDescription() {
        return "erased types";
    }

    public Phase createPhase(Global global) {
	this.definitions = global.definitions;
        return new Erasure(global, this);
    }

    private static final Name as_N = Name.fromString("as");
    private static final Name is_N = Name.fromString("is");
    private static final Name box_N = Name.fromString("box");
    private static final Name scala_runtime_RunTime_N =
       Name.fromString("scala.runtime.RunTime");

    private Type eraseParams(Type tp) {
	switch (tp) {
	case PolyType(_, Type result):
	    return eraseParams(result);
	case MethodType(Symbol[] params, Type result):
	    Symbol[] params1 = Type.erasureMap.map(params);
	    if (params1 == params) return tp;
	    else return Type.MethodType(params1, result);
	default:
	    return tp;
	}
    }

    public Type transformInfo(Symbol sym, Type tp) {
	if ((sym.name == is_N || sym.name == as_N) &&
	    sym.owner() == definitions.ANY_CLASS)
	    return tp;
	else if (sym.name == box_N &&
		 sym.owner().fullName() == scala_runtime_RunTime_N)
	    return eraseParams(tp);
	else if (sym.isClass())
	    return Type.erasureMap.map(tp);
        else if (sym.isType())
            return tp;
	else
	    return tp.erasure();
    }

    public Checker[] postCheckers(Global global) {
        return new Checker[] {
            new CheckSymbols(global),
            new CheckTypes(global),
            new CheckOwners(global)
        };
    }
}
