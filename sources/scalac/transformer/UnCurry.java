/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.transformer;

import java.io.*;
import java.util.*;
import scalac.*;
import scalac.util.*;
import scalac.ast.*;
import scalac.symtab.*;
import Tree.*;

/** - uncurry all symbol and tree types (@see UnCurryPhase)
 *  - for every curried parameter list:  (ps_1) ... (ps_n) ==> (ps_1, ..., ps_n)
 *  - for every curried application: f(args_1)...(args_n) ==> f(args_1, ..., args_n)
 *  - for every type application: f[Ts] ==> f[Ts]() unless followed by parameters
 *  - for every use of a parameterless function: f ==> f()  and  q.f ==> q.f()
 *  - for every def-parameter:  def x: T ==> x: () => T
 *  - for every use of a def-parameter: x ==> x.apply()
 *  - for every argument to a def parameter `def x: T':
 *      if argument is not a reference to a def parameter:
 *        convert argument `e' to (expansion of) `() => e'
 *  - for every argument list that corresponds to a repeated parameter
 *       (a_1, ..., a_n) => (Sequence(a_1, ..., a_n))
 *  - for every argument list that is an escaped sequence
 *       (a_1:_*) => (a_1)
 */
public class UnCurry extends OwnerTransformer
                     implements Modifiers {

    UnCurryPhase descr;
    CompilationUnit unit;

    public UnCurry(Global global, UnCurryPhase descr) {
        super(global);
	this.descr = descr;
    }

    public void apply(CompilationUnit unit) {
	this.unit = unit;
	super.apply(unit);
    }

    /** (ps_1) ... (ps_n) => (ps_1, ..., ps_n)
     */
    ValDef[][] uncurry(ValDef[][] params) {
	int n = 0;
	for (int i = 0; i < params.length; i++)
	    n = n + params[i].length;
	ValDef[] ps = new ValDef[n];
	int j = 0;
	for (int i = 0; i < params.length; i++) {
	    System.arraycopy(params[i], 0, ps, j, params[i].length);
	    j = j + params[i].length;
	}
	return new ValDef[][]{ps};
    }

    /** tree of non-method type T ==> same tree with method type ()T
     */
    Tree asMethod(Tree tree) {
	switch (tree.type) {
	case MethodType(_, _):
	    return tree;
	default:
	    return tree.setType(
		Type.MethodType(Symbol.EMPTY_ARRAY, tree.type.widen()));
	}
    }

    /** apply parameterless functions and def parameters
     */
    Tree applyDef(Tree tree1) {
	assert tree1.symbol() != null : tree1;
	switch (tree1.symbol().type()) {
	case PolyType(Symbol[] tparams, Type restp):
	    if (tparams.length == 0 && !(restp instanceof Type.MethodType)) {
		return gen.Apply(asMethod(tree1), new Tree[0]);
	    } else {
		return tree1;
	    }
	default:
	    if (tree1.symbol().isDefParameter()) {
		tree1.type = global.definitions.FUNCTION_TYPE(
		    Type.EMPTY_ARRAY, tree1.type.widen());
		return gen.Apply(gen.Select(tree1, global.definitions.FUNCTION_APPLY(0)));
	    } else {
		return tree1;
	    }
	}
    }

    /** - uncurry all symbol and tree types (@see UnCurryPhase)
     *  - for every curried parameter list:  (ps_1) ... (ps_n) ==> (ps_1, ..., ps_n)
     *  - for every curried application: f(args_1)...(args_n) ==> f(args_1, ..., args_n)
     *  - for every type application: f[Ts] ==> f[Ts]() unless followed by parameters
     *  - for every use of a parameterless function: f ==> f()  and  q.f ==> q.f()
     *  - for every def-parameter:  def x: T ==> x: () => T
     *  - for every use of a def-parameter: x ==> x.apply()
     *  - for every argument to a def parameter `def x: T':
     *      if argument is not a reference to a def parameter:
     *        convert argument `e' to (expansion of) `() => e'
     *  - for every argument list that corresponds to a repeated parameter
     *       (a_1, ..., a_n) => (Sequence(a_1, ..., a_n))
     */
    public Tree transform(Tree tree) {
        System.out.flush();
	//uncurry type and symbol
	Type prevtype = tree.type;
	if (prevtype != null) {
	    switch (prevtype) {
	    case OverloadedType(_, _):
		assert tree.symbol() != null;
		prevtype = tree.symbol().removeInheritedOverloaded(prevtype);
	    }
	    tree.type = descr.uncurry(prevtype);
	}
        switch (tree) {
	case ClassDef(_, _, AbsTypeDef[] tparams, ValDef[][] vparams, Tree tpe, Template impl):
            Symbol clazz = tree.symbol();
            for (Scope.SymbolIterator it = clazz.members().iterator(); it.hasNext(); )
                checkNoDoubleDef(clazz, it.next());
	    return copy.ClassDef(
		tree, clazz, tparams,
		uncurry(transform(vparams, clazz)),
		tpe,
		transform(impl, clazz));

	case DefDef(_, _, AbsTypeDef[] tparams, ValDef[][] vparams, Tree tpe, Tree rhs):
	    Symbol sym = tree.symbol();
	    if (descr.isUnaccessedConstant(sym))
                return gen.mkUnitLit(tree.pos);
	    Tree rhs1 = transform(rhs, sym);
	    return copy.DefDef(
		tree, sym, tparams, uncurry(transform(vparams, sym)), tpe, rhs1);

	case ValDef(_, _, Tree tpe, Tree rhs):
	    Symbol sym = tree.symbol();
	    if (descr.isUnaccessedConstant(sym))
                return gen.mkUnitLit(tree.pos);
	    if (sym.isDefParameter()) {
		Type newtype = global.definitions.FUNCTION_TYPE(Type.EMPTY_ARRAY, tpe.type);
		Tree tpe1 = gen.mkType(tpe.pos, newtype);
                return copy.ValDef(tree, tpe1, rhs).setType(newtype);
	    } else {
		return super.transform(tree);
	    }

	case TypeApply(Tree fn, Tree[] args):
	    Tree tree1 = asMethod(super.transform(tree));
	    return gen.Apply(tree1, new Tree[0]);


	case Apply(Tree fn, Tree[] args):
	    // f(x)(y) ==> f(x, y)
	    // argument to parameterless function e => ( => e)
	    Type ftype = fn.type;
	    Tree fn1 = transform(fn);
            boolean myInArray = TreeInfo.methSymbol(fn1) == global.definitions.PREDEF_ARRAY();
            inArray = myInArray;
	    Tree[] args1 = transformArgs(tree.pos, args, ftype);
            if( myInArray )
                  switch( fn1 ) {
                  case Apply( TypeApply( Select(Tree fn2, _), Tree[] targs), _ ):
                        return gen.mkBlock(args1[0].pos, fn2, args1[0]);
                  default:
                        assert false : "dead";
                  }
	    if (TreeInfo.methSymbol(fn1) == global.definitions.ANY_MATCH &&
		!(args1[0] instanceof Tree.Visitor)) {
		switch (TreeInfo.methPart(fn1)) {
		case Select(Tree qual, Name name):
		    assert name == Names._match;
		    return gen.postfixApply(qual, args1[0], currentOwner);
		default:
		    throw new ApplicationError("illegal prefix for match: " + tree);
		}

	    } else {
		switch (fn1) {
		case Apply(Tree fn2, Tree[] args2):
		    Tree[] newargs = new Tree[args1.length + args2.length];
		    System.arraycopy(args2, 0, newargs, 0, args2.length);
		    System.arraycopy(args1, 0, newargs, args2.length, args1.length);
		    return copy.Apply(tree, fn2, newargs);
		default:
		    return copy.Apply(tree, fn1, args1);
		}
	    }

	case Select(_, _):
	    return applyDef(super.transform(tree));

	case Ident(Name name):
	    if (name == TypeNames.WILDCARD_STAR) {
		unit.error(tree.pos, " argument does not correspond to `*'-parameter");
		return tree;
	    } else if (tree.symbol() == global.definitions.PATTERN_WILDCARD) {
		return tree;
	    } else {
		return applyDef(super.transform(tree));
	    }

        case CaseDef(Tree pat, Tree guard, Tree body):
              inPattern = true;
              Tree pat1 = transform( pat );
              inPattern = false;
              Tree guard1 = transform( guard );
              Tree body1 = transform( body );
              return copy.CaseDef( tree, pat1, guard1, body1 );

	default:
	    return super.transform(tree);
	}
    }

      boolean inPattern = false;
      boolean inArray = false;
//    java.util.HashSet visited = new java.util.HashSet();//DEBUG

    /** Transform arguments `args' to method with type `methtype'.
     */
    private Tree[] transformArgs(int pos, Tree[] args, Type methtype) {
//	if (args.length != 0 && visited.contains(args)) {
//	    new scalac.ast.printer.TextTreePrinter().print("dup args: ").print(make.Block(pos, args)).println().end();//DEBUG
//	    assert false;
//	}
//	visited.add(args);//DEBUG

	switch (methtype) {
	case MethodType(Symbol[] params, _):
	    if (params.length > 0 &&
		(params[params.length-1].flags & REPEATED) != 0) {
		args = toSequence(pos, params, args);
	    }
	    Tree[] args1 = args;
	    for (int i = 0; i < args.length; i++) {
		Tree arg = args[i];
		Tree arg1 = transformArg(arg, params[i]);
		if (arg1 != arg && args1 == args) {
		    args1 = new Tree[args.length];
		    System.arraycopy(args, 0, args1, 0, i);
		}
		args1[i] = arg1;
	    }
	    return args1;
	case PolyType(_, Type restp):
	    return transformArgs(pos, args, restp);
	default:
	    if (args.length == 0) return args; // could be arguments of nullary case pattern
	    else throw new ApplicationError(methtype);
	}
    }

    /** converts `a_1,...,a_n' to Seq(a_1,...,a_n)
     *  if a_n is an escaped sequence x:_*, takes care of escaping
     *
     *  precondition: params[params.length-1].flags & REPEATED != 0
     */
    private Tree[] toSequence( int pos, Symbol[] params, Tree[] args ) {
	Tree[] result = new Tree[params.length];
        System.arraycopy(args, 0, result, 0, params.length - 1);
        /*
	for (int i = 0; i < params.length - 1; i++)
	    result[i] = args[i];
        */
	assert (args.length != params.length
		|| !(args[params.length-1] instanceof Tree.Sequence)
		|| TreeInfo.isSequenceValued(args[params.length-1]));
 	if (args.length == params.length) {
            switch (args[params.length-1]) {
            case Typed(Tree arg, Ident(TypeNames.WILDCARD_STAR)): // seq:_* escape
		result[params.length-1] = arg;
		return result;
            }
        }
	Tree[] args1 = args;
	if (params.length != 1) {
	    args1 = new Tree[args.length - (params.length - 1)];
	    System.arraycopy(args, params.length - 1, args1, 0, args1.length);
	}
        Type theType = params[params.length-1].type();
        if( inPattern )
              result[params.length-1] =
                    make.Sequence(pos, args1).setType( theType );
        else if( inArray ) {
              result[params.length-1] = gen.mkNewArray(pos,
                                                       theType.typeArgs()[0],
                                                       args1,
                                                       currentOwner);

        } else
              result[params.length-1] =
                    gen.mkNewList(pos,
                                  theType.typeArgs()[0],
                                  args1);
	return result;
    }

    /** for every argument to a def parameter `def x: T':
     *    if argument is not a reference to a def parameter:
     *      convert argument `e' to (expansion of) `() => e'
     */
    private Tree transformArg(Tree arg, Symbol formal) {
	if ((formal.flags & DEF) != 0) {
	    Symbol sym = arg.symbol();
	    if (sym != null && (sym.flags & DEF) != 0) {
		Tree arg1 = transform(arg);
		switch (arg1) {
		case Apply(Select(Tree qual, Name name), Tree[] args1):
		    assert name == Names.apply && args1.length == 0;
		    return qual;
		default:
		    System.err.println(arg1);//debug
		    throw new ApplicationError();
		}
	    }
	    return transform(
		gen.mkUnitFunction(arg, descr.uncurry(arg.type.widen()), currentOwner));
	} else {
	    return transform(arg);
	}
    }

// Double Definition Checking -----------------------------------------------

    private void checkNoDoubleDef(Symbol clazz, Symbol sym) {
        switch (sym.type()) {
        case OverloadedType(Symbol[] alts, Type[] alttypes):
            for (int i = 0; i < alttypes.length; i++) {
                for (int j = i + 1; j < alttypes.length; j++)
                    if (inConflict(alts[i], alts[j], descr.uncurry(alttypes[i]), descr.uncurry(alttypes[j])))
                        conflictError(clazz, alts[i], alts[j], alttypes[i], alttypes[j]);
            }
            break;
        default:
        }
    }

    private void conflictError(Symbol clazz, Symbol sym1, Symbol sym2, Type type1, Type type2) {
        if (sym1.owner() == clazz && sym2.owner() == clazz)
            unit.error(sym2.pos,
                       "Double declaration:\n" +
                       sym1 + ": " + type1 + " and\n" +
                       sym2 + ": " + type2 + " have same types after erasure");
        else if (sym1.owner() == clazz)
            unit.error(sym1.pos,
                       "Accidental override:\n" +
                       sym1 + ": " + type1 + " has same type after erasure as\n" +
                       sym2 + ": " + type2 + " which is inherited from " + sym2.owner());
        else if (sym2.owner() == clazz)
            unit.error(sym2.pos,
                       "Accidental override:\n" +
                       sym2 + ": " + type2 + " has same type after erasure as\n" +
                       sym1 + ": " + type1 + " which is inherited from " + sym1.owner());
        else
            unit.error(clazz.pos,
                       "Inheritance conflict: inherited members\n" +
                      sym1 + ": " + type1 + sym1.locationString() + " and\n" +
                       sym2 + ": " + type2 + sym2.locationString() + " have same types after erasure");
    }

    private boolean inConflict(Symbol sym1, Symbol sym2, Type type1, Type type2) {
        switch (type1) {
        case PolyType(_, Type restype1):
            return inConflict(sym1, sym2, restype1, type2);

        case MethodType(Symbol[] params1, Type restype1):
            switch (type2) {
            case PolyType(_, Type restype2):
                return inConflict(sym1, sym2, type1, restype2);

            case MethodType(Symbol[] params2, Type restype2):
                if (params1.length != params2.length) return false;
                for (int i = 0; i < params1.length; i++) {
                    if (!params1[i].nextInfo().erasure().isSameAs(
                            params2[i].nextInfo().erasure())) return false;
                }
                if (restype1.erasure().isSameAs(restype2.erasure()))
                    return true;
                if (sym1.owner() == sym2.owner())
                    return false;
                for (int i = 0; i < params1.length; i++) {
                    if (!params1[i].nextInfo().isSameAs(
                            params2[i].nextInfo())) return false;
                }
                return true;

            default:
                return false;
            }

        default:
            switch (type2) {
            case PolyType(_, _):
            case MethodType(_, _): return inConflict(sym1, sym2, type2, type1);
            default: return true;
            }
        }
    }
}
