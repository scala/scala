/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.transformer;

import java.io.*;
import java.util.*;
import scalac.*;
import scalac.util.*;
import scalac.ast.*;
import scalac.symtab.*;
import Tree.*;

/** Make all functions into one-argument functions
 */
public class UnCurry extends OwnerTransformer
                     implements Modifiers {

    UnCurryPhase descr;

    public UnCurry(Global global, UnCurryPhase descr) {
        super(global, descr);
	this.descr = descr;
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
	    return tree.setType(Type.MethodType(Symbol.EMPTY_ARRAY, tree.type));
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
     *       (a_1, ..., a_n) => (Tuple(a_1, ..., a_n))
     */
    public Tree transform(Tree tree) {
	//new scalac.ast.printer.TextTreePrinter().print("uncurry: ").print(tree).println().end();//DEBUG
	//uncurry type and symbol
	if (tree.type != null) tree.type = descr.uncurry(tree.type);
        switch (tree) {
	case ClassDef(int mods, Name name, TypeDef[] tparams, ValDef[][] vparams, Tree tpe, Template impl):
	    return copy.ClassDef(
		tree, mods, name, tparams,
		uncurry(transform(vparams, tree.symbol())),
		tpe,
		transform(impl, tree.symbol()));

	case DefDef(int mods, Name name, TypeDef[] tparams, ValDef[][] vparams, Tree tpe, Tree rhs):
	    Tree rhs1 = transform(rhs, tree.symbol());
	    return copy.DefDef(
		tree, mods, name, tparams,
		uncurry(transform(vparams, tree.symbol())),
		tpe, rhs1);

	case ValDef(int mods, Name name, Tree tpe, Tree rhs):
	    if (tree.symbol().isDefParameter()) {
		int mods1 = mods & ~ DEF;
		Type newtype = global.definitions.functionType(Type.EMPTY_ARRAY, tpe.type);
		Tree tpe1 = gen.mkType(tpe.pos, newtype);
		return copy.ValDef(tree, mods1, name, tpe1, rhs).setType(newtype);
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
	    Tree[] args1 = transformArgs(tree.pos, args, ftype);
	    switch (fn1) {
	    case Apply(Tree fn2, Tree[] args2):
		Tree[] newargs = new Tree[args1.length + args2.length];
		System.arraycopy(args2, 0, newargs, 0, args2.length);
		System.arraycopy(args1, 0, newargs, args2.length, args1.length);
		return copy.Apply(tree, fn2, newargs);
	    default:
		return copy.Apply(tree, fn1, args1);
	    }

	case Select(_, _):
	case Ident(_):
	    Tree tree1 = super.transform(tree);
	    switch (tree1.symbol().type()) {
	    case PolyType(Symbol[] tparams, Type restp):
		if (tparams.length == 0 && !(restp instanceof Type.MethodType)) {
		    return gen.Apply(asMethod(tree1), new Tree[0]);
		} else {
		    return tree1;
		}
	    default:
		if (tree1.symbol().isDefParameter()) {
		    tree1.type = global.definitions.functionType(
			Type.EMPTY_ARRAY, tree1.type);
		    return gen.Apply(gen.Select(tree1, Names.apply), new Tree[0]);
		} else {
		    return tree1;
		}
	    }

	default:
	    return super.transform(tree);
	}
    }

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
	    if (params.length == 1 && (params[0].flags & REPEATED) != 0) {
		assert (args.length != 1 || !(args[0] instanceof Tree.Tuple));
		args = new Tree[]{make.Tuple(pos, args).setType(params[0].type())};
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
	    throw new ApplicationError(methtype);
	}
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
		    global.debugPrinter.print(arg1).flush();//debug
		    throw new ApplicationError();
		}
	    }
	    return transform(
		gen.mkUnitFunction(arg, descr.uncurry(arg.type), currentOwner));
	} else {
	    return transform(arg);
	}
    }
}
