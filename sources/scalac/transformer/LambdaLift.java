/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.transformer;

import java.io.*;
import java.util.HashMap;
import scalac.*;
import scalac.util.*;
import scalac.ast.*;
import scalac.symtab.*;
import Tree.*;

/** A lambda lifting transformer
 *
 *  @author     Martin Odersky
 *  @version    1.0
 *
 *  What it does:
 *  Lift every class and function that's contained in another function
 *  out to be a member to the next enclosing class.
 *  Pass free variables and type variables as parameters to class constructors.
 *  Variables and values that are free in some function or class
 *  are given new unique names.
 *  Free mutable variables are converted to reference cells.
 *  All types are updated so that proxies are passed for additional free
 *  type variables.
 */
public class LambdaLift extends OwnerTransformer
                        implements Modifiers, Kinds {

    final Global global;
    final Definitions definitions;
    final FreeVars free;
    final LambdaLiftPhase descr;

    public LambdaLift(Global global, LambdaLiftPhase descr) {
        super(global);
	this.global = global;
	this.definitions = global.definitions;
	this.free = new FreeVars(global);
	this.descr = descr;
    }

    public void apply(Unit unit) {
	global.log(unit.source.toString());
	free.initialize(unit);
	currentOwner = global.definitions.ROOT_CLASS;
	unit.body = transformTemplateStats(unit.body, currentOwner);
    }

    /** If `sym' is a class, return its primary constructor;
     *  otherwise current symbol itself
     */
    static Symbol asFunction(Symbol sym) {
	return sym.kind == CLASS ? sym.constructor() : sym;
    }

    /** `asFunction' applied to the next enclosing function or class owner.
     */
    static Symbol enclFun(Symbol owner) {
	Symbol sym = owner;
	while (!asFunction(sym).isMethod()) sym = sym.owner();
	return asFunction(sym);
    }

    /** Return SymSet from a hashmap.
     */
    private static SymSet get(HashMap f, Symbol sym) {
	SymSet ss = (SymSet) f.get(sym);
	return ss == null ? SymSet.EMPTY : ss;
    }

    /** Compute free variables map `fvs'.
     *  Also assign unique names to all
     *  value/variable/let symbols that are free in some function or class, and to
     *  all class/function symbols that are owned by some function.
     */
    static class FreeVars extends OwnerTransformer {

	private Unit unit;

	public FreeVars(Global global) {
	    super(global);
	}

	/** A hashtable storing free variables of functions and class constructors.
	 */
	private HashMap/*<Symbol,SymSet>*/ fvs;

	/** A hashtable storing free type variables of functions and class constructors.
	 */
	private HashMap/*<Symbol,SymSet>*/ ftvs;

	/** A hashtable storing calls between functions and class constructors
	 */
	private HashMap/*<Symbol,SymSet>*/ called;

	/** The set of symbols that need to be renamed.
	 */
	private SymSet renamable;

	/** The set of symbols that bound by polytypes
	 *  and therefore are not free type variables.
	 */
	private SymSet excluded;

	/** A flag to indicate whether new free variables have been found
	 */
	private boolean changedFreeVars;

	/** A flag to indicate whether we are in propagation phase
	 *  (used in assertion).
	 */
	private boolean propagatePhase;

	/** Insert `sym' into the set of free variables of `owner'
	 */
	private void putFree(Symbol owner, Symbol sym) {
	    assert owner.isLocal();
	    HashMap f = sym.isType() ? ftvs : fvs;
	    SymSet ss = get(f, owner);
	    if (!ss.contains(sym)) {
		f.put(owner, ss.incl(sym));
		changedFreeVars = true;
		if (global.debug) global.log(sym + " is free in " + owner);
	    }
	}

	/** Insert `to' into the set of functions called by `from'
	 */
	private void putCall(Symbol from, Symbol to) {
	    SymSet ss = get(called, from);
	    if (!ss.contains(to)) {
		called.put(from, ss.incl(to));
	    }
	}

	/** Mark symbol `sym' as being free in `owner', unless `sym'
	 *  is defined in `owner' or there is a class between `owner's owner
	 *  and the owner of `sym'.
	 *  Return `true' if there is no class between `owner' and
	 *  the owner of sym.
	 */
	private boolean markFree(Symbol sym, Symbol owner) {
	    if (global.debug) global.log("mark " + sym + " of " + sym.owner() + " free in " + owner);//debug
	    if (owner.kind == NONE) {
		assert propagatePhase : sym + " in " + sym.owner();
		return false;
	    } else if (sym.owner() == owner) {
		return true;
	    } else if (markFree(sym, owner.owner())) {
		Symbol fowner = asFunction(owner);
		if (fowner.isMethod()) {
		    putFree(fowner, sym);
		    renamable = renamable.incl(sym);
		    if (sym.isVariable()) sym.flags |= CAPTURED;
		}
		return owner.kind != CLASS;
	    } else {
		return false;
	    }
	}

	/** Assign unique name to symbol.
	 *  If symbol is a class assign same name to its primary constructor.
	 */
	private void makeUnique(Symbol sym) {
	    Name newname = global.freshNameCreator.newName(sym.name);
	    sym.name = newname;
	    if (sym.kind == CLASS)
		sym.constructor().name = newname.toConstrName();
	}

	private Type.Map traverseTypeMap = new Type.Map() {
	    public Type apply(Type tp) {
	        switch (tp) {
		case TypeRef(ThisType(_), Symbol sym, Type[] targs):
		    if (sym.isLocal() && sym.kind == TYPE && !excluded.contains(sym))
			markFree(sym, currentOwner);
		    break;
		case PolyType(Symbol[] tparams, Type restp):
		    for (int i = 0; i < tparams.length; i++)
			excluded = excluded.incl(tparams[i]);
		    Type tp1 = super.map(tp);
		    for (int i = 0; i < tparams.length; i++)
			excluded = excluded.excl(tparams[i]);
		    return tp1;
		}
		return map(tp);
	    }
        };

	public Tree transform(Tree tree) {
	    //if (global.debug) global.debugPrinter.print("free ").print(tree).println().end();//DEBUG
	    assert tree.type != null : tree;
	    traverseTypeMap.apply(tree.type.widen());
	    Symbol sym = tree.symbol();
	    switch(tree) {
	    case ClassDef(_, _, _, _, _, _):
	    case DefDef(_, _, _, _, _, _):
		if (sym.isLocal()) {
		    renamable = renamable.incl(sym);
		}
		return super.transform(tree);

	    case TypeDef(int mods, Name name, Tree rhs, Tree lobound):
		// ignore type definition as owner.
		// reason: it might be in a refinement
		// todo: handle type parameters?
		return copy.TypeDef(
		    tree, sym,
		    transform(rhs, currentOwner),
		    transform(lobound, currentOwner));

	    case Ident(_):
		if (sym.isLocal()) {
		    if (sym.isMethod()) {
			Symbol f = enclFun(currentOwner);
			if (f.name.length() > 0) // it is not a template function {
			    putCall(f, sym);
		    } else if (sym.kind == VAL || sym.kind == TYPE) {
			markFree(sym, currentOwner);
		    }
		}
		return tree;

	    default:
		return super.transform(tree);
	    }
	}

	/** Propagate free fariables from all called functions.
	 */
	void propagateFvs(HashMap fvs) {
	    Object[] fs = called.keySet().toArray();
	    for (int i = 0; i < fs.length; i++) {
		Symbol f = (Symbol) fs[i];
		Symbol[] calledFromF = get(called, f).toArray();
		for (int j = 0; j < calledFromF.length; j++) {
		    //System.out.println(f + " calls " + calledFromF[j]);//DEBUG
		    Symbol[] newFvs = get(fvs, calledFromF[j]).toArray();
		    for (int k = 0; k < newFvs.length; k++) {
			markFree(newFvs[k], f);
		    }
		}
	    }
	}

	/** This method re-enters all free variables into their free variable sets
	 *  This is necessary because the names of these variables (and therefore their
	 *  `isLess' order have changed.
	 */
	void restoreFvs(HashMap fvs) {
	    Object[] fs = fvs.keySet().toArray();
	    for (int i = 0; i < fs.length; i++) {
		Symbol f = (Symbol) fs[i];
		Symbol[] elems = get(fvs, f).toArray();
		SymSet elems1 = SymSet.EMPTY;
		for (int j = 0; j < elems.length; j++)
		    elems1 = elems1.incl(elems[j]);
		fvs.put(f, elems1);
	    }
	}

	/** Compute a mapping from symbols to their free variables
	 *  in hashtable `fvs'. Also rename all variables that need it.
	 */
	public void initialize(Unit unit) {
	    this.unit = unit;
	    fvs = new HashMap();
	    ftvs = new HashMap();
	    called = new HashMap();
	    renamable = SymSet.EMPTY;
	    excluded = SymSet.EMPTY;
	    apply(unit);

	    propagatePhase = true;
	    do {
		changedFreeVars = false;
		propagateFvs(fvs);
		propagateFvs(ftvs);
	    } while (changedFreeVars);

	    Symbol[] ss = renamable.toArray();
	    for (int i = 0; i < ss.length; i++) {
		makeUnique(ss[i]);
	    }

	    restoreFvs(fvs);
	    restoreFvs(ftvs);
	}
    }

    private TreeList liftedDefs;

    /** Transform template and add lifted definitions to it.
     */
    public Tree[] transformTemplateStats(Tree[] stats, Symbol tsym) {
	TreeList prevLiftedDefs = liftedDefs;
	liftedDefs = new TreeList();
	TreeList stats1 = new TreeList(super.transformTemplateStats(stats, tsym));
	stats1.append(liftedDefs);
	liftedDefs = prevLiftedDefs;
	return stats1.toArray();
    }

    public Tree transform(Tree tree) {
	//global.debugPrinter.print("lifting ").print(tree).println().end();//DEBUG
	//System.out.print(tree.type + " --> ");//DEBUG
	tree.type = descr.transform(tree.type, currentOwner);
	//System.out.println(tree.type);//DEBUG
        switch (tree) {
	case Block(Tree[] stats):
	    for (int i = 0; i < stats.length; i++)
		liftSymbol(stats[i]);
	    return copy.Block(tree, transform(stats));

	case ClassDef(int mods, _, TypeDef[] tparams, ValDef[][] vparams, Tree tpe, Template impl):
	    Symbol sym = tree.symbol();
	    if ((mods & LIFTED) != 0) {
		((ClassDef) tree).mods &= ~LIFTED;
		Tree tree1 = copy.ClassDef(
		    tree, sym,
		    addTypeParams(transform(tparams, sym), newtparams(sym.constructor())),
		    new ValDef[][]{
			addParams(transform(vparams, sym)[0], newparams(sym.constructor()))},
		    transform(tpe, sym),
		    transform(impl, sym));
		liftedDefs.append(tree1);
		return Tree.Empty;
	    } else {
		assert !sym.isLocal() : sym;
		return copy.ClassDef(
		    tree, sym,
		    transform(tparams, sym),
		    transform(vparams, sym),
		    transform(tpe, sym),
		    transform(impl, sym));
	    }

	case DefDef(int mods, _, TypeDef[] tparams, ValDef[][] vparams, Tree tpe, Tree rhs):
	    Symbol sym = tree.symbol();
	    if ((mods & LIFTED) != 0) {
		((DefDef) tree).mods &= ~LIFTED;
		Tree tree1 = copy.DefDef(
		    tree, sym,
		    addTypeParams(transform(tparams, sym), newtparams(sym)),
		    new ValDef[][]{
			addParams(transform(vparams, sym)[0], newparams(sym))},
		    transform(tpe, sym),
		    transform(rhs, sym));
		liftedDefs.append(tree1);
		return Tree.Empty;
	    } else {
		assert !sym.isLocal() : sym;
		return copy.DefDef(
		    tree, sym,
		    transform(tparams, sym), transform(vparams, sym), transform(tpe, sym),
		    transform(rhs, sym));
	    }

	case TypeDef(int mods, Name name, Tree rhs, Tree lobound):
	    // ignore type definition as owner.
	    // reason: it might be in a refinement
	    // todo: handle type parameters?
	    return copy.TypeDef(
		tree, tree.symbol(),
		transform(rhs, currentOwner),
		transform(lobound, currentOwner));

	case ValDef(_, _, Tree tpe, Tree rhs):
	    Symbol sym = tree.symbol();
	    Tree tpe1 = transform(tpe);
	    Tree rhs1 = transform(rhs, sym);
	    if ((sym.flags & CAPTURED) != 0) {
		assert sym.isLocal();
		Type boxedType = sym.typeAt(descr.nextPhase);
		Type unboxedType = boxedType.typeArgs()[0];
		tpe1 = gen.mkType(tpe.pos, boxedType);
		rhs1 = gen.New(
		    rhs.pos,
		    definitions.SCALA_TYPE,
		    boxedType.symbol(),
		    new Type[]{unboxedType},
		    new Tree[]{rhs1});
	    }
	    return copy.ValDef(tree, sym, tpe1, rhs1);

	case Sequence(Tree[] args):
	    Tree tree1 = mkList(tree.pos, tree.type, transform(args));
	    //new scalac.ast.printer.TextTreePrinter().print("TUPLE: ").print(tree).print("\n ==> \n").print(tree1).println().end();//DEBUG
	    return tree1;

	case Apply(Tree fn, Tree[] args):
	    Symbol fsym = TreeInfo.methSymbol(fn);
	    Tree fn1 = transform(fn);
	    switch (fn1) {
	    case TypeApply(Tree fn2, Tree[] targs):
		fn1 = copy.TypeApply(
		    fn1, fn2, addFreeArgs(tree.pos, get(free.ftvs, fsym), targs));
		break;
	    default:
 		Tree[] targs = addFreeArgs(
		    tree.pos, get(free.ftvs, fsym), Tree.EMPTY_ARRAY);
		if (targs.length > 0)
		    fn1 = gen.TypeApply(fn1, targs);
	    }
	    Tree[] args1 = transform(args);
	    return copy.Apply(
		tree, fn1, addFreeArgs(tree.pos, get(free.fvs, fsym), args1));

	case Ident(Name name):
	    Symbol sym = tree.symbol();
	    if (sym.isLocal() &&
		(sym.kind == TYPE || (sym.kind == VAL && !sym.isMethod()))) {
		sym = descr.proxy(sym, currentOwner);
	    }
	    Tree tree1 = copy.Ident(tree, sym).setType(
		sym.typeAt(descr.nextPhase));
	    if (name != sym.name) ((Ident)tree1).name = sym.name;
	    if ((sym.flags & CAPTURED) != 0) return gen.Select(tree1, Names.elem);
	    else return tree1;

	default:
	    return super.transform(tree);
        }
    }

    Symbol[] ftvsParams(Symbol owner) {
	Symbol[] freevars = get(free.ftvs, owner).toArray();
	Symbol[] params = new Symbol[freevars.length];
	for (int i = 0; i < params.length; i++) {
	    params[i] = freevars[i].cloneSymbol(owner);
	    params[i].pos = owner.pos;
	    params[i].flags = PARAM | SYNTHETIC;
	}
	for (int i = 0; i < params.length; i++)
	    params[i].setInfo(freevars[i].info().subst(freevars, params));
	return params;
    }

    Symbol[] fvsParams(Symbol owner) {
	Symbol[] freevars = get(free.fvs, owner).toArray();
	Symbol[] params = new Symbol[freevars.length];
	for (int i = 0; i < params.length; i++) {
	    params[i] = freevars[i].cloneSymbol(owner);
	    params[i].pos = owner.pos;
	    params[i].flags &= CAPTURED;
	    params[i].flags |= PARAM | SYNTHETIC;
	    params[i].setInfo(freevars[i].type());
	}
	return params;
    }

    Symbol[] newtparams(Symbol owner) {
	Symbol[] tparams = owner.typeAt(descr.nextPhase).typeParams();
	int nfree = get(free.ftvs, owner).size();
	assert nfree == tparams.length - owner.type().typeParams().length
	    : owner + " " + nfree + " " + tparams.length + " " + owner.type().firstParams().length;
	Symbol[] newtparams = new Symbol[nfree];
	System.arraycopy(tparams, tparams.length - nfree, newtparams, 0, nfree);
	return newtparams;
    }

    Symbol[] newparams(Symbol owner) {
	Symbol[] params = owner.typeAt(descr.nextPhase).firstParams();
	int nfree = get(free.fvs, owner).size();
	assert nfree == params.length - owner.type().firstParams().length;
	Symbol[] newparams = new Symbol[nfree];
	System.arraycopy(params, params.length - nfree, newparams, 0, nfree);
	return newparams;
    }

    /** change symbol of tree so that
     *  owner = currentClass
     *  newparams are added
     *  enter symbol in scope of currentClass
     */
    void liftSymbol(Tree tree) {
	switch (tree) {
	case ClassDef(_, _, _, _, _, _):
	    ((ClassDef) tree).mods |= LIFTED;
	    Symbol sym = tree.symbol();
	    assert sym.isLocal() : sym;
	    liftSymbol(sym, ftvsParams(sym.constructor()), fvsParams(sym.constructor()));
	    break;

	case DefDef(_, _, _, _, _, _):
	    ((DefDef) tree).mods |= LIFTED;
	    Symbol sym = tree.symbol();
	    assert sym.isLocal() : sym;
	    liftSymbol(sym, ftvsParams(sym), fvsParams(sym));
	}
    }

    void liftSymbol(Symbol sym, Symbol[] newtparams, Symbol[] newparams) {
	Symbol enclClass = sym.owner().enclClass();
	if (!sym.isPrimaryConstructor()) sym.setOwner(enclClass);
	enclClass.members().enter(sym);
	if (sym.isMethod()) {
	    if (newtparams.length != 0 || newparams.length != 0) {
		sym.updateInfo(
		    addParams(
			addTypeParams(sym.infoAt(descr.nextPhase), newtparams),
			newparams));
		if (global.debug)
		    global.log(sym + " has now type " + sym.typeAt(descr.nextPhase));
	    }
	} else if (sym.kind == CLASS) {
	    liftSymbol(sym.constructor(), newtparams, newparams);
	} else {
	    throw new ApplicationError();
	}
    }

    Type addTypeParams(Type tp, Symbol[] newtparams) {
	if (newtparams.length == 0) return tp;
	switch (tp) {
	case MethodType(_, _):
	    return Type.PolyType(newtparams, tp);
	case PolyType(Symbol[] tparams, Type restpe):
	    Symbol[] tparams1 = new Symbol[tparams.length + newtparams.length];
	    System.arraycopy(tparams, 0, tparams1, 0, tparams.length);
	    System.arraycopy(newtparams, 0, tparams1, tparams.length, newtparams.length);
	    return Type.PolyType(tparams1, restpe);
	default:
	    throw new ApplicationError("illegal type: " + tp);
	}
    }

    Type addParams(Type tp, Symbol[] newparams) {
	if (newparams.length == 0) return tp;
	switch (tp) {
	case MethodType(Symbol[] params, Type restpe):
	    Symbol[] params1 = new Symbol[params.length + newparams.length];
	    System.arraycopy(params, 0, params1, 0, params.length);
	    System.arraycopy(newparams, 0, params1, params.length, newparams.length);
	    return Type.MethodType(params1, restpe);
	case PolyType(Symbol[] tparams, Type restpe):
	    return Type.PolyType(tparams, addParams(restpe, newparams));
	default:
	    throw new ApplicationError("illegal type: " + tp);
	}
    }

    TypeDef[] addTypeParams(TypeDef[] tparams, Symbol[] newtparams) {
	if (newtparams.length == 0) return tparams;
	TypeDef[] tparams1 = new TypeDef[tparams.length + newtparams.length];
	System.arraycopy(tparams, 0, tparams1, 0, tparams.length);
	for (int i = 0; i < newtparams.length; i++) {
	    tparams1[tparams.length + i] = gen.mkTypeParam(newtparams[i]);
	}
	return tparams1;
    }

    ValDef[] addParams(ValDef[] params, Symbol[] newparams) {
	if (newparams.length == 0) return params;
	ValDef[] params1 = new ValDef[params.length + newparams.length];
	System.arraycopy(params, 0, params1, 0, params.length);
	for (int i = 0; i < newparams.length; i++) {
	    params1[params.length + i] = gen.mkParam(newparams[i]);
	}
	return params1;
    }

    /** For all variables or type variables in `fvs',
     *  append proxies to argument array `args'.
     */
    Tree[] addFreeArgs(int pos, SymSet fvs, Tree[] args) {
	if (fvs != SymSet.EMPTY) {
	    Symbol[] fparams = fvs.toArray();
	    Tree[] args1 = new Tree[args.length + fparams.length];
	    System.arraycopy(args, 0, args1, 0, args.length);
	    for (int i = 0; i < fparams.length; i++) {
		Symbol farg = descr.proxy(fparams[i], currentOwner);
		args1[args.length + i] =
		    gen.Ident(pos, farg).setType(farg.typeAt(descr.nextPhase));
	    }
	    return args1;
	} else {
	    return args;
	}
    }

    //todo: remove type parameters
    Tree mkList(int pos, Type tpe, Tree[] args) {
	return mkList(pos, tpe.typeArgs()[0], args, 0);
    }

    Tree mkList(int pos, Type elemtpe, Tree[] args, int start) {
	if (start == args.length) return mkNil(pos);
	else return mkCons(pos, elemtpe, args[start],
			   mkList(pos, elemtpe, args, start + 1));
    }

    Tree mkNil(int pos) {
	return gen.mkRef(pos, global.definitions.getModule(Names.scala_Nil));
    }

    Tree mkCons(int pos, Type elemtpe, Tree hd, Tree tl) {
	return gen.New(
	    gen.Apply(
		gen.TypeApply(
		    gen.mkRef(
			pos,
			global.definitions.getClass(Names.scala_COLONCOLON).constructor()),
		    new Tree[]{gen.mkType(pos, elemtpe)}),
		new Tree[]{hd, tl}));
    }
}
