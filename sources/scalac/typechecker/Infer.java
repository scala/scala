/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.typechecker;

import scalac.Global;
import scalac.ApplicationError;
import scalac.*;
import scalac.util.*;
import scalac.ast.*;
import scalac.symtab.*;

public class Infer implements Modifiers, Kinds {

    Global global;
    Definitions definitions;
    TreeGen gen;
    TreeFactory make;
    Substituter substituter;

    public Infer(Global global, TreeGen gen, TreeFactory make) {
	this.global = global;
	this.definitions = global.definitions;
	this.gen = gen;
	this.make = make;
	this.substituter = new Substituter(global, gen);
    }

    public Infer(Transformer trans) {
	this(trans.global, trans.gen, trans.make);
    }

// Error messages -------------------------------------------------------------

    String applyErrorMsg(String msg1, Tree fn,
			 String msg2, Type[] argtypes, Type pt) {
	return msg1 + toString(fn.symbol(), fn.type) + msg2 +
	    ArrayApply.toString(argtypes, "(", ",", ")") +
	    (pt == Type.AnyType ? "" : " with expected result type " + pt);
    }

    String typeErrorMsg(String msg, Type found, Type req) {
	return msg +
	    ";\n found   : " + found.toLongString() +
	     "\n required: " + req;
    }

    String overloadResolveErrorMsg(Symbol sym1, Type tpe1, Symbol sym2, Type tpe2) {
	return "ambiguous reference to overloaded definition,\n" +
	    "both " + sym1 + ": " + tpe1 + "\n" +
	    "and  " + sym2 + ": " + tpe2 + "\nmatch";
    }

    /** Give a string representation of symbol `sym' with type `tp'
     *  for error diagnostics. `sym' may be null.
     */
    static String toString(Symbol sym, Type tp) {
	return
	    (tp instanceof Type.OverloadedType ? "overloaded " : "") +
	    (sym == null ? "expression" : sym) + " of type " + tp;
    }

// Tree Substitution -------------------------------------------------------------

    static class Substituter extends Transformer {

	Symbol[] tparams;
	Type[] targs;
	TreeGen gen;
	Type.SubstTypeMap typeSubstituter;

	public Substituter(Global global, TreeGen gen) {
	    super(global);
	    this.gen = gen;
	}

        public Tree apply(Tree tree, Symbol[] tparams, Type[] targs) {
	    this.tparams = tparams;
	    this.targs = targs;
	    this.typeSubstituter = new Type.SubstTypeMap(tparams, targs) {
  	        public boolean matches(Symbol sym1, Symbol sym2) {
		    return
			sym1.name == sym2.name && sym1.owner() == sym2.owner();
		}
	    };
	    return transform(tree);
	}

	Type.Map elimInferredPolyMap = new Type.Map() {
    	    public Type apply(Type t) {
		switch (t) {
		case PolyType(Symbol[] tparams1, Type restp):
		    if (tparams1.length == tparams.length &&
			tparams1[0] == tparams[0]) {
			for (int i = 1; i < tparams.length; i++)
			    assert tparams1[i] == tparams[i];
			return apply(restp);
		    }
		}
		return map(t);
	    }
	};

	public Tree transform(Tree tree) {
//	    System.out.println("[" + ArrayApply.toString(targs,"",",","") + "/" + ArrayApply.toString(tparams,"",",","") + "]" + tree + "@" + tree.symbol());//DEBUG
	    if (tree.type != null) {
		tree.type = typeSubstituter.apply(
		    elimInferredPolyMap.apply(tree.type));
	    }
	    switch (tree) {
	    case Ident(Name name):
		if (name.isTypeName()) {
		    Symbol sym = tree.symbol();
		    for (int i = 0; i < tparams.length; i++) {
			if (typeSubstituter.matches(tparams[i], sym)) {
			    return gen.mkType(tree.pos, targs[i]);
			}
		    }
		}
		return tree;

	    case TypeApply(Tree fun, Tree[] targs):
		boolean proceed = true;
		switch (fun.type) {
		case PolyType(Symbol[] tparams1, _):
		    if (tparams1.length == tparams.length &&
			tparams1[0] == tparams[0] &&
			targs.length == tparams.length) {
			proceed = false;
			for (int i = 0; i < tparams.length; i++)
			    if (!typeSubstituter.matches(targs[i].type.symbol(), tparams[i]))
				proceed = true;
		    }
		}
		Tree fun1 = proceed ? transform(fun) : fun;
		Tree[] targs1 = transform(targs);
		return copy.TypeApply(tree, fun1, targs1);

/*
	    case TypeTerm():
		Symbol sym = tree.type.symbol();
		for (int i = 0; i < tparams.length; i++) {
		    if (tparams[i].name == sym.name &&
			tparams[i].owner() == sym.owner()) {
			return gen.mkType(tree.pos, targs[i]);
		    }
		}
		return tree;
*/
	    default:
		return super.transform(tree);
	    }
	}
    }

// Variance calculation ----------------------------------------------------------

    private static int flip(int v) {
	if (v == COVARIANT) return CONTRAVARIANT;
	else if (v == CONTRAVARIANT) return COVARIANT;
	else return v;
    }

    private static static int cut(int v) {
	if (v == VARIANCES) return v;
	else return 0;
    }

    /** Compute variances of all type parameters `tparams' in type `tp'.
     *  A variance is taken from the four point lattice
     *
     *  0, Modifiers.COVARIANT, Modifiers.CONTRAVARIANT, Modifiers.VARIANCES.
     */
    private static int[] variance(Symbol[] tparams, Type tp) {
	int[] vs = new int[tparams.length];
	for (int i = 0; i < vs.length; i++) vs[i] = variance(tparams[i], tp);
	return vs;
    }

    /** Compute variances of all type parameters `tparams' in types `tps'.
     */
    private static int[] variance(Symbol[] tparams, Type[] tps) {
	int[] vs = new int[tparams.length];
	for (int i = 0; i < vs.length; i++) vs[i] = variance(tparams[i], tps);
	return vs;
    }

    /** Compute variance of type parameter `tparam' in types of all symbols `sym'.
     */
    private static int variance(Symbol tparam, Symbol[] syms) {
	int v = VARIANCES;
	for (int i = 0; i < syms.length; i++) {
	    v = v & variance(tparam, syms[i]);
	}
	return v;
    }

    /** Compute variance of type parameter `tparam' in type of symbol `sym'.
     */
    private static int variance(Symbol tparam, Symbol sym) {
	switch (sym.kind) {
	case ERROR:
	    return VARIANCES;
	case VAL:
	    return variance(tparam, sym.info());
	case TYPE:
	    return variance(tparam, sym.info()) &
		flip(variance(tparam, sym.loBound()));
	case ALIAS:
	    return cut(variance(tparam, sym.info()));
	default:
	    return 0;
	}
    }

    /** Compute variance of type parameter `tparam' in all types `tps'.
     */
    private static int variance(Symbol tparam, Type[] tps) {
	int v = VARIANCES;
	for (int i = 0; i < tps.length; i++) {
	    v = v & variance(tparam, tps[i]);
	}
	return v;
    }

    /** Compute variance of type parameter `tparam' in all type arguments
     *  `tps' which correspond to formal type parameters `tparams'.
     */
    private static int varianceInArgs(Symbol tvar, Type[] tps, Symbol[] tparams) {
	int v = VARIANCES;
	for (int i = 0; i < tps.length; i++) {
	    if ((tparams[i].flags & COVARIANT) != 0) {
		v = v & variance(tvar, tps[i]);
	    } else if ((tparams[i].flags & CONTRAVARIANT) != 0) {
		v = v & flip(variance(tvar, tps[i]));
	    } else {
		v = v & cut(variance(tvar, tps[i]));
	    }
	}
	return v;
    }

    /** Does given `tparam' occur with variance `v' in type?
     */
    private static int variance(Symbol tparam, Type tp) {
	switch (tp) {
	case ErrorType:
	case AnyType:
	case NoType:
	case ThisType(_):
	case ConstantType(_, _):
	    return VARIANCES;
	case TypeRef(Type pre, Symbol sym, Type[] args):
	    if (sym == tparam) return COVARIANT;
	    else return variance(tparam, pre) &
		     varianceInArgs(tparam, args, sym.typeParams());
	case SingleType(Type pre, Symbol sym):
	    return cut(variance(tparam, pre));
	case CompoundType(Type[] parts, Scope members):
	    return variance(tparam, parts) & variance(tparam, members.elements());
	case MethodType(Symbol[] params, Type restype):
	    return flip(variance(tparam, params)) & variance(tparam, restype);
	case PolyType(Symbol[] tparams, Type restype):
	    return flip(variance(tparam, tparams)) & variance(tparam, restype);
	default:
	    throw new ApplicationError(tp.toString());
	}
    }

// Type parameter inference -----------------------------------------------------

    private static class NoInstance extends RuntimeException {
	NoInstance(String msg) {
	    super(msg);
	}
    }

    /** map every TypeVar to its constraint.inst field.
     *  throw a NoInstance exception if a NoType or AnyType is encountered.
     */
    private static Type.Map instantiateMap = new Type.Map() {
        public Type apply(Type t) {
	    return instantiate(t);
	}
    };

    private static Type instantiate(Type tp) throws NoInstance {
	switch (tp) {
	case AnyType:
	case NoType:
	    throw new NoInstance("undetermined type");
	case TypeVar(Type origin, Type.Constraint constr):
	    if (constr.inst != Type.NoType) return instantiate(constr.inst);
	    else throw new NoInstance("no unique instantiation of type variable " +
				      origin + " could be found");
	default:
	    return instantiateMap.map(tp);
	}
    }

    /** Map type variable to its instance, or, if `covariant' is true,
     *  to its upper bound;
     */
    private Type instantiateToBound(Type tp, int variance) {
	switch (tp) {
	case TypeVar(Type origin, Type.Constraint constr):
	    try {
		if (constr.inst != Type.NoType) {
		    return instantiate(constr.inst);
		} else if ((variance & COVARIANT) != 0 &&
			   constr.hibounds != Type.List.EMPTY) {
		    maximizeVar(tp);
		    return instantiate(constr.inst);
		} else if ((variance & CONTRAVARIANT) != 0 &&
			   constr.lobounds != Type.List.EMPTY) {
		    minimizeVar(tp);
		    return instantiate(constr.inst);
		}
	    } catch (NoInstance ex) {
	    }
	    return Type.AnyType;
	default:
	    throw new ApplicationError();
	}
    }

    /** The formal parameter types corresponding to `params'.
     *  If `params' is a repeated parameter, a list of `length' copies
     *  of its type is returned.
     */
    public Type[] formalTypes(Symbol[] params, int length) {
	Type[] result;
	if (params.length == 1 && (params[0].flags & REPEATED) != 0) {
	    Type[] formals = new Type[length];
	    Type[] args = params[0].type().typeArgs();
	    if (args.length == 1) {
		Type ft = args[0];
		// params[0] has type Seq[T], we need T here
		for (int i = 0; i < length; i++) formals[i] = ft;
		return formals;
	    }
	}
	return Symbol.type(params);
    }

    /** Is type fully defined, i.e. no embedded anytypes or typevars in it?
     */
    public boolean isFullyDefined(Type tp) {
	try {
	    instantiate(tp);
	    return true;
	} catch (NoInstance ex) {
	    return false;
	}
    }

    /** Do type arguments `targs' conform to formal parameters `tparams'?
     */
    private boolean isWithinBounds(Symbol[] tparams, Type[] targs) {
	for (int i = 0; i < targs.length; i++) {
	    Type hibound = tparams[i].info().subst(tparams, targs);
	    if (!targs[i].isSubType(hibound)) {
		for (int j = 0; j < tparams.length; j++) {
		    if (hibound.symbol() == tparams[j])
			return isWithinBounds(
			    tparams,
			    Type.subst(
				targs,
				new Symbol[]{tparams[j]},
				new Type[]{targs[i]}));
		}
		return false;
	    }
	    Type lobound = tparams[i].loBound().subst(tparams, targs);
	    if (!lobound.isSubType(targs[i])) {
		for (int j = 0; j < tparams.length; j++) {
		    if (lobound.symbol() == tparams[j])
			return isWithinBounds(
			    tparams,
			    Type.subst(
				targs,
				new Symbol[]{tparams[j]},
				new Type[]{targs[i]}));
		}
		return false;
	    }
	}
	return true;
    }

    /** throw a type error if arguments not within bounds.
     */
    void checkBounds(Symbol[] tparams, Type[] targs, String prefix) {
	if (!isWithinBounds(tparams, targs)) {
	    throw new Type.Error(
		prefix + "type arguments " +
		ArrayApply.toString(targs, "[", ",", "]") + " do not conform to " +
		tparams[0].owner() + "'s type parameter bounds " +
		ArrayApply.toString(Symbol.defString(tparams), "[", ",", "]"));
	}
    }

    /** Instantiate variable to glb of its high bounds.
     */
    private void maximizeVar(Type tp) {
	switch (tp) {
	case TypeVar(Type origin, Type.Constraint constr):
	    if (constr.inst == Type.NoType)
		constr.inst = Type.glb(constr.hibounds.toArray());
	    break;
	default:
	    throw new ApplicationError();
	}
    }

    /** Instantiate variable to lub of its low bounds.
     */
    private void minimizeVar(Type tp) {
	switch (tp) {
	case TypeVar(Type origin, Type.Constraint constr):
	    if (constr.inst == Type.NoType)
		constr.inst = Type.lub(constr.lobounds.toArray());
	    break;
	default:
	    throw new ApplicationError();
	}
    }

    /** Solve constraint collected in types `tvars', instantiating `tvars[i]'
     *  in the process.
     *  @param tparams    The type parameters corresponding to `tvars'
     *  @param upper      When `true' search for max solution else min.
     *  @param variances  The variances of type parameters; need to reverse
     *                    solution direction for all contravariant variables.
     *  @param tvars      All type variables to be instantiated.
     *  @param i          The index of the type variable to be instantiated.
     */
    private void solve(Symbol[] tparams, boolean upper, int[] variances, Type[] tvars, int i)
        throws NoInstance {
	if (tvars[i] != Type.NoType) {
	    switch (tvars[i]) {
	    case TypeVar(Type origin, Type.Constraint constr):
		if (constr.inst != Type.NoType) {
		    constr.inst = tvars[i] = instantiate(constr.inst);
		} else {
		    Type tvar = tvars[i];
		    boolean up = (variances[i] != CONTRAVARIANT) ? upper
			: !upper;
		    tvars[i] = Type.NoType;
		    Type bound = up ? tparams[i].info() : tparams[i].loBound();
		    boolean cyclic = false;
		    for (int j = 0; j < tvars.length; j++) {
			if (bound.contains(tparams[j]) ||
			    up && tparams[j].loBound().isSameAs(tparams[i].type()) ||
			    !up && tparams[j].info().isSameAs(tparams[i].type())) {
			    cyclic |= tvars[j] == Type.NoType;
			    solve(tparams, upper, variances, tvars, j);
			}
		    }
		    if (!cyclic) {
			if (up) {
			    if (bound.symbol() != Global.instance.definitions.ANY_CLASS)
				constr.hibounds = new Type.List(
				    bound.subst(tparams, tvars), constr.hibounds);
			    for (int j = 0; j < tvars.length; j++) {
				if (tparams[j].loBound().isSameAs(
					tparams[i].type())) {
				    constr.hibounds = new Type.List(
					tparams[j].type().subst(tparams, tvars),
					constr.hibounds);
				}
			    }
			} else {
			    if (bound.symbol() != Global.instance.definitions.ALL_CLASS)
				constr.lobounds = new Type.List(
				    bound.subst(tparams, tvars), constr.lobounds);
			    for (int j = 0; j < tvars.length; j++) {
				if (tparams[j].info().isSameAs(
					tparams[i].type())) {
				    constr.lobounds = new Type.List(
					tparams[j].type().subst(tparams, tvars),
					constr.lobounds);
				}
			    }
			}
		    }
		    if (up) maximizeVar(tvar);
		    else minimizeVar(tvar);
		    tvars[i] = ((Type.TypeVar) tvar).constr.inst;
		}
	    }
	}
    }

    /** Generate an array of fresh type variables corresponding to parameters
     *  `tparams'
     */
    private Type[] freshVars(Symbol[] tparams) {
	Type[] tvars = new Type[tparams.length];
	for (int i = 0; i < tvars.length; i++) {
	    tvars[i] = Type.TypeVar(tparams[i].type(), new Type.Constraint());
	}
	return tvars;
    }

    private Type.Map freshInstanceMap = new Type.Map() {
        public Type apply(Type t) {
	    switch (t) {
	    case PolyType(Symbol[] tparams, Type restp):
		Type restp1 = apply(restp);
		Symbol[] tparams1 = Symbol.EMPTY_ARRAY;
		Symbol[] newparams1 = Symbol.EMPTY_ARRAY;
		switch (restp1) {
		case PolyType(_, _):
		    // If there is a nested polytype, we need to
		    // substitute also its new type parameters for its old ones
		    // here. Reason: The outer polytype may refer to type
		    // variables of the inner one.
		    tparams1 = restp.typeParams();
		    newparams1 = restp1.typeParams();
		}
		Symbol[] newparams = new Symbol[tparams.length];
		for (int i = 0; i < tparams.length; i++)
		    newparams[i] = tparams[i].cloneSymbol();
		for (int i = 0; i < tparams.length; i++) {
		    newparams[i].setInfo(
			newparams[i].info()
			.subst(tparams, newparams)
			.subst(tparams1, newparams1));
		    newparams[i].setLoBound(
			newparams[i].loBound()
			.subst(tparams, newparams)
			.subst(tparams1, newparams1));
		}
		return Type.PolyType(
		    newparams, restp1.subst(tparams, newparams));

	    case OverloadedType(_, _):
		return map(t);
	    default:
		return t;
	    }
	}
    };

    public Type freshInstance(Type tp) {
	return freshInstanceMap.apply(tp);
    }

    /** Automatically perform the following conversions on expression types:
     *  A method type becomes the corresponding function type.
     *  A nullary method type becomes its result type.
     */
    private Type normalize(Type tp) {
	switch (tp) {
	case MethodType(Symbol[] params, Type restype):
	    return global.definitions.FUNCTION_TYPE(
		Symbol.type(params), normalize(restype));
	case PolyType(Symbol[] tparams, Type restype):
	    if (tparams.length == 0) return normalize(restype);
	}
	return tp;
    }

    /** Is normalized type `tp' a subtype of prototype `pt'?
     */
    boolean isCompatible(Type tp, Type pt) {
	return normalize(tp).isSubType(pt);
    }

    /** Type arguments mapped to `scala.All' are taken to be uninstantiated.
     *  Map all those type arguments to their corresponding type parameters
     *  and return all these type parameters as result.
     */
    private Symbol[] normalizeArgs(Type[] targs, Symbol[] tparams) {
	Type.List uninstantiated = Type.List.EMPTY;
	for (int i = 0; i < targs.length; i++) {
	    if (targs[i].symbol() == Global.instance.definitions.ALL_CLASS) {
		targs[i] = tparams[i].type();
		uninstantiated = Type.List.append(uninstantiated, targs[i]);
	    }
	}
	return Type.symbol(uninstantiated.toArray());
    }

    /** Return inferred type arguments of polymorphic expression, given
     *  its type parameters and result type and a prototype `pt'.
     *  If no minimal type variables exist that make the
     *  instantiated type a subtype of `pt', return `null'.
     */
    private Type[] exprTypeArgs(Symbol[] tparams, Type restype, Type pt) {
	Type[] tvars = freshVars(tparams);
	Type insttype = restype.subst(tparams, tvars);
	if (isCompatible(insttype, pt)) {
	    try {
	    	restype = normalize(restype);
		for (int i = 0; i < tvars.length; i++) {
		    solve(tparams, false, variance(tparams, restype), tvars, i);
		}
		return tvars;
	    } catch (NoInstance ex) {
	    }
	}
	return null;
    }

    /** Return inferred proto-type arguments of function, given
     *  its type and value parameters and result type, and a
     *  prototype `pt' for the function result.
     *  Type arguments need to be either determined precisely by
     *  the prototype, or they are maximized, if they occur only covariantly
     *  in the value parameter list.
     *  If instantiation of a type parameter fails,
     *  take Type.AnyType for the proto-type argument.
     */
    public Type[] protoTypeArgs(Symbol[] tparams, Type restype, Type pt,
				Symbol[] params) {
	Type[] tvars = freshVars(tparams);
	Type insttype = restype.subst(tparams, tvars);
	Type[] targs = new Type[tvars.length];
	for (int i = 0; i < tvars.length; i++) targs[i] = Type.AnyType;
	if (isCompatible(insttype, pt)) {
	    try {
		for (int i = 0; i < tvars.length; i++) {
		    targs[i] = instantiateToBound(
			tvars[i], variance(tparams[i], params));
		}
		return targs;
	    } catch (NoInstance ex) {
	    }
	}
	return targs;
    }

    /** Return inferred type arguments, given type parameters, formal parameters,
     *  argument types, result type and expected result type.
     *  If this is not possible, throw a `NoInstance' exception, or, if
     *  `needToSucceed' is false alternatively return `null'.
     *  Undetermined type arguments are represented by `definitions.ALL_TYPE'.
     *  No check that inferred parameters conform to their bounds is made here.
     */
    private Type[] methTypeArgs(Symbol[] tparams,
				Symbol[] params, Type[] argtypes,
				Type restp, Type pt,
				boolean needToSucceed) throws NoInstance {
	//System.out.println("methTypeArgs, tparams = " + ArrayApply.toString(tparams) + ", params = " + ArrayApply.toString(params) + ", type(params) = " + ArrayApply.toString(Symbol.type(params)) + ", argtypes = " + ArrayApply.toString(argtypes));//DEBUG

	Type[] tvars = freshVars(tparams);
	Type[] formals = formalTypes(params, argtypes.length);
	if (formals.length != argtypes.length) {
	    if (needToSucceed)
		throw new NoInstance("parameter lists differ in length");
	    return null;
	}

	// check first whether type variables can be fully defined from
	// expected result type.
	if (!isCompatible(restp.subst(tparams, tvars), pt)) {
	    if (needToSucceed)
		throw new NoInstance("result type " + restp +
				     " is incompatible with expected type " + pt);
	    return null;
	}
	for (int i = 0; i < tvars.length; i++) {
	    Type.TypeVar tvar = (Type.TypeVar) tvars[i];
	    if (!isFullyDefined(tvar)) tvar.constr.inst = Type.NoType;
	}

	// Then define remaining type variables from argument types.
	for (int i = 0; i < argtypes.length; i++) {
	    if (!isCompatible(argtypes[i].widen().subst(tparams, tvars),
			      formals[i].subst(tparams, tvars))) {
		if (needToSucceed) {
		    if (global.explaintypes) {
			Type.explainSwitch = true;
			argtypes[i].widen().subst(tparams, tvars).isSubType(
			    formals[i].subst(tparams, tvars));
			Type.explainSwitch = false;
		    }
		    throw new NoInstance(
			typeErrorMsg(
			    "argument expression's type is not compatible with formal parameter type",
			    argtypes[i].widen().subst(tparams, tvars),
			    formals[i].subst(tparams, tvars)));
		}
		return null;
	    }
	}
	for (int i = 0; i < tvars.length; i++) {
	    solve(tparams, false, variance(tparams, formals), tvars, i);
	}
	//System.out.println(" = " + ArrayApply.toString(tvars));//DEBUG
	return tvars;
    }

    /** Create and attribute type application node. Pass arguments for that
     *  `tparams' prefix which is owned by the tree's symbol. If there are remaining
     *  type parameters, substitute corresponding type arguments for them in the
     *  tree. Such remaining type parameters always come from an inferred PolyType.
     */
    public Tree mkTypeApply(Tree tree, Symbol[] tparams, Type restype, Type[] targs) {
	Tree tree1 = tree;
	Symbol sym = tree.symbol();
	int i = 0;
	while (i < tparams.length && tparams[i].owner() == sym)
	    i++;
	if (i < tparams.length) {
	    //System.out.println("tpar " + tparams[i] + " of " + tparams[i].owner() + " <> " + sym);//DEBUG
	    //new Printer().print(tree1);//DEBUG
	    //System.out.println(ArrayApply.toString(targs) + "/" + i + "/" + ArrayApply.toString(tparams));//DEBUG
	    Symbol[] tparams1 = new Symbol[tparams.length - i];
	    System.arraycopy(tparams, i, tparams1, 0, tparams1.length);
	    Type[] targs1 = new Type[tparams.length - i];
	    System.arraycopy(targs, i, targs1, 0, targs1.length);
	    tree1 = substituter.apply(tree1, tparams1, targs1);
	}
	if (0 < i) {
	    Tree[] argtrees = new Tree[i];
	    for (int j = 0; j < i; j++)
		argtrees[j] = gen.mkType(tree.pos, targs[j]);
	    tree1 = make.TypeApply(tree.pos, tree1, argtrees);
	}
	//System.out.println(Sourcefile.files[Position.file(tree1.pos)] + ": ");
	return tree1.setType(restype.subst(tparams, targs));
    }

    /** Return the instantiated and normalized type of polymorphic expression
     *  with type `[tparams]restype', given two prototypes `pt1', and `pt2'.
     *  `pt1' is the strict first attempt prototype where type parameters
     *  are left unchanged. `pt2' is the fall-back prototype where type parameters
     *  are replaced by `AnyType's. We try to instantiate first to `pt1' and then,
     *  if this fails, to `pt2'. If both attempts fail, a Type.Error is thrown.
     */
    Type argumentTypeInstance(Symbol[] tparams, Type restype, Type pt1, Type pt2)
	                      throws Type.Error {
	switch (restype) {
	case PolyType(Symbol[] tparams1, Type restype1):
	    Symbol[] tparams2 = new Symbol[tparams.length + tparams1.length];
	    System.arraycopy(tparams, 0, tparams2, 0, tparams.length);
	    System.arraycopy(tparams1, 0, tparams2, tparams.length, tparams1.length);
	    return argumentTypeInstance(tparams2, restype1, pt1, pt2);
	default:
	    if (tparams.length != 0) {
		Type[] targs = exprTypeArgs(tparams, restype, pt1);
		if (targs == null)
		    targs = exprTypeArgs(tparams, restype, pt2);
		if (targs == null)
		    throw new Type.Error(
			typeErrorMsg(
			    "polymorphic argument cannot be instantiated to formal parameter type",
			    Type.PolyType(tparams, restype), pt2));
		checkBounds(tparams, targs, "inferred ");
		return restype.subst(tparams, targs);
	    } else {
		return normalize(restype);
	    }
	}
    }

    /** Instantiate expression `tree' of polymorphic type [tparams]restype,
     *  using prototype `pt'.
     */
    public Tree exprInstance(Tree tree, Symbol[] tparams, Type restype, Type pt)
                            throws Type.Error {
	switch (restype) {
	case PolyType(Symbol[] tparams1, Type restype1):
	    Symbol[] tparams2 = new Symbol[tparams.length + tparams1.length];
	    System.arraycopy(tparams, 0, tparams2, 0, tparams.length);
	    System.arraycopy(tparams1, 0, tparams2, tparams.length, tparams1.length);
	    return exprInstance(tree, tparams2, restype1, pt);
	}
	Type[] targs = exprTypeArgs(tparams, restype, pt);
	if (targs == null)
	    throw new Type.Error(
		"polymorphic expression of type " + tree.type +
		" cannot be instantiated from expected type " + pt);
	checkBounds(tparams, targs, "inferred ");
	return mkTypeApply(tree, tparams, restype, targs);
    }

    /** Instantiate method `tree' of polymorphic type [tparams]restype,
     *  so that resulting method type can be applied to arguments with
     *  types `argtypes' and its result type is compatible with `pt'.
     */
    public Tree methodInstance(Tree tree,
			       Symbol[] tparams, Type restype,
			       Type[] argtypes, Type pt)
	                       throws Type.Error {
	switch (restype) {
	case PolyType(Symbol[] tparams1, Type restype1):
	    Symbol[] tparams2 = new Symbol[tparams.length + tparams1.length];
	    System.arraycopy(tparams, 0, tparams2, 0, tparams.length);
	    System.arraycopy(tparams1, 0, tparams2, tparams.length, tparams1.length);
	    return methodInstance(tree, tparams2, restype1, argtypes, pt);
	case MethodType(Symbol[] params, Type restpe):
	    Type[] targs;
	    try {
		targs = methTypeArgs(tparams, params, argtypes, restpe, pt, true);
	    } catch (NoInstance ex) {
		throw new Type.Error(
		    applyErrorMsg(
			"no type parameters for ", tree,
			" exist so that it can be applied to arguments ",
			Type.widen(argtypes), Type.AnyType) +
		    "\n --- because ---\n" + ex.getMessage());
	    }
	    Symbol[] uninstantiated = normalizeArgs(targs, tparams);
	    checkBounds(tparams, targs, "inferred ");
	    Type restype1 = (uninstantiated.length == 0)
		? restype
		: Type.MethodType(
		    params, Type.PolyType(uninstantiated, restpe));
	    return mkTypeApply(tree, tparams, restype1, targs);
	default:
	    return tree;
	}
    }

    /** Instantiate constructor `tree' of polymorphic type [tparams]restype',
     *  so that its the result type of `restype' matches prototype `pt'.
     *  If constructor is polymorphic, maximize all type variables under this
     *  condition.
     */
    public void constructorInstance(Tree tree,
				    Symbol[] tparams, Type restype, Type pt)
	                            throws Type.Error {
	switch (restype) {
	case PolyType(Symbol[] tparams1, Type restype1):
	    Symbol[] tparams2 = new Symbol[tparams.length + tparams1.length];
	    System.arraycopy(tparams, 0, tparams2, 0, tparams.length);
	    System.arraycopy(tparams1, 0, tparams2, tparams.length, tparams1.length);
	    constructorInstance(tree, tparams2, restype1, pt);
	    return;
	}
	Type[] tvars = freshVars(tparams);
	Type restype1 = restype.subst(tparams, tvars);
	Type ctpe1 = restype1.resultType();
	if (ctpe1.isSubType(pt)) {
	    try {
		for (int i = 0; i < tvars.length; i++) {
		    solve(tparams, true, variance(tparams, restype.resultType()),
			  tvars, i);
		}
		checkBounds(tparams, tvars, "inferred ");
		tree.setType(restype.subst(tparams, tvars));
		//System.out.println("inferred constructor type: " + tree.type);//DEBUG
	    } catch (NoInstance ex) {
		throw new Type.Error(
		    "constructor of type " + ctpe1 +
		    " can be instantiated in more than one way to expected type " +
		    pt +
		    "\n --- because ---\n" + ex.getMessage());
	    }
	} else {
	    throw new Type.Error(
		typeErrorMsg(
		    "constructor cannot be instantiated to expected type",
		    ctpe1, pt));
	}
    }

// Overload Resolution -------------------------------------------------------------

    /** Is function type `ftpe' applicable to `argtypes' and
     *  does its result conform to `pt'?
     */
    boolean isApplicable(Type ftpe, Type[] argtypes, Type pt) {
	switch (ftpe) {
	case MethodType(Symbol[] params, Type restpe):
	    // sequences ? List( a* )
	    Type[] formals = formalTypes(params, argtypes.length);
	    return
		isCompatible(restpe, pt) &&
		formals.length == argtypes.length &&
		Type.isSubType(argtypes, formals);
	case PolyType(Symbol[] tparams, MethodType(Symbol[] params, Type restpe)):
	    try {
		Type[] targs = methTypeArgs(
		    tparams, params, argtypes, restpe, pt, false);
		if (targs != null) {
		    Symbol[] uninstantiated = normalizeArgs(targs, tparams);
		    return
			isWithinBounds(tparams, targs) &&
			exprTypeArgs(uninstantiated, restpe.subst(tparams, targs), pt)
			    != null;
		}
	    } catch (NoInstance ex) {
	    }
	}
	return false;
    }

    /** Does function type `ftpe1' specialize function type `ftpe2'
     *  when both are alternatives in an overloaded function?
     */
    boolean specializes(Type ftpe1, Type ftpe2) {
	switch (ftpe1) {
	case MethodType(Symbol[] params, _):
	    return isApplicable(ftpe2, Symbol.type(params), Type.AnyType);
	case PolyType(_, MethodType(Symbol[] params, _)):
	    return isApplicable(ftpe2, Symbol.type(params), Type.AnyType);
	default:
	    return false;
	}
    }

    /** Assign `tree' the type of the alternative which matches
     *  prototype `pt', if it exists.
     *  If several alternatives match `pt', take unique parameterless one.
     *  Throw a Type.Error if several such alternatives exist.
     *  If no alternative matches, leave `tree' unchanged.
     */
    public void exprAlternative(Tree tree, Symbol[] alts,
				Type[] alttypes, Type pt)
	                        throws Type.Error {
	// first, catch the case of a missing parameter
        // list for an overloaded constructor.
	if (alts.length > 0) {
	    int i = 0;
	    while (i < alts.length &&
		   alts[i].isConstructor() &&
		   alttypes[i] instanceof Type.MethodType)
		i++;
	    if (i == alts.length)
		throw new Type.Error("missing arguments for " + alts[0]);
	}
	// second, do the normal case.
	int best = -1;
	for (int i = 0; i < alttypes.length; i++) {
	    if (isCompatible(alttypes[i], pt) &&
		(best < 0 || improves(alttypes[i], alttypes[best]))) {
		best = i;
	    }
	}
	if (best >= 0) {
	    for (int i = 0; i < alttypes.length; i++) {
		if (isCompatible(alttypes[i], pt) &&
		    best != i && !improves(alttypes[best], alttypes[i])) {
		    throw new Type.Error(overloadResolveErrorMsg(
			alts[best], alttypes[best], alts[i], alttypes[i]) +
			" expected type " + pt);
		}
	    }
	    tree.setSymbol(alts[best]).setType(alttypes[best]);
	}
    }
    //where
	private boolean improves(Type tp1, Type tp2) {
	    return tp2.isParameterized() &&
		(!tp1.isParameterized() || specializes(tp1, tp2));
	}

    /** Assign `tree' the type of an alternative
     *  which is applicable to `argtypes', and whose result type is
     *  a subtype of `pt' if it exists.
     *  If several applicable alternatives exist, take the
     *  most specialized one, or throw an error if no
     *  most specialized applicable alternative exists.
     *  If no alternative matches, leave `tree' unchanged,
     *  try to select method with pt = AnyType.
     *  If pt is AnyType, leave tree unchanged.
     */
    public void methodAlternative(Tree tree, Symbol[] alts, Type[] alttypes,
				  Type[] argtypes, Type pt)
	                         throws Type.Error {
	if (alts.length == 1) {
	    tree.setSymbol(alts[0]).setType(alttypes[0]);
	    return;
	}
	int best = -1;
	for (int i = 0; i < alttypes.length; i++) {
	    if (isApplicable(alttypes[i], argtypes, pt) &&
		(best < 0 || specializes(alttypes[i], alttypes[best])))	best = i;
	}
	if (best >= 0) {
	    for (int i = 0; i < alttypes.length; i++) {
		if (i != best &&
		    isApplicable(alttypes[i], argtypes, pt) &&
		    !(specializes(alttypes[best], alttypes[i]) &&
		      !specializes(alttypes[i], alttypes[best]))) {
		    throw new Type.Error(
			overloadResolveErrorMsg(
			alts[best], alttypes[best], alts[i], alttypes[i]) +
			" argument types " +
			ArrayApply.toString(argtypes, "(", ",", ")") +
			((pt == Type.AnyType) ? ""
			 : " and expected result type " + pt));
		}
	    }
	    tree.setSymbol(alts[best]).setType(alttypes[best]);
	} else if (pt != Type.AnyType) {
	    methodAlternative(tree, alts, alttypes, argtypes, Type.AnyType);
	}
    }

    /** Assign `tree' the type of unique polymorphic alternative with `nparams'
     *  as the number of type parameters, if it exists.
     *  Throw error if several such polymorphic alternatives exist.
     *  If no alternative matches, leave `tree' unchanged.
     */
    public void polyAlternative(Tree tree,
				Symbol[] alts, Type[] alttypes, int nparams)
	                       throws Type.Error {
	if (alts.length == 1) {
	    tree.setSymbol(alts[0]).setType(alttypes[0]);
	    return;
	}
	int i = 0;
	while (i < alttypes.length &&
	       !(alts[i].isValue() &&
		 alttypes[i].typeParams().length == nparams)) {
	    i++;
	}
	if (i < alttypes.length) {
	    for (int j = i + 1; j < alttypes.length; j++) {
		if (alts[j].isValue() &&
		    alttypes[j].typeParams().length == nparams)
		    throw new Type.Error(overloadResolveErrorMsg(
			alts[i], alttypes[i], alts[j], alttypes[j]) +
			" polymorphic function with " + nparams + " parameters");
	    }
	    tree.setSymbol(alts[i]).setType(alttypes[i]);
	}
    }
}

