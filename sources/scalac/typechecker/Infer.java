/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**
** $Id$
\*                                                                      */

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

    public Infer(Transformer trans) {
	this.global = trans.global;
	this.definitions = global.definitions;
	this.gen = trans.gen;
	this.make = trans.make;
	this.substituter = new Substituter(global, trans.descr, gen);
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
	    "and  " + sym2 + ": " + tpe2 + " match.";
    }

    /** Give a string representation of symbol `sym' with type `tp'
     *  for error diagnostics. `sym' may be null.
     */
    static String toString(Symbol sym, Type tp) {
	return
	    (tp instanceof Type.OverloadedType ? "overloaded " : "") +
	    (sym == null ? "expression" : sym) + " of type " + tp;
    }

// Helper definitions ---------------------------------------------------------

    /** Is type `tp' a polymorphic method type?
     */
    private boolean isPolymorphic(Type tp) {
	return tp.typeParams().length > 0;
    }

    /** Is type `tp' a parameterized method type?
     */
    /** Is type `tp' a parameterized method type?
     */
    boolean isParameterized(Type tp) {
	switch (tp) {
	case MethodType(_, _): return true;
	default: return isPolymorphic(tp);
	}
    }

// Tree Substitution -------------------------------------------------------------

    static class Substituter extends Transformer {

	Symbol[] tparams;
	Type[] targs;
	TreeGen gen;

	public Substituter(Global global, PhaseDescriptor descr, TreeGen gen) {
	    super(global, descr);
	    this.gen = gen;
	}

        public Tree apply(Tree tree, Symbol[] tparams, Type[] targs) {
	    this.tparams = tparams;
	    this.targs = targs;
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
	    if (tree.type == null) return tree;
	    tree.type = elimInferredPolyMap.apply(tree.type).subst(tparams, targs);
	    switch (tree) {
	    case Ident(Name name):
		if (name.isTypeName()) {
		    Symbol sym = tree.symbol();
		    for (int i = 0; i < tparams.length; i++) {
			if (tparams[i].name == sym.name &&
			    tparams[i].owner() == sym.owner()) {
			    return gen.mkType(tree.pos, targs[i]);
			}
		    }
		}
		return tree;
	    default:
		return super.transform(tree);
	    }
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
     *  to its upper bound (if this is not AnyType);
     *  or return `AnyType' if not possible.
     */
    private Type instantiateUpper(Type tp, boolean covariant) throws NoInstance {
	switch (tp) {
	case TypeVar(Type origin, Type.Constraint constr):
	    if (constr.inst != Type.NoType) {
		return instantiate(constr.inst);
	    } else if (covariant && constr.hibounds != Type.List.EMPTY) {
		maximizeVar(tp);
		return instantiate(constr.inst);
	    }
	    return Type.AnyType;
	default:
	    throw new ApplicationError();
	}
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
	// check that covariant types do not appear in F-bounds.
	for (int i = 0; i < targs.length; i++) {
	    if (targs[i].isCovarType()) {
		for (int j = 0; j < tparams.length; j++)
		    if (tparams[j].info().contains(tparams[i]))
			return false;
	    }
	}
	for (int i = 0; i < targs.length; i++) {
	    if (!targs[i].dropVariance().isSubType(
		    tparams[i].info().subst(tparams, targs)))
		return false;
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

    /** Instantiate undetermined variable to its minimal upper bound.
     *  Throw a NoInstance exception if this not possible.
     */
    private void maximizeVar(Type tp) throws NoInstance {
	switch (tp) {
	case TypeVar(Type origin, Type.Constraint constr):
	    if (constr.inst == Type.NoType) {
		if (constr.hibounds == Type.List.EMPTY)
		    constr.inst = definitions.ANY_TYPE;
		else if (constr.hibounds.tail == Type.List.EMPTY)
		    constr.inst = constr.hibounds.head;
		else {
		    for (Type.List bs = constr.hibounds;
			 bs != Type.List.EMPTY && constr.inst == Type.NoType;
			 bs = bs.tail) {
			//System.out.println("hibound: " + bs.head);//DEBUG
			if (isSubSymOfAll(bs.head, constr.hibounds)) {
			    //System.out.println("best: " + bs.head);//DEBUG
			    constr.inst = bs.head.any2typevar();
			}
		    }
		}
		if (constr.inst == Type.NoType ||
		    !isSubTypeOfAll(constr.inst, constr.hibounds)) {
		    throw new NoInstance(
			"no unique maximal instance exists for type variable " +
			origin);
		}
	    }
	    return;
	default:
	    throw new ApplicationError();
	}
    }
    //where
        private boolean isSubSymOfAll(Type tp, Type.List tps) {
	    Symbol sym = tp.unalias().symbol();
	    for (Type.List l = tps; l != Type.List.EMPTY; l = l.tail) {
		if (!isSubSym(sym, l.head.unalias().symbol())) return false;
	    }
	    return true;
	}

	private boolean isSubSym(Symbol sym, Symbol sym1) {
	    return
		sym == sym1 ||
		sym.kind == ERROR ||
		(sym.kind == TYPE || sym.kind == CLASS) && sym.isSubClass(sym1);
	}

	private boolean isSubTypeOfAll(Type tp, Type.List tps) {
	    for (Type.List l = tps; l != Type.List.EMPTY; l = l.tail) {
		if (!tp.isSubType(l.head)) return false;
	    }
	    return true;
	}

    private void minimizeVar(Type tp) {
	switch (tp) {
	case TypeVar(Type origin, Type.Constraint constr):
	    if (constr.inst == Type.NoType && constr.lobounds != Type.List.EMPTY)
		constr.inst = Type.lub(constr.lobounds.toArray());
	    return;
	default:
	    throw new ApplicationError();
	}
    }

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
		Symbol[] newparams = new Symbol[tparams.length];
		for (int i = 0; i < tparams.length; i++)
		    newparams[i] = tparams[i].cloneSymbol();
		for (int i = 0; i < tparams.length; i++)
		    newparams[i].setInfo(
			newparams[i].info().subst(tparams, newparams));
		return Type.PolyType(
		    newparams, apply(restp).subst(tparams, newparams));
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
    private Type normalize(Type tp, Type pt) {
	switch (tp) {
	case MethodType(Symbol[] params, Type restype):
	    return global.definitions.functionType(
		Symbol.type(params), normalize(restype, Type.AnyType));
	case PolyType(Symbol[] tparams, Type restype):
	    if (tparams.length == 0) return normalize(restype, pt);
	}
	return tp;
    }

    boolean isCompatible(Type tp, Type pt) {
	return normalize(tp, pt).isSubType(pt);
    }

    private Symbol[] normalizeArgs(Type[] targs, Symbol[] tparams) {
	Type.List uninstantiated = Type.List.EMPTY;
	for (int i = 0; i < targs.length; i++) {
	    if (targs[i] == Type.NoType) {
		targs[i] = tparams[i].type();
		uninstantiated = Type.List.append(uninstantiated, targs[i]);
	    }
	}
	return Type.symbol(uninstantiated.toArray());
    }

    /** Return inferred type arguments of polymorphic expression, given
     *  its type parameters and result type and a prototype `pt'.
     *  If no maximal type variables exists that make the
     *  instantiated type a subtype of `pt' and `lastTry' is true, return `null'.
     */
    private Type[] instTypeArgs(Symbol[] tparams, Type restype, Type pt) {
	Type[] tvars = freshVars(tparams);
	// add all bounds except F-bounds to upper bounds of type variable.
	for (int i = 0; i < tvars.length; i++) {
	    switch (tvars[i]) {
	    case TypeVar(_, Type.Constraint constr):
		Type bound = tparams[i].info();
		if (!bound.containsSome(tparams))
		    constr.hibounds = new Type.List(bound, Type.List.EMPTY);
	    }
	}
	Type insttype = restype.subst(tparams, tvars);
	if (isCompatible(insttype, pt)) {
	    try {
		Type[] targs = new Type[tvars.length];
		for (int i = 0; i < tvars.length; i++) {
		    maximizeVar(tvars[i]);
		    targs[i] = instantiate(tvars[i]);
		}
		return targs;
	    } catch (NoInstance ex) {
	    }
	}
	return null;
    }

    /** As before, but: don't maximize. Instead map all unistantiated
     *  type vars to AnyType.
     */
    public Type[] protoTypeArgs(Symbol[] tparams, Type restype, Type pt,
				Symbol[] params) {
	Type[] tvars = freshVars(tparams);
	Type insttype = restype.subst(tparams, tvars);
	Type[] targs = new Type[tvars.length];
	if (isCompatible(insttype, pt)) {
	    for (int i = 0; i < tvars.length; i++) {
		targs[i] = instantiateUpper(tvars[i], isCovariant(tparams[i], params));
	    }
	} else {
	    for (int i = 0; i < tvars.length; i++) {
		targs[i] = Type.AnyType;
	    }
	}
	return targs;
    }

    /** Does given `tparam' occur only covariantly in symbols?
     */
    private boolean isCovariant(Symbol tparam, Symbol[] syms) {
	for (int i = 0; i < syms.length; i++) {
	    if (!isCovariant(tparam, syms[i])) return false;
	}
	return true;
    }

    /** Does given `tparam' occur only covariantly in symbol?
     */
    private boolean isCovariant(Symbol tparam, Symbol sym) {
	switch (sym.kind) {
	case ERROR: case VAL: case TYPE: return isCovariant(tparam, sym.info());
	case ALIAS: return !sym.info().contains(tparam);
	default: return false;
	}
    }

    /** Does given `tparam' occur only covariantly in types?
     */
    private boolean isCovariant(Symbol tparam, Type[] tps) {
	for (int i = 0; i < tps.length; i++) {
	    if (!isCovariant(tparam, tps[i])) return false;
	}
	return true;
    }

    /** Does given `tparam' occur only covariantly in argument types?
     */
    private boolean isCovariantArgs(Symbol tparam, Type[] tps) {
	for (int i = 0; i < tps.length; i++) {
	    switch (tps[i]) {
	    case CovarType(Type t):
		if (!isCovariant(tparam, t)) return false;
		break;
	    default:
		if (tps[i].contains(tparam)) return false;
	    }
	}
	return true;
    }

    /** Does given `tparam' occur only covariantly in type?
     */
    private boolean isCovariant(Symbol tparam, Type tp) {
	switch (tp) {
	case ErrorType:
	case AnyType:
	case NoType:
	case ThisType(Symbol sym):
	    return true;
	case TypeRef(Type pre, Symbol sym, Type[] args):
	    return isCovariant(tparam, pre) && isCovariantArgs(tparam, args);
	case SingleType(Type pre, Symbol sym):
	    return !pre.contains(tparam);
	case CompoundType(Type[] parts, Scope members):
	    return isCovariant(tparam, parts) &&
		isCovariant(tparam, members.elements());
	default:
	    throw new ApplicationError();
	}
    }

    /** The formal parameter types corresponding to `params'.
     *  If `params' is a repeated parameter, a list of `length' copies
     *  of its type is returned.
     */
    Type[] formalTypes(Symbol[] params, int length) {
	Type[] result;
	if (params.length == 1 && (params[0].flags & REPEATED) != 0) {
	    Type[] formals = new Type[length];
	    Type ft = params[0].type().typeArgs()[0];
	    for (int i = 0; i < length; i++) formals[i] = ft;
	    return formals;
	} else {
	    return Symbol.type(params);
	}
    }

    /** Return inferred type arguments, given type parameters, formal parameters and
     *  argument types.
     *  If this is not possible, throw a `NoInstance' exception, or, if
     *  `needToSucceed' is false alternatively return `null'.
     *  Undetermined type arguments are represented by `NoType'.
     *  No check that inferred parameters conform to their bounds is made here.
     */
    private Type[] methTypeArgs(Symbol[] tparams, Symbol[] params, Type[] argtypes,
				boolean needToSucceed) throws NoInstance {
	//System.out.println("methTypeArgs, tparams = " + ArrayApply.toString(tparams) + ", params = " + ArrayApply.toString(params) + ", type(params) = " + ArrayApply.toString(Symbol.type(params)) + ", argtypes = " + ArrayApply.toString(argtypes));//DEBUG

	Type[] tvars = freshVars(tparams);
	Type[] formals = formalTypes(params, argtypes.length);
	if (formals.length != argtypes.length) {
	    if (needToSucceed)
		throw new NoInstance("parameter lists differ in length");
	    return null;
	}
	for (int i = 0; i < argtypes.length; i++) {
	    if (!isCompatible(argtypes[i].subst(tparams, tvars),
			      formals[i].subst(tparams, tvars))) {
		if (needToSucceed)
		    throw new NoInstance(
			typeErrorMsg(
			    "argument expression's type is not compatible with formal parameter type",
			    argtypes[i].subst(tparams, tvars),
			    formals[i].subst(tparams, tvars)));
		return null;
	    }
	}
	Type[] targs = new Type[tvars.length];
	for (int i = 0; i < tvars.length; i++) {
	    minimizeVar(tvars[i]);
	    targs[i] = (((Type.TypeVar) tvars[i]).constr.inst == Type.NoType)
		? Type.NoType
		: instantiate(tvars[i]);
	}
	return targs;
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
     *  with type `[tparams]restype', given a two prototypes `pt1', and `pt2'.
     *  `pt1' is the strict first attempt prototype where type parameters
     *  are left unchanged. `pt2' is the fall-back prototype where type parameters
     *  are replaced by `AnyType's. We try to instantiate first to `pt1' and then,
     *  if this fails, to `pt2'. If both atempts fail, a `Type.Error' is thrown.
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
		Type[] targs = instTypeArgs(tparams, restype, pt1);
		if (targs == null)
		    targs = instTypeArgs(tparams, restype, pt2);
		if (targs == null)
		    throw new Type.Error(
			typeErrorMsg(
			    "polymorphic argument cannot be instantiated to formal parameter type",
			    Type.PolyType(tparams, restype), pt2));
		checkBounds(tparams, targs, "inferred ");
		return restype.subst(tparams, targs);
	    } else {
		return normalize(restype, pt2);
	    }
	}
    }

    /** Instantiate expression `tree' of polymorphic type with given `tparams' and
     *  `restype', using prototype `pt'.
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
	Type[] targs = instTypeArgs(tparams, restype, pt);
	if (targs == null)
	    throw new Type.Error(
		"polymorphic expression of type " + tree.type +
		" cannot be instantiated from expected type " + pt);
	checkBounds(tparams, targs, "inferred ");
	return mkTypeApply(tree, tparams, restype, targs);
    }

    /** Instantiate method `tree' of polymorphic type with given `tparams' and
     *  `restype', so that resulting method type can be applied to
     *  arguments with types `argtypes'.
     */
    public Tree methodInstance(Tree tree,
			       Symbol[] tparams, Type restype, Type[] argtypes)
	                       throws Type.Error {
	switch (restype) {
	case PolyType(Symbol[] tparams1, Type restype1):
	    Symbol[] tparams2 = new Symbol[tparams.length + tparams1.length];
	    System.arraycopy(tparams, 0, tparams2, 0, tparams.length);
	    System.arraycopy(tparams1, 0, tparams2, tparams.length, tparams1.length);
	    return methodInstance(tree, tparams2, restype1, argtypes);
	case MethodType(Symbol[] params, Type restpe):
	    Type[] argtypes1 = Type.widen(argtypes);
	    Type[] targs;
	    try {
		targs = methTypeArgs(tparams, params, argtypes1, true);
	    } catch (NoInstance ex) {
		throw new Type.Error(
		    applyErrorMsg(
			"no type parameters for ", tree,
			" exist so that it can be applied to arguments ",
			argtypes1, Type.AnyType) +
		    "\n --- because ---\n" + ex.getMessage());
	    }
	    Symbol[] uninstantiated = normalizeArgs(targs, tparams);
	    checkBounds(tparams, targs, "inferred ");
	    Type restype1 = (uninstantiated.length == 0) ? restype
		: Type.MethodType(params,
				  Type.PolyType(uninstantiated, restpe));
	    return mkTypeApply(tree, tparams, restype1, targs);
	default:
	    return tree;
	}
    }

    /** Instantiate constructor `tree' of polymorphic type with given `tparams' and
     *  `restype', so that its result type matches prototype `pt'.
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
	    Type[] targs = new Type[tparams.length];
	    for (int i = 0; i < tvars.length; i++) {
		try {
		    targs[i] = instantiateUpper(tvars[i], true);
		} catch (NoInstance ex) {
		    throw new Type.Error(
			"constructor of type " + ctpe1 +
			" can be instantiated in mode than one way to expected type " +
			pt +
			"\n --- because ---\n" + ex.getMessage());
		}
	    }
	    checkBounds(tparams, targs, "inferred ");
	    tree.setType(restype.subst(tparams, targs));
	    //System.out.println("inferred constructor type: " + tree.type);//DEBUG
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
	    Type[] formals = formalTypes(params, argtypes.length);
	    return
		isCompatible(restpe, pt) &&
		formals.length == argtypes.length &&
		Type.isSubType(argtypes, formals);
	case PolyType(Symbol[] tparams, MethodType(Symbol[] params, Type restpe)):
	    try {
		Type[] targs = methTypeArgs(
		    tparams, params, Type.widen(argtypes), false);
		if (targs != null) {
		    Symbol[] uninstantiated = normalizeArgs(targs, tparams);
		    return
			isWithinBounds(tparams, targs) &&
			instTypeArgs(uninstantiated, restpe.subst(tparams, targs), pt)
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
	if (alts.length == 1) {
	    tree.setSymbol(alts[0]).setType(alttypes[0]);
	    return;
	}
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
			alts[best], alttypes[best], alts[i], alttypes[i]));
		}
	    }
	    tree.setSymbol(alts[best]).setType(alttypes[best]);
	}
    }
    //where
	private boolean improves(Type tp1, Type tp2) {
	    return !isParameterized(tp1) && isParameterized(tp2);
	}

    /** Assign `tree' the type of an alternative
     *  which is applicable to `argtypes', and whose result type is
     *  a subtype of `pt' if it exists.
     *  If several applicable alternatives exist, take the
     *  most specialized one, or throw an error if no
     *  most specialized applicable alternative exists.
     *  If no alternative matches, leave `tree' unchanged.
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
		    throw new Type.Error(overloadResolveErrorMsg(
			alts[best], alttypes[best], alts[i], alttypes[i]));
		}
	    }
	    tree.setSymbol(alts[best]).setType(alttypes[best]);
	}
    }

    /** Assign `tree' the type of unique polymorphic alternative with `nparams' numbers
     *  of type parameters, if it exists.
     *  throw error if several polymorphic alternatives exist.
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
	       !(alts[i].isValue() && alttypes[i].typeParams().length == nparams)) {
	    i++;
	}
	if (i < alttypes.length) {
	    for (int j = i + 1; j < alttypes.length; j++) {
		if (alts[i].isValue() && alttypes[i].typeParams().length == nparams)
		    throw new Type.Error(overloadResolveErrorMsg(
			alts[i], alttypes[i], alts[j], alttypes[j]));
	    }
	    tree.setSymbol(alts[i]).setType(alttypes[i]);
	}
    }
}

