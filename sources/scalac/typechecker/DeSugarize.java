/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */
package scalac.typechecker;

import java.io.*;
import java.util.*;
import scalac.*;
import scalac.util.*;
import scalac.symtab.*;
import scalac.ast.*;
import scalac.ast.printer.*;
import Tree.*;

/** A transformer for removing syntactic sugar. This transformer does
 *  not need any type or symbol-table information.
 *
 *  @author     Christine Roeckl, Martin Odersky
 *  @version    2.0
 */
public class DeSugarize implements Kinds, Modifiers {

    /** the global environment
     */
    protected Global global;

    /** the tree factory
     */
    protected TreeFactory make;

    /** The copying factory
     */
    protected TreeCopyFactory copy;

    /** the tree generator
     */
    protected TreeGen gen;

    /** the type inferencer
     */
    protected Infer infer;

    /** the name creator
     */
    protected final FreshNameCreator freshNameCreator;

    /** the constructor
     */
    public DeSugarize(Analyzer analyzer, Global global) {
	this.global = global;
	this.make = analyzer.make;
	this.copy = analyzer.copy;
	this.gen = analyzer.gen;
	this.infer = analyzer.infer;
        this.freshNameCreator = global.freshNameCreator;
    }

// Auxiliary definitions and functions -------------------------------------------

    /** introduce fresh variable of the form "deS$56"
     */
    Name getvar() {
        return freshNameCreator.newName("ds", '$');
    }

    Name setterName(Name name) {
	return name.append(Names._EQ);
    }

    Name parameterName(int i) {
	return Name.fromString("x$" + i);
    }

    Name tupleSelectorName(int i) {
	return Name.fromString("_" + i);
    }

    /** extract variables from a pattern
     */
    void getVariables(Tree tree, ArrayList vars) {
	switch(tree) {
	case Ident(Name name):
	    if (name.isVariable()) vars.add(name);
	    break;
	case Typed(Tree expr, Tree type):
	    getVariables(expr, vars);
	    break;
	case Apply(Tree fn, Tree[] args):
	    switch (fn) {
	    case Apply(_, _): getVariables(fn, vars);
	    }
	    for (int i = 0; i < args.length; i++)
		getVariables(args[i], vars);
	    break;
	default:
	    throw new ApplicationError ("illegal pattern", tree);
	}
    }

// Transform functions -----------------------------------------------------

    /**  (T_1, ..., T_N) => T  ==>  scala.FunctionN[T_1, ..., T_N, +T]
     */
    public Tree FunType(Tree tree) {
	switch(tree) {
	case FunType(Tree[] argtpes, Tree restpe):
	    Tree[] types = new Tree[argtpes.length + 1];
	    System.arraycopy(argtpes, 0, types, 0, argtpes.length);
	    types[argtpes.length] = make.CovariantType(restpe.pos, restpe);
	    return make.AppliedType(tree.pos,
		make.Select(tree.pos,
		    make.Ident(tree.pos, Names.scala),
		    Name.fromString("Function" + argtpes.length).toTypeName()),
		types);
	default:
	    throw new ApplicationError("function type expected", tree);
	}
    }

    public Tree mkTuple(int pos, Tree[] trees) {
	Name n = trees.length == 0 ? Names.Unit
	    : Name.fromString("Tuple" + trees.length);
	Tree select = make.Select(pos,
	    make.Ident(pos, Names.scala), n.toConstrName());
	return make.Apply(pos, select, trees);
    }

    /** Convert method to function type.
     */
    Type meth2fun(Type tp) {
	switch (tp) {
	case MethodType(Symbol[] params, Type restype):
	    return global.definitions.functionType(
		Symbol.type(params), meth2fun(restype));
	default:
	    return tp;
	}
    }

    /** If `pt' is a matching function type insert missing parameters
     *  in `vparams' from it, and return result type,
     *  else return AnyType.
     */
    public Type preFunction(ValDef[] vparams, Type pt) {
	switch (pt) {
	case TypeRef(Type pre, Symbol psym, Type[] ptargs):
	    if (psym.fullName().startsWith(Names.scala_Function) &&
		ptargs.length == vparams.length + 1) {
		for (int i = 0; i < vparams.length; i++)
		    assignType(vparams[i], ptargs[i]);
		return ptargs[vparams.length].dropVariance();
	    }
	}
	return Type.AnyType;
    }
    //where
	void assignType(ValDef vparam, Type pt) {
	    if (vparam.tpe == Tree.Empty && infer.isFullyDefined(pt))
		vparam.tpe = gen.mkType(vparam.pos, pt);
	}

    /** (x_1: T_1, ..., x_n: T_N) => e  ==>
     *  new scala.Function[T_1, ..., T_N, T] with {
     *    def apply(x_1: T_1, ..., x_N: T_N): T = e
     *  }
     *  where T = `restpe'
     *  T_i = `argtpes[i]'
     *  T_i's might be missing in the original tree.
     */
    public Tree Function(Tree tree, Type restype) {
	assert !restype.isCovarType();
	switch (tree) {
	case Function(ValDef[] vparams, Tree body):
	    int length = vparams.length;
	    Tree restpe = gen.mkType(tree.pos, meth2fun(restype));

	    Tree[] argtpes = new Tree[length + 1];
	    for (int i = 0; i < length; i++)
		argtpes[i] = vparams[i].tpe;
	    argtpes[vparams.length] = restpe;
	    Tree constr = make.TypeApply(tree.pos,
		make.Select(tree.pos,
			    make.Ident(tree.pos, Names.scala),
			    Name.fromString("Function" + length).toConstrName()),
		argtpes);

	    Tree applyDef = make.DefDef(
		tree.pos, 0, Names.apply,
		Tree.ExtTypeDef.EMPTY_ARRAY, new ValDef[][]{vparams},
		restpe, body);

	    Tree result = make.New(tree.pos,
		make.Template(tree.pos, new Tree[]{constr}, new Tree[]{applyDef}));
	    print(tree, "mkfun", result);
	    return result;
	default:
	    throw new ApplicationError();
	}
    }

    /** e of type FunctionN[T_1,...,T_N, T] -->
     *            (e: FunctionN[T_1,...,T_n, +T])
     */
    Tree postFunction(Tree tree) {
	Type[] targs = tree.type.typeArgs();
	if (targs.length >= 1) {
	    Type[] targs1 = new Type[targs.length - 1];
	    System.arraycopy(targs, 0, targs1, 0, targs1.length);
	    Tree result = gen.Typed(
		tree,
		global.definitions.functionType(targs1, targs[targs1.length]));
	    print(tree, "postfun", result);
	    return result;
	} else {
	    print(tree, "postfun", tree);
	    return tree;
	}
    }

    /** Cases, Argtpe, Restpe ==>
     *     (new scala.PartialFunction[Argtpe, Restpe] {
     *        def apply(x: Argtpe): Restpe = x match {Cases}
     *        def isDefinedAt(x: Argtpe): scala.Boolean = x match {Cases'}
     *      }: scala.PartialFunction[Argtpe, +Restpe])
     *  WHERE
     *    case P1 if G1 => E1, ..., Pn if Gn => En) = Cases
     *    Cases' = case P1 if G1 => True, ..., Pn if Gn => True, _ => False
     *    Argtpe = targs[0]
     *    Restpe = targs[1]
     */
    public Tree partialFunction(Tree tree, Type pattpe, Type restpe) {
	Tree constr = make.TypeApply(tree.pos,
	    make.Select(tree.pos,
		make.Ident(tree.pos, Names.scala),
		Names.PartialFunction.toConstrName()),
		new Tree[]{gen.mkType(tree.pos, pattpe),
			   gen.mkType(tree.pos, restpe)});
	Name x = getvar();
	ValDef param = (ValDef) make.ValDef(
	    tree.pos, PARAM, x, gen.mkType(tree.pos, pattpe), Tree.Empty);
	ValDef[][] vparams = new ValDef[][]{new ValDef[]{param}};
	Tree body = make.Apply(tree.pos,
	    make.Select(tree.pos,
		make.Ident(tree.pos, x), Names.match), new Tree[]{tree});
	Tree applyDef = make.DefDef(
	    tree.pos, 0, Names.apply, Tree.ExtTypeDef.EMPTY_ARRAY, vparams,
	    gen.mkType(tree.pos, restpe), body);
	Tree tree1 = isDefinedAtVisitor(tree);
	Tree body1 = make.Apply(tree.pos,
	    make.Select(tree.pos,
		make.Ident(tree.pos, x), Names.match), new Tree[]{tree1});
	Tree isDefinedAtDef = make.DefDef(
	    tree.pos, 0, Names.isDefinedAt, Tree.ExtTypeDef.EMPTY_ARRAY,
	    Tree.duplicator.transform(vparams),
	    gen.mkType(tree.pos, global.definitions.BOOLEAN_TYPE), body1);
	Tree result = make.New(tree.pos,
	    make.Template(
		tree.pos, new Tree[]{constr}, new Tree[]{applyDef, isDefinedAtDef}));

	//Tree result = make.Typed(tree.pos,
	//    newTree,
	//    gen.mkType(tree.pos,
        //        global.definitions.partialFunctionType(targs[0], targs[1])));
	print(tree, "partialfun", result);
	return result;
    }

    private Tree isDefinedAtVisitor(Tree tree) {
	switch (tree) {
	case Visitor(CaseDef[] cases):
	    CaseDef lastCase = cases[cases.length - 1];
	    switch (lastCase) {
	    case CaseDef(Ident(Name name), Tree.Empty, Tree expr):
		if (name.isVariable())
		    return make.Visitor(tree.pos,
			new CaseDef[]{
			    make.CaseDef(lastCase.pos,
				lastCase.pat.duplicate(),
				Tree.Empty,
				gen.mkBoolean(lastCase.body.pos, true))});
	    }
	    CaseDef[] cases1 = new CaseDef[cases.length + 1];
	    for (int i = 0; i < cases.length; i++) {
		switch (cases[i]) {
		case CaseDef(Tree pat, Tree guard, _):
		    cases1[i] = (CaseDef) make.CaseDef(
			cases[i].pos,
			pat.duplicate(),
			guard.duplicate(),
			gen.mkBoolean(tree.pos, true));
		}
	    }
	    cases1[cases.length] = (CaseDef) make.CaseDef(
		tree.pos,
		make.Ident(tree.pos, Names.WILDCARD),
		Tree.Empty,
		gen.mkBoolean(tree.pos, false));
	    return make.Visitor(tree.pos, cases1);
	default:
	    throw new ApplicationError("visitor expected", tree);
	}
    }

    /** match => this.match
     *  match[targs] => this.match[targs]
     *  tree is already attributed and attributes need to be preserved.
     */
    Tree postMatch(Tree tree, Symbol currentclazz) {
	switch (tree) {
	case Ident(Name name):
	    return
		make.Select(tree.pos,
		    make.This(tree.pos, Tree.Empty).setType(currentclazz.type()),
		    name).setSymbol(tree.symbol()).setType(tree.type);
	case TypeApply(Tree fn, Tree[] args):
	    return copy.TypeApply(tree, postMatch(fn, currentclazz), args);
	default:
	    return tree;
	}
    }

    /** { cases }   ==>   (x => x.match {cases})
     *  only called when match has to be added
     *  no type for parameter x
     */
    public Tree Visitor(Tree tree) {
        switch(tree) {
	case Visitor(CaseDef[] cases):
	    Name x = getvar();
	    ValDef param = (ValDef) make.ValDef(
		tree.pos, PARAM, x, Tree.Empty, Tree.Empty);
	    Tree xuse = make.Ident(tree.pos, x);
	    // x.match {cases}
	    Tree body = make.Apply(tree.pos,
		make.Select(tree.pos, xuse, Names.match),
		new Tree[]{tree});
	    return make.Function(tree.pos, new ValDef[]{param}, body);
	default:
	    throw new ApplicationError("visitor expected", tree);
	}
    }

    /** e = e'   ==>   e_=(e')
     */
    public Tree Assign(int pos, Tree lhs, Tree rhs) {
	Tree lhs1;
	switch (lhs) {
	case Ident(Name name):
	    lhs1 = make.Ident(lhs.pos, setterName(name));
	    break;
	case Select(Tree qual, Name name):
	    lhs1 = make.Select(lhs.pos, qual, setterName(name));
	    break;
	default:
	    throw new ApplicationError();
	}
	return make.Apply(pos, lhs1, new Tree[]{rhs});
    }

    /** e(args) = e'  ==>  e.update(args ; e')
     */
    public Tree Update(Tree tree) {
	switch(tree) {
	case Assign(Apply(Tree fn, Tree[] args), Tree rhs):
	    // e.update
	    Tree update = make.Select(fn.pos, fn, Names.update);
	    Tree[] args1 = new Tree[args.length + 1];
	    System.arraycopy(args, 0, args1, 0, args.length);
	    args1[args.length] = rhs;
	    return make.Apply(tree.pos, update, args1);
	default:
	    throw new ApplicationError();
	}
    }

    /** expand pattern definitions and variable definitions in templates.
     */
    public Tree[] Statements(Tree[] stats, boolean isLocal) {
	boolean change = false;
	for (int i = 0; i < stats.length && !change; i++) {
	    switch (stats[i]) {
	    case PatDef(_, _, _):
		change = true;
		break;
	    case ValDef(int mods, _, _, _):
		change = !isLocal && (mods & MUTABLE) != 0;
	    }
	}
	if (change) {
	    TreeList ts = new TreeList();
	    for (int i = 0; i < stats.length; i++) {
		switch (stats[i]) {
		case PatDef(_, _, _):
		    ts.append(this.PatDef(stats[i]));
		    break;
		case ValDef(int mods, _, _, _):
		    if (!isLocal && (mods & MUTABLE) != 0)
			ts.append(this.VarDef(stats[i]));
		    else
			ts.append(stats[i]);
		    break;
		default:
		    ts.append(stats[i]);
		}
	    }
	    stats = ts.toArray();
	    //TextTreePrinter p = new TextTreePrinter();//debug
	    //p.print("desugarized:");//debug
	    //for (int i = 0; i < stats.length; i++) p.print(stats[i]).println();//debug
	    //p.end();//debug
	    return stats;
	} else {
	    return stats;
	}
    }

    /** expands pattern definitions
     *  in case pattern is a simple (typed) identifier:
     *  val x = e     ==>  val x = e
     *  val x: T = e  ==>  val x: T = e
     *
     *  in case there are no variables in pattern
     *  val p = e  ==>  e.match (case p => ())
     *
     *  in case there is exactly one variable in pattern
     *  val x_1 = e.match (case p => (x_1))
     *
     *  in case there are more variables in pattern
     *  val p = e  ==>  private synthetic val t$ = e.match (case p => (x_1, ..., x_N))
     *                  val x_1 = t$._1
     *                  ...
     *                  val x_N = t$._N
     *
     */
    public Tree[] PatDef(Tree tree) {
	switch(tree) {

	case PatDef(int mods, Ident(Name name), Tree rhs):
	    // val x = e     ==>  val x = e
	    return new Tree[]{
		make.ValDef(tree.pos, mods, name, Tree.Empty, rhs)};

	case PatDef(int mods, Typed(Ident(Name name), Tree type), Tree rhs):
	    // val x: T = e  ==> val x: T = e
	    return new Tree[]{
		make.ValDef(tree.pos, mods, name, type, rhs)};

	case PatDef(int mods, Tree pat, Tree rhs):
	    int pos = tree.pos;
	    ArrayList varlist = new ArrayList();
	    getVariables(pat, varlist);
	    Name[] vars = new Name[varlist.size()];
	    varlist.toArray(vars);

	    // Tuple_N(x_1, ..., x_N)
	    Tree[] vtree = new Tree[vars.length];
	    for (int i = 0; i < vars.length; i++) {
		vtree[i] = make.Ident(pos, vars[i]);
	    }
	    Tree tuple = mkTuple(tree.pos, vtree);

	    // e.match (case p => Tuple_N(x_1, ..., x_N))
	    CaseDef[] cases = {make.CaseDef(pos, pat, Tree.Empty, tuple)};
	    Tree match = make.Apply(pos,
				    make.Select(pos, rhs, Names.match),
				    new Tree[]{make.Visitor(pos, cases)});

	    if (vars.length == 0) {
		// e.match (case p => ())
		return new Tree[]{match};
	    } else if (vars.length == 1) {
		// val x_1 = e.match (case p => (x_1))
		return new Tree[]{
		    make.ValDef(pos, mods, vars[0], Tree.Empty, match)};
	    } else {
		// t$
		Name var = getvar();

		// private synthetic val t$ = e.match (case p => (x_1, ..., x_N))
		Tree[] res = new Tree[vars.length + 1];
		res[0] = make.ValDef(pos, PRIVATE | SYNTHETIC, var,
				     Tree.Empty, match);
		for (int i = 0; i < vars.length; i ++) {
		    // val x_i = t$._i
		    res[i + 1] = make.ValDef(
			pos, mods, vars[i], Tree.Empty,
			make.Select(pos, make.Ident(pos, var), tupleSelectorName(i + 1)));
		}
		print(pat, " -> ", new Block(res));//debug
		return res;
	    }
	default:
	    throw new ApplicationError("pattern definition expected", tree);
	}
    }

    public Tree[] VarDef(Tree tree) {
	switch (tree) {
	case ValDef(int mods, Name name, Tree tpe, Tree rhs):
	    Name varname = Name.fromString(name + "$");
	    Tree vardef1 = copy.ValDef(
		tree, PRIVATE | MUTABLE | SYNTHETIC, varname, tpe, rhs);
	    Tree getter = make.DefDef(
		tree.pos, mods | ACCESSOR, name,
		Tree.ExtTypeDef.EMPTY_ARRAY,
		Tree.ExtValDef.EMPTY_ARRAY_ARRAY,
		tpe,
		((mods & DEFERRED) != 0) ? Tree.Empty
		    : make.Ident(tree.pos, varname));
	    Tree setter = make.DefDef(
		tree.pos, mods | ACCESSOR, setterName(name),
		Tree.ExtTypeDef.EMPTY_ARRAY,
		new ValDef[][]{{
		    (ValDef) make.ValDef(
			tree.pos, SYNTHETIC, parameterName(0), tpe, Tree.Empty)}},
		gen.mkType(tree.pos, global.definitions.UNIT_TYPE),
		((mods & DEFERRED) != 0) ? Tree.Empty
		    : make.Assign(
			tree.pos,
			make.Ident(tree.pos, varname),
			make.Ident(tree.pos, parameterName(0))));
	    if ((mods & DEFERRED) != 0) return new Tree[]{getter, setter};
	    else return new Tree[]{vardef1, getter, setter};
	default:
	    throw new ApplicationError();
	}
    }

    /** Tree represents an application of a constructor method of a case class
     *  (whose name is a term name). Convert this tree to application of
     *  the case classe's primary constructor `constr'.
     */
    public Tree toConstructor(Tree tree, Symbol constr) {
	switch (tree) {
	case Apply(Tree fn, Tree[] args):
	    return copy.Apply(tree, toConstructor(fn, constr), args);
	case TypeApply(Tree fn, Tree[] args):
	    return copy.TypeApply(tree, toConstructor(fn, constr), args);
	case Ident(Name name):
	    return copy.Ident(tree, constr.name).setSymbol(constr);
	case Select(Tree qual, Name name):
	    return copy.Select(tree, qual, constr.name).setSymbol(constr);
	default:
	    throw new ApplicationError();
	}
    }

    /** Expand partial function applications of type `type'.
     *
     *  p.f(es_1)...(es_n)
     *     ==>  {
     *            private synthetic val eta$f    = p.f   // if p is not stable
     *            ...
     *            private synthetic val eta$e_i = e_i    // if e_i is not stable
     *             ...
     *
     *            (ps_1 => ... => ps_m => eta$f([es_1])...([es_m])(ps_1)...(ps_m))
     *          }
     *  tree is already attributed
     */
    public Tree etaExpand(Tree tree, Type type) {
	TreeList defs = new TreeList();
	Tree lambda =
	    toFunction(toApply(liftoutPrefix(tree, defs), type), type);
	defs.append(lambda);
	Tree result = make.Block(tree.pos, defs.toArray());
	print(tree, "eta", result);//debug
	return result;
    }

    private static String preName = "eta$";

    /** Append to `defs' value definitions for all non-stable subexpressions
     *  of the function application `tree'
     */
    public Tree liftoutPrefix(Tree tree, TreeList defs) {
	switch (tree) {
	case Ident(_):
	    return tree;

	case Select(Tree qual, Name name):
	    return copy.Select(tree, liftout(qual, defs), name);

	case TypeApply(Tree fn, Tree[] args):
	    return copy.TypeApply(tree, liftoutPrefix(fn, defs), args);

	case Apply(Tree fn, Tree[] args):
	    return copy.Apply(tree, liftoutPrefix(fn, defs), liftout(args, defs));

	default:
	    throw new ApplicationError();
	}
    }

    public Tree[] liftout(Tree[] trees, TreeList defs) {
	Tree[] trees1 = trees;
	for (int i = 0; i < trees.length; i++) {
	    Tree tree = trees[i];
	    Tree tree1 = liftout(tree, defs);
	    if (tree1 != tree && trees1 == trees) {
		trees1 = new Tree[trees.length];
		System.arraycopy(trees, 0, trees1, 0, trees.length);
	    }
	    trees1[i] = tree1;
	}
	return trees1;
    }

    public Tree liftout(Tree tree, TreeList defs) {
	if (!TreeInfo.isPureExpr(tree)) {
	    Name vname = Name.fromString(preName + defs.length());
	    defs.append(
		make.ValDef(
		    tree.pos, SYNTHETIC, vname, Tree.Empty, tree));
	    return make.Ident(tree.pos, vname);
	} else {
	    return tree;
	}
    }

    /** f, (syms_1)...(syms_n)T    ==>    f(ps_1)...(ps_n)
     */
    Tree toApply(Tree tree, Type type) {
	switch(type) {
	case MethodType(Symbol[] vparams, Type restpe):
	    Tree res = make.Apply(tree.pos, tree, toIdents(vparams));
	    return toApply(res, restpe);
	default:
	    return tree;
	}
    }

    /** e, (syms_1)...(syms_n)T    ==>    (ps_1 => ... => ps_n => e)
     */
    Tree toFunction(Tree tree, Type type) {
	switch(type) {
	case MethodType(Symbol[] vparams, Type restpe):
	    return this.Function(
		make.Function(tree.pos, toVparams(vparams), toFunction(tree, restpe)),
		restpe);
	default:
	    return tree;
	}
    }

    /** Extract value parameters from type.
     */
    ValDef[] toVparams(Symbol[] symbols) {
	ValDef[] vpars = new ValDef[symbols.length];
	for (int i = 0; i < symbols.length; i++) {
	    vpars[i] = (ValDef)make.ValDef(
		symbols[i].pos, 0, symbols[i].name,
		gen.mkType(symbols[i].pos, symbols[i].type()),
		Tree.Empty);
	}
	return vpars;
    }

    /** Extract value identifiers from method type.
     *  It is assumed that all symbols are term symbols  ==>  make.Ident().
     */
    Tree[] toIdents(Symbol[] symbols) {
	Tree[] idents = new Ident[symbols.length];
	for (int i = 0; i < symbols.length; i++) {
	    idents[i] = make.Ident(symbols[i].pos, symbols[i].name);
	}
	return idents;
    }

    /** Build value element definition name for case parameter.
     */
    void addCaseElement(TreeList ts, ValDef vparam) {
	//System.out.println("add case for " + vparam.name);//DEBUG
	ts.append(
	    make.ValDef(
		vparam.pos, CASE, vparam.name, vparam.tpe,
		make.Ident(vparam.pos, vparam.name)
		.setSymbol(vparam.symbol()).setType(vparam.symbol().type())));
    }

    /** add case constructor, value defintiions and access functions.
     */
    Tree[] addCaseElements(Tree[] body, ValDef[] vparams) {
	TreeList stats = new TreeList();
	for (int i = 0; i < vparams.length; i++) {
	    addCaseElement(stats, vparams[i]);
	}
	stats.append(body);
	return stats.toArray();
    }

    /** Does list of types inherit from class scala.Algebraic?
     */
    boolean inheritsAlgebraic(Type[] tps) {
	Type algebraic = global.definitions.getType(Names.scala_Algebraic);
	for (int i = 0; i < tps.length; i++) {
	    if (tps[i].isSubType(algebraic)) return true;
	}
	return false;
    }

    /** Add toString, hashCode and == if class inherts from scala.Algebraic.
     */
    Tree[] addCaseMethods(Tree[] body, Symbol clazz, Type[] parents) {
	if (inheritsAlgebraic(parents)) {
	    /* todo uncomment and implement
	    TreeList stats = new TreeList(body);
	    Symbol[] params = clazz.constructor().firstParams();
	    stats.append(toStringMethod(clazz, params));
	    stats.append(hashcodeMethod(clazz, params));
	    stats.append(equalsMethod(clazz, params));
	    */
	}
	return body;
    }

    //debug
    void print(Tree tree, String conv, Tree result) {
	if (global.log()) {
	    new TextTreePrinter()
		.print(tree).println()
		.print(" --" + conv + "--> ").println()
		.print(result).println().end();
	}
    }
}
