/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */
package scalac.typechecker;

import java.util.HashMap;
import scalac.*;
import scalac.util.*;
import scalac.ast.*;
import scalac.ast.printer.*;
import scalac.symtab.*;
import Tree.*;

/** Check that no forward reference to a term symbol extends beyond a value definition.
 */
public class RefCheck extends Transformer implements Modifiers, Kinds {

    public RefCheck(Global global) {
        super(global);
    }

    private Unit unit;
    private Scope[] scopes = new Scope[4];
    private int[] maxindex = new int[4];
    private int[] refpos = new int[4];
    private Symbol[] refsym = new Symbol[4];
    private int level;
    private HashMap symIndex = new HashMap();
    private Definitions definitions = global.definitions;

    void pushLevel() {
	level++;
	if (level == scopes.length) {
	    Scope[] scopes1 = new Scope[scopes.length * 2];
	    int[] maxindex1 = new int[scopes.length * 2];
	    int[] refpos1 = new int[scopes.length * 2];
	    Symbol[] refsym1 = new Symbol[scopes.length * 2];
	    System.arraycopy(scopes, 0, scopes1, 0, scopes.length);
	    System.arraycopy(maxindex, 0, maxindex1, 0, scopes.length);
	    System.arraycopy(refpos, 0, refpos1, 0, scopes.length);
	    System.arraycopy(refsym, 0, refsym1, 0, scopes.length);
	    scopes = scopes1;
	    maxindex = maxindex1;
	    refpos = refpos1;
	    refsym = refsym1;
	}
	scopes[level] = new Scope(scopes[level - 1]);
	maxindex[level] = Integer.MIN_VALUE;
    }

    void popLevel() {
	scopes[level] = null;
	level --;
    }

    public void apply(Unit unit) {
	this.unit = unit;
	level = 0;
	scopes[0] = new Scope();
	maxindex[0] = Integer.MIN_VALUE;
	unit.body = transformStats(unit.body);
	scopes[0] = null;
	symIndex.clear();
    }

    /** compensate for renaming during addition of access functions
     */
    Name normalize(Name name) {
	return (name.endsWith(Name.fromString("$")))
	    ? name.subName(0, name.length() - 1)
	    : name;
    }

    void enterSyms(Tree[] stats) {
	for (int i = 0; i < stats.length; i++) {
	    enterSym(stats[i], i);
	}
    }

    void enterSym(Tree stat, int index) {
	Symbol sym = null;
	switch (stat) {
	case ClassDef(_, _, _, _, _, _):
	    sym = stat.symbol().constructor();
	    break;
	case DefDef(_, _, _, _, _, _):
	case ModuleDef(_, _, _, _):
	case ValDef(_, _, _, _):
	    sym = stat.symbol();
	}
	if (sym != null && sym.isLocal()) {
	    scopes[level].enter(sym);
	    symIndex.put(sym, new Integer(index));
	}
    }

    public Tree[] transformStats(Tree[] stats) {
	pushLevel();
	enterSyms(stats);
	int i = 0;
	while (i < stats.length) {
	    Tree[] newstat = transformStat(stats[i], i);
	    if (newstat != null) {
		Tree[] newstats = new Tree[stats.length + newstat.length - 1];
		System.arraycopy(stats, 0, newstats, 0, i);
		System.arraycopy(newstat, 0, newstats, i, newstat.length);
		System.arraycopy(stats, i + 1, newstats, i + newstat.length,
				 stats.length - i - 1);
		i = i + newstat.length;
		stats = newstats;
	    } else {
		i = i + 1;
	    }
	}
	popLevel();
	return stats;
    }

    public Tree nullTree(int pos, Type tp) {
	return gen.TypeApply(
	    gen.Ident(pos, definitions.NULL),
	    new Tree[]{gen.mkType(pos, tp)});
    }

    private boolean isGlobalModule(Symbol sym) {
	return
	    sym.isModule() &&
	    (sym.owner().isPackage() || isGlobalModule(sym.owner().module()));
    }

    private Tree[] transformModule(Tree tree, int mods, Name name, Tree tpe, Tree.Template templ) {
	Symbol sym = tree.symbol();
	Tree cdef = make.ClassDef(
	    tree.pos,
	    mods | FINAL | MODUL,
	    name.toTypeName(),
	    Tree.ExtTypeDef.EMPTY_ARRAY,
	    Tree.ExtValDef.EMPTY_ARRAY_ARRAY,
	    Tree.Empty,
	    templ)
	    .setSymbol(sym.moduleClass()).setType(tree.type);
	Tree alloc = gen.New(
	    tree.pos,
	    sym.type().prefix(),
	    sym.moduleClass(),
	    Tree.EMPTY_ARRAY);
	if (isGlobalModule(sym)) {
	    Tree vdef = gen.ValDef(sym, alloc);
	    return new Tree[]{cdef, vdef};
	} else {
	    // var m$: T = null[T];
	    Name varname = Name.fromString(name + "$");
	    Symbol mvar = new TermSymbol(
		tree.pos, varname, sym.owner(), PRIVATE | MUTABLE | SYNTHETIC)
		.setInfo(sym.type());
	    Tree vdef = gen.ValDef(mvar, nullTree(tree.pos, sym.type()));

	    // { if (m$ == null[T]) m$ = new m$class; m$ }
	    Tree body = gen.Block(new Tree[]{
		gen.If(
		    gen.Apply(
			gen.Select(gen.mkRef(tree.pos, mvar), definitions.EQEQ),
			new Tree[]{nullTree(tree.pos, sym.type())}),
		    gen.Assign(gen.mkRef(tree.pos, mvar), alloc),
		    gen.Block(tree.pos, Tree.EMPTY_ARRAY)),
		gen.mkRef(tree.pos, mvar)});

	    // def m: T = { if (m$ == null[T]) m$ = new m$class; m$ }
	    sym.updateInfo(Type.PolyType(Symbol.EMPTY_ARRAY, sym.type()));
	    sym.flags |= STABLE;
	    Tree ddef = gen.DefDef(sym, body);
	    return new Tree[]{cdef, vdef, ddef};
	}
    }

    private boolean hasImplementation(Symbol clazz, Name name) {
	Symbol sym = clazz.info().lookupNonPrivate(name);
	return sym.kind == VAL &&
	    (sym.owner() == clazz ||
	     !definitions.JAVA_OBJECT_CLASS.isSubClass(sym.owner()) &&
	     (sym.flags & DEFERRED) == 0);
    }

    private Tree[] caseFields(ClassSymbol clazz) {
	Symbol[] vparams = clazz.constructor().type().firstParams();
	Tree[] fields = new Tree[vparams.length];
	for (int i = 0; i < fields.length; i++) {
	    fields[i] = gen.mkRef(
		clazz.pos, clazz.thisType(), clazz.caseFieldAccessor(i));
	}
	return fields;
    }

    private Tree toStringMethod(ClassSymbol clazz) {
	Symbol toStringSym = new TermSymbol(
	    clazz.pos, Names.toString, clazz, OVERRIDE)
	    .setInfo(definitions.TOSTRING.type());
	clazz.info().members().enter(toStringSym);
	Tree[] fields = caseFields(clazz);
	Tree body;
	if (fields.length == 0) {
	    body = gen.mkStringLit(
		clazz.pos, NameTransformer.decode(clazz.name).toString());
	} else {
	    body = gen.mkStringLit(
		clazz.pos, NameTransformer.decode(clazz.name).toString() + "(");
	    for (int i = 0; i < fields.length; i++) {
		body = gen.Apply(
		    gen.Select(body, definitions.STRING_PLUS_ANY),
		    new Tree[]{fields[i]});
		body = gen.Apply(
		    gen.Select(body, definitions.STRING_PLUS_ANY),
		    new Tree[]{gen.mkStringLit(clazz.pos, (i == fields.length - 1) ? ")" : ",")});
	    }
	}
	return gen.DefDef(clazz.pos, toStringSym, body);
    }

    private Tree equalsMethod(ClassSymbol clazz) {
	Symbol equalsSym = new TermSymbol(clazz.pos, Names.EQEQ, clazz, OVERRIDE);
	Symbol equalsParam =
	    new TermSymbol(clazz.pos, Names.that, equalsSym, PARAM)
	    .setInfo(definitions.ANY_TYPE);
	equalsSym.setInfo(
	    Type.MethodType(new Symbol[]{equalsParam}, definitions.BOOLEAN_TYPE));
	clazz.info().members().enter(equalsSym);
	Tree[] fields = caseFields(clazz);
	Tree[] patargs = patternVars(clazz, equalsSym);
	Type constrtype = clazz.constructor().type();
	switch (constrtype) {
	case PolyType(Symbol[] tparams, Type restp):
	    Type[] targs = new Type[tparams.length];
	    for (int i = 0; i < targs.length; i++)
		targs[i] = definitions.ANY_TYPE;
	    constrtype = restp.subst(tparams, targs);
	}
	Tree pattern = make.Apply(
	    clazz.pos, gen.mkType(clazz.pos, constrtype), patargs)
	    .setType(clazz.type());
	Tree rhs;
	if (fields.length == 0) {
	    rhs = gen.mkBooleanLit(clazz.pos, true);
	} else {
	    rhs = eqOp(fields[0], patargs[0]);
	    for (int i = 1; i < fields.length; i++) {
		rhs = gen.Apply(
		    gen.Select(rhs, definitions.AMPAMP()),
		    new Tree[]{eqOp(fields[i], patargs[i])});
	    }
	}
	CaseDef case1 = (Tree.CaseDef) make.CaseDef(
	    clazz.pos, pattern, Tree.Empty, rhs)
	    .setType(definitions.BOOLEAN_TYPE);
	CaseDef case2 = (Tree.CaseDef) make.CaseDef(clazz.pos,
	    patternVar(clazz.pos, Names.WILDCARD, equalsSym),
            Tree.Empty,
	    gen.mkBooleanLit(clazz.pos, false))
	    .setType(definitions.BOOLEAN_TYPE);
	Tree body = make.Apply(clazz.pos,
	    gen.Select(
		gen.mkRef(clazz.pos, Type.localThisType, equalsParam),
		definitions.MATCH),
	    new Tree[]{make.Visitor(clazz.pos, new CaseDef[]{case1, case2})
		       .setType(definitions.BOOLEAN_TYPE)})
	    .setType(definitions.BOOLEAN_TYPE);
	return gen.DefDef(clazz.pos, equalsSym, body);
    }
    //where
	private Tree patternVar(int pos, Name name, Symbol owner) {
	    return make.Ident(pos, name)
		.setSymbol(new TermSymbol(pos, name, owner, 0)
			   .setType(definitions.ANY_TYPE))
		.setType(definitions.ANY_TYPE);
	}

	private Tree[] patternVars(ClassSymbol clazz, Symbol owner) {
	    Symbol[] vparams = clazz.constructor().type().firstParams();
	    Tree[] pats = new Tree[vparams.length];
	    for (int i = 0; i < pats.length; i++) {
		pats[i] = patternVar(clazz.pos, vparams[i].name, owner);
	    }
	    return pats;
	}

	private Tree eqOp(Tree l, Tree r) {
	    return gen.Apply(gen.Select(l, definitions.EQEQ), new Tree[]{r});
	}

    private Tree hashCodeMethod(ClassSymbol clazz) {
	Symbol hashCodeSym = new TermSymbol(
	    clazz.pos, Names.hashCode, clazz, OVERRIDE)
	    .setInfo(definitions.HASHCODE.type());
	clazz.info().members().enter(hashCodeSym);
	Tree[] fields = caseFields(clazz);
	Symbol getClassSym = getMember(clazz.type(), Names.getClass);
	Symbol addSym = intMethod(Names.ADD);
	Symbol mulSym = intMethod(Names.MUL);
	Tree body =
	    gen.Apply(
		gen.Select(
		    gen.Apply(
			gen.mkRef(clazz.pos, clazz.thisType(), getClassSym),
			Tree.EMPTY_ARRAY),
		    getMember(getClassSym.type().resultType(), Names.hashCode)),
		Tree.EMPTY_ARRAY);
	for (int i = 0; i < fields.length; i++) {
	    Tree operand = gen.Apply(
		gen.Select(
		    fields[i],
		    getMember(fields[i].type, Names.hashCode)),
		Tree.EMPTY_ARRAY);
	    body =
		gen.Apply(
		    gen.Select(
			gen.Apply(
			    gen.Select(body, mulSym),
			    new Tree[]{gen.mkIntLit(clazz.pos, 41)}),
			addSym),
		    new Tree[]{operand});
	}
	return gen.DefDef(clazz.pos, hashCodeSym, body);
    }
    // where
	private Symbol getMember(Type site, Name name) {
	    Symbol sym = site.lookupNonPrivate(name);
	    assert sym.kind == VAL;
	    return sym;
	}

	private Symbol intMethod(Name name) {
	    Symbol sym = getMember(definitions.INT_TYPE, name);
	    switch (sym.type()) {
	    case OverloadedType(Symbol[] alts, Type[] alttypes):
		for (int i = 0; i < alts.length; i++) {
		    if (hasIntParam(alttypes[i])) return alts[i];
		}
	    }
	    assert hasIntParam(sym.type()) : "no int method among " + sym.type();
	    return sym;
	}

	private boolean hasIntParam(Type tp) {
	    Symbol[] params = tp.firstParams();
	    return params.length == 1 &&
		params[0].type().isSameAs(definitions.INT_TYPE);
	}

    private Template addCaseMethods(Template templ, Symbol sym) {
	if (sym.kind == CLASS && (sym.flags & CASE) != 0) {
	    Tree[] body1 = addCaseMethods(templ.body, (ClassSymbol) sym);
	    return copy.Template(templ, templ.parents, body1);
	}
	return templ;
    }

    private Tree[] addCaseMethods(Tree[] stats, ClassSymbol clazz) {
	TreeList ts = new TreeList();
	if (!hasImplementation(clazz, Names.toString))
	    ts.append(toStringMethod(clazz));
	if (!hasImplementation(clazz, Names.EQEQ))
	    ts.append(equalsMethod(clazz));
	if (!hasImplementation(clazz, Names.hashCode))
	    ts.append(hashCodeMethod(clazz));
	if (ts.length() > 0) {
	    Tree[] stats1 = new Tree[stats.length + ts.length()];
	    System.arraycopy(stats, 0, stats1, 0, stats.length);
	    ts.copyTo(stats1, stats.length);
	    return stats1;
	} else {
	    return stats;
	}
    }

    public Tree convertCaseFactoryCall(Tree tree) {
	Symbol fsym = TreeInfo.methSymbol(tree);
	if (fsym != null && fsym.isMethod() && !fsym.isConstructor() &&
	    (fsym.flags & CASE) != 0) {
	    // convert case methods to new's
	    Symbol constr = fsym.owner().info()
		.lookup(fsym.name.toTypeName()).constructor();
	    return gen.New(toConstructor(tree, constr));
	} else {
	    return tree;
	}
    }
    //where
	/** Tree represents an application of a constructor method of a case class
	 *  (whose name is a term name). Convert this tree to application of
	 *  the case classe's primary constructor `constr'.
	 */
	private Tree toConstructor(Tree tree, Symbol constr) {
	    switch (tree) {
	    case Apply(Tree fn, Tree[] args):
		return copy.Apply(tree, toConstructor1(fn, constr), args);
	    default:
		return gen.Apply(
		    tree.pos, toConstructor1(tree, constr), Tree.EMPTY_ARRAY);
	    }
	}

	private Tree toConstructor1(Tree tree, Symbol constr) {
	    switch (tree) {
	    case TypeApply(Tree fn, Tree[] args):
		return toMethodType(
		    copy.TypeApply(tree, toConstructor1(fn, constr), args));
	    case Ident(Name name):
		return toMethodType(
		    copy.Ident(tree, constr.name).setSymbol(constr));
	    case Select(Tree qual, Name name):
		return toMethodType(
		    copy.Select(tree, qual, constr.name).setSymbol(constr));
	    default:
		throw new ApplicationError();
	    }
	}

	private Tree toMethodType(Tree tree) {
	    Type tp = toMethodType(tree.type);
	    if (tp == tree.type) return tree;
	    else return tree.duplicate().setType(tp);
	}

	private Type toMethodType(Type tp) {
	    switch (tp) {
	    case MethodType(_, _):
		return tp;
	    case PolyType(Symbol[] tparams, Type restp):
		Type restp1 = toMethodType(restp);
		if (restp == restp) return tp;
		else return Type.PolyType(tparams, restp1);
	    default:
		return Type.MethodType(Symbol.EMPTY_ARRAY, tp);
	    }
	}

    /** The main checking functions
     */
    public Tree[] transformStat(Tree tree, int index) {
	Tree resultTree;
	switch (tree) {
	case ModuleDef(int mods, Name name, Tree tpe, Tree.Template templ):
	    return transform(transformModule(tree, mods, name, tpe, templ));

	case ValDef(int mods, Name name, Tree tpe, Tree rhs):
	    Symbol sym = tree.symbol();
	    resultTree = transform(tree);
	    //todo: handle variables
	    if (sym.isLocal() && !sym.isModule() && index <= maxindex[level]) {
		if (Global.instance.debug)
		    System.out.println(refsym[level] + ":" + refsym[level].type());
		unit.error(
		    refpos[level],
		    "forward reference extends over definition of value " +
		    normalize(name));
	    }
	    break;

	default:
	    resultTree = transform(tree);
	}
	return (resultTree == tree) ? null : new Tree[]{resultTree};
    }

    public Tree transform(Tree tree) {
	Tree tree1;
	switch (tree) {
	case ClassDef(int mods, Name name, Tree.TypeDef[] tparams, Tree.ValDef[][] vparams, Tree tpe, Tree.Template templ):
	    return super.transform(
		copy.ClassDef(tree, mods, name, tparams, vparams, tpe, addCaseMethods(templ, tree.symbol())));

	case Template(Tree[] bases, Tree[] body):
	    Tree[] bases1 = transform(bases);
	    Tree[] body1 = transformStats(body);
	    return copy.Template(tree, bases1, body1);
	case Block(Tree[] stats):
	    Tree[] stats1 = transformStats(stats);
	    return copy.Block(tree, stats1);
	case This(_):
	    return tree;
	case PackageDef(Tree pkg, Template packaged):
	    return copy.PackageDef(tree, pkg, super.transform(packaged));
	case Ident(Name name):
	    Scope.Entry e = scopes[level].lookupEntry(name);
	    Symbol sym = tree.symbol();
	    assert sym != null : name;
	    if (sym.isLocal() && sym == e.sym) {
		int i = level;
		while (scopes[i] != e.owner) i--;
		int symindex = ((Integer) symIndex.get(tree.symbol())).intValue();
		if (maxindex[i] < symindex) {
		    refpos[i] = tree.pos;
		    refsym[i] = e.sym;
		    maxindex[i] = symindex;
		}
	    }
	    tree1 = convertCaseFactoryCall(tree);
	    break;
	default:
	    tree1 = super.transform(convertCaseFactoryCall(tree));
	}
	if (tree1.isType() && !tree1.isMissing())
	    tree1 = gen.mkType(tree1.pos, tree1.type);
	return tree1;
    }
}

