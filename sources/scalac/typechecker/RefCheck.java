/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.typechecker;

import java.util.HashMap;
import java.util.Iterator;
import scalac.*;
import scalac.util.*;
import scalac.ast.*;
import scalac.ast.printer.*;
import scalac.symtab.*;
import Tree.*;

/** Post-attribution checking and transformation.
 *
 *  This phase performs the following checks.
 *
 *   - All overrides conform to rules.
 *   - All type arguments conform to bounds.
 *   - All type variable uses conform to variance annotations.
 *   - No forward reference to a term symbol extends beyond a value definition.
 *
 *  It preforms the following transformations.
 *
 *   - Local modules are replaced by variables and classes
 *   - toString, equals, and hashCode methods are added to case classes, unless
 *     they are defined in the class or a baseclass different from java.lang.Object
 *   - Calls to case factory methods are replaced by new's.
 *   - Type nodes are replaced by TypeTerm nodes.
 */
public class RefCheck extends Transformer implements Modifiers, Kinds {

    public RefCheck(Global global) {
        super(global);
    }

    private Unit unit;
    private Definitions defs = global.definitions;
    private Infer infer = new Infer(this);
    private Symbol enclClass;

    public void apply(Unit unit) {
	this.unit = unit;
	level = 0;
	scopes[0] = new Scope();
	maxindex[0] = Integer.MIN_VALUE;
	unit.body = transformStats(unit.body);
	scopes[0] = null;
	symIndex.clear();
    }

// Override checking ------------------------------------------------------------

    /** 1. Check all members of class `clazz' for overriding conditions.
     *  2. Check that only abstract classes have deferred members*
     *  3. Check that every member with an `override' modifier
     *     overrides a concrete member.
     */
    void checkAllOverrides(int pos, Symbol clazz) {
	Type[] closure = clazz.closure();
	HashMap/*<Symbol,Symbol>*/ overrides = new HashMap();
	for (int i = 0; i < closure.length; i++) {
	    for (Scope.SymbolIterator it = closure[i].members().iterator(true);
		 it.hasNext();) {
		checkOverride(pos, clazz, it.next(), overrides);
	    }
	}

	Type[] parents = clazz.info().parents();
	for (Scope.SymbolIterator it = clazz.members().iterator(true);
	     it.hasNext();) {
	    Symbol sym = it.next();
	    if ((sym.flags & OVERRIDE) != 0) {
		int i = parents.length - 1;
		while (i >= 0 && sym.overriddenSymbol(parents[i]).kind == NONE)
		    i--;
		if (i < 0) {
		    unit.error(sym.pos, sym + " overrides nothing");
		    sym.flags &= ~OVERRIDE;
		}
	    }
	}

	for (Iterator/*<Symbol>*/ it = overrides.keySet().iterator();
	     it.hasNext();) {
	    Symbol member = (Symbol) it.next();
	    Symbol other = (Symbol) overrides.get(member);
	    if ((other.flags & DEFERRED) != 0) {
		abstractClassError(
		    clazz, member + member.locationString() +
		    " is marked `override' and overrides only abstract members" + other + other.locationString());
	    }
	}
    }

    void checkOverride(int pos, Symbol clazz, Symbol other,
		       HashMap/*<Symbol,Symbol>*/ overrides) {
	Symbol member = other;
	if ((other.flags & PRIVATE) == 0) {
	    Symbol member1 = clazz.info().lookup(other.name);
	    if (member1.kind != NONE && member1.owner() != other.owner()) {
		Type self = clazz.thisType();
		Type otherinfo = normalizedInfo(self, other);
		Type template = resultToAny(otherinfo);
		switch (member1.info()) {
		case OverloadedType(Symbol[] alts, _):
		    for (int i = 0; i < alts.length; i++) {
			if (normalizedInfo(self, alts[i]).isSubType(template)) {
			    if (member == other)
				member = alts[i];
			    else
				unit.error(
				    pos,
				    "ambiguous override: both " +
				    member + ":" + normalizedInfo(self, member) +
				    "\n and " + alts[i] + ":" + normalizedInfo(self, alts[i]) +
				    "\n override " + other + ":" + otherinfo +
				    other.locationString());
			}
		    }
		    break;
		default:
		    if (normalizedInfo(self, member1).isSubType(template)) {
			member = member1;
		    }
		}
	    }
	    if (member != other) {
		checkOverride(pos, clazz, member, other);
	    }
	}
	if (clazz.kind == CLASS && (clazz.flags & ABSTRACTCLASS) == 0) {
	    if ((member.flags & DEFERRED) != 0) {
		abstractClassError(
		    clazz,
		    member + member.locationString() + " is not defined" +
		    (((member.flags & MUTABLE) == 0) ? ""
		     : "\n(Note that variables need to be initialized to be defined)"));
	    } else if (member != other &&
		       (member.flags & OVERRIDE) != 0 &&
		       ((other.flags & DEFERRED) == 0 ||
			overrides.get(member) == null))
		overrides.put(member, other);
	}
    }
    //where
        private Type resultToAny(Type tp) {
	    switch (tp) {
	    case PolyType(Symbol[] tparams, Type restp):
		return Type.PolyType(tparams, resultToAny(restp));
	    case MethodType(Symbol[] tparams, Type restp):
		return Type.MethodType(tparams, Type.AnyType);
	    default:
		return defs.ANY_TYPE;
	    }
	}
	private void abstractClassError(Symbol clazz, String msg) {
	    if (clazz.isAnonymousClass())
		unit.error(clazz.pos, "object creation impossible, since " + msg);
	    else
		unit.error(clazz.pos, clazz + " needs to be abstract, since " + msg);
	    clazz.flags |= ABSTRACTCLASS;
	}


    /** Check that all conditions for overriding `other' by `member' are met.
     *  That is for overriding member M and overridden member O:
     *
     *    1. M must have the same or stronger access privileges as O.
     *    2. O must not be final.
     *    3. O is deferred, or M has `override' modifier.
     *    4. O is not a class, nor a class constructor.
     *    5. If O is a type alias, then M is an alias of O.
     *    6. If O is an abstract type then
     *         either M is an abstract type, and M's bounds are sharper than O's bounds.
     *         or M is a type alias or class which conforms to O's bounds.
     *    7. If O and M are values, then M's type is a subtype of O's type.
     *    8. If O is an immutable value, then so is M.
     */
    void checkOverride(int pos, Symbol clazz, Symbol member, Symbol other) {
	if (member.owner() == clazz)
	    pos = member.pos;
	else if (member.owner().isSubClass(other.owner()))
	    return; // everything was already checked elsewhere

	if ((member.flags & PRIVATE) != 0) {
	    overrideError(pos, member, other, "has weaker access privileges; it should not be private");
	} else if ((member.flags & PROTECTED) != 0 && (other.flags & PROTECTED) == 0) {
	    overrideError(pos, member, other, "has weaker access privileges; it should not be protected");
	} else if ((other.flags & FINAL) != 0) {
	    overrideError(pos, member, other, "cannot override final member");
	} else if ((other.flags & DEFERRED) == 0 && ((member.flags & OVERRIDE) == 0)) {
	    overrideError(pos, member, other, "needs `override' modifier");
	} else if (other.isStable() && !member.isStable()) {
	    overrideError(pos, member, other, "needs to be an immutable value");
	} else if ((member.flags & DEFERRED) == 0 && (other.flags & DEFERRED) == 0 &&
		   member.owner() != clazz &&
		   !clazz.parents()[0].symbol().isSubClass(other.owner())) {
	    unit.error(pos, "conflict between concrete members " +
		       member + member.locationString() + " and " +
		       other + other.locationString() +
		       ":\n both are inherited from mixin classes; " +
		       "\n an overriding definition in the current template is required");
	} else {
	    Type self = clazz.thisType();
	    switch (other.kind) {
	    case CLASS:
 		overrideError(pos, member, other, "cannot override a class");
		break;
	    case ALIAS:
		if (!self.memberType(member).isSameAs(self.memberType(other)))
		    overrideTypeError(pos, member, other, self, false);
		break;
	    default:
		if (other.isConstructor())
		    overrideError(pos, member, other,
				  "cannot override a class constructor");
		if (!normalizedInfo(self, member).isSubType(
			normalizedInfo(self, other)))
		    overrideTypeError(pos, member, other, self, false);
		if (member.kind == TYPE &&
		    !self.memberLoBound(other).isSubType(
			 self.memberLoBound(member)))
		    overrideTypeError(pos, member, other, self, true);

	    }
	}
    }

    void overrideError(int pos, Symbol member, Symbol other, String msg) {
	if (other.type() != Type.ErrorType && member.type() != Type.ErrorType)
	    unit.error(pos,
		"error overriding " + other + other.locationString() +
		";\n " + member + member.locationString() + " " + msg);
    }

    void overrideTypeError(int pos, Symbol member, Symbol other, Type site,
			   boolean lobound) {
	if (other.type() != Type.ErrorType && member.type() != Type.ErrorType) {
	    Type memberInfo = lobound ? site.memberLoBound(member)
		: normalizedInfo(site, member);
	    Type otherInfo = lobound ? site.memberLoBound(other)
		: normalizedInfo(site, other);
	    unit.error(pos,
		member + member.locationString() +
		infoString(member, memberInfo, lobound) +
		"\n cannot override " + other + other.locationString() +
		infoString(other, otherInfo, lobound));
	    Type.explainTypes(memberInfo, otherInfo);
	}
    }

    Type normalizedInfo(Type site, Symbol sym) {
	return site.memberInfo(sym).derefDef();
    }

    String infoString(Symbol sym, Type symtype, boolean lobound) {
	switch (sym.kind) {
	case ALIAS: return ", which equals " + symtype;
	case TYPE:  return " bounded" + (lobound ? " from below" : "") + " by " + symtype;
	case VAL:   return " of type " + symtype;
	default:    return "";
	}
    }

    /** compensate for renaming during addition of access functions
     */
    Name normalize(Name name) {
	return (name.endsWith(Name.fromString("$")))
	    ? name.subName(0, name.length() - 1)
	    : name;
    }

// Basetype Checking --------------------------------------------------------

    /** 1. Check that only traits are inherited several times (except if the
     *     inheriting instance is a compund type).
     *  2. Check that later type instances in the base-type sequence
     *     of a class are subtypes of earlier type instances of the same trait.
     *  3. Check that case classes do not inherit from case classes.
     *  4. Check that at most one base type is a case-class.
     */
    void validateBaseTypes(Symbol clazz) {
	validateBaseTypes(clazz, clazz.type().parents(),
			  new Type[clazz.closure().length], clazz, 0);
    }
    //where
        void validateBaseTypes(Symbol clazz, Type[] tps, Type[] seen,
			       Symbol caseSeen, int start) {
	    for (int i = tps.length - 1; i >= start; i--) {
		validateBaseTypes(
		    clazz, tps[i].unalias(), seen, caseSeen, i == 0 ? 0 : 1);
	    }
	}

	void validateBaseTypes(Symbol clazz, Type tp, Type[] seen,
			       Symbol caseSeen, int start) {
	    Symbol baseclazz = tp.symbol();
	    if (baseclazz.kind == CLASS) {
		int index = clazz.closurePos(baseclazz);
                if (index < 0) return;
		if (seen[index] != null) {
		    // check that only uniform classes are inherited several times.
		    if (!clazz.isCompoundSym() && !baseclazz.isTrait()) {
			unit.error(clazz.pos, "illegal inheritance;\n" + clazz +
			      " inherits " + baseclazz + " twice");
		    }
		    // if there are two different type instances of same class
		    // check that second is a subtype of first.
		    if (!seen[index].isSubType(tp)) {
			String msg = (clazz.isCompoundSym())
			    ? "illegal combination;\n compound type combines"
			    : "illegal inheritance;\n " + clazz + " inherits";
			unit.error(clazz.pos, msg + " different type instances of " +
			      baseclazz + ":\n" + tp + " and " + seen[index]);
		    }
		}
		// check that case classes do not inherit from case classes
		if (baseclazz.isCaseClass())
		    if (caseSeen.isCaseClass())
			unit.error(
			    clazz.pos, "illegal combination of case " +
			    caseSeen + " and case " + baseclazz + " in one object");
		    else
			caseSeen = baseclazz;

		seen[index] = tp;
		validateBaseTypes(clazz, tp.parents(), seen, caseSeen, start);
	    }
	}

// Variance Checking --------------------------------------------------------

    private final int
        ContraVariance = -1,
	NoVariance = 0,
	CoVariance = 1,
	AnyVariance = 2;

    private String varianceString(int variance) {
	if (variance == 1) return "covariant";
	else if (variance == -1) return "contravariant";
	else return "invariant";
    }

    /** The variance of symbol `base' relative to the class which defines `tvar'.
     */
    int flip(Symbol base, Symbol tvar) {
	Symbol clazz = tvar.owner().constructorClass();
	Symbol sym = base;
	int flip = CoVariance;
	while (sym != clazz && flip != AnyVariance) {
	    //System.out.println("flip: " + sym + " " + sym.isParameter());//DEBUG
	    if (sym.isParameter()) flip = -flip;
	    else if (sym.owner().kind != CLASS) flip = AnyVariance;
	    else if (sym.kind == ALIAS) flip = NoVariance;
	    sym = sym.owner();
	}
	return flip;
    }

    /** Check variance of type variables in this type
     */
    void validateVariance(Symbol base, Type tp, int variance) {
	validateVariance(base, tp, tp, variance);
    }

    void validateVariance(Symbol base, Type all, Type tp, int variance) {
	switch (tp) {
	case ErrorType:
	case AnyType:
	case NoType:
	case ThisType(Symbol sym):
	    break;
	case SingleType(Type pre, Symbol sym):
	    validateVariance(base, all, pre, variance);
	    break;
	case TypeRef(Type pre, Symbol sym, Type[] args):
	    if (sym.variance() != 0) {
		int f = flip(base, sym);
		if (f != AnyVariance && sym.variance() != f * variance) {
		    //System.out.println("flip(" + base + "," + sym + ") = " + f);//DEBUG
		    unit.error(base.pos,
			  varianceString(sym.variance()) + " " + sym +
			  " occurs in " + varianceString(f * variance) +
			  " position in type " + all + " of " + base);
		}
	    }
	    validateVariance(base, all, pre, variance);
	    validateVariance(base, all, args, variance, sym.typeParams());
	    break;
	case CompoundType(Type[] parts, Scope members):
	    validateVariance(base, all, parts, variance);
	    break;
	case MethodType(Symbol[] vparams, Type result):
	    validateVariance(base, all, result, variance);
	    break;
	case PolyType(Symbol[] tparams, Type result):
	    validateVariance(base, all, result, variance);
	    break;
	case OverloadedType(Symbol[] alts, Type[] alttypes):
	    validateVariance(base, all, alttypes, variance);
	}
    }

    void validateVariance(Symbol base, Type all, Type[] tps, int variance) {
	for (int i = 0; i < tps.length; i++)
	    validateVariance(base, all, tps[i], variance);
    }

    void validateVariance(Symbol base, Type all, Type[] tps, int variance, Symbol[] tparams) {
	for (int i = 0; i < tps.length; i++)
	    validateVariance(base, all, tps[i], variance * tparams[i].variance());
    }

// Forward reference checking ---------------------------------------------------

    private Scope[] scopes = new Scope[4];
    private int[] maxindex = new int[4];
    private int[] refpos = new int[4];
    private Symbol[] refsym = new Symbol[4];
    private int level;
    private HashMap symIndex = new HashMap();

    private void pushLevel() {
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

    private void popLevel() {
	scopes[level] = null;
	level --;
    }

    private void enterSyms(Tree[] stats) {
	for (int i = 0; i < stats.length; i++) {
	    enterSym(stats[i], i);
	}
    }

    private void enterSym(Tree stat, int index) {
	Symbol sym = null;
	switch (stat) {
	case ClassDef(_, _, _, _, _, _):
	    sym = stat.symbol().primaryConstructor();
	    break;
	case DefDef(_, _, _, _, _, _):
	case ModuleDef(_, _, _, _):
	case ValDef(_, _, _, _):
	    sym = stat.symbol();
	}
	if (sym != null && sym.isLocal()) {
	    scopes[level].enter(new Scope.Entry(sym, scopes[level]));
	    symIndex.put(sym, new Integer(index));
	}
    }

// Module eliminiation -----------------------------------------------------------

    private Tree[] transformModule(Tree tree, int mods, Name name, Tree tpe, Tree.Template templ) {
	Symbol sym = tree.symbol();
	Tree cdef = gen.ClassDef(sym.moduleClass(), templ);
	Tree alloc = gen.New(gen.mkPrimaryConstr(tree.pos, sym.moduleClass()));
	if (sym.isGlobalModule()) {
	    Tree vdef = gen.ValDef(sym, alloc);
	    return new Tree[]{cdef, vdef};
	} else {
	    // var m$: T = null;
	    Name varname = Name.fromString(name + "$");
	    Symbol mvar = new TermSymbol(
		tree.pos, varname, sym.owner(), PRIVATE | MUTABLE | SYNTHETIC)
		.setInfo(sym.type());
	    Tree vdef = gen.ValDef(mvar, gen.Ident(tree.pos, defs.NULL));

	    // { if (null == m$) m$ = new m$class; m$ }
	    Symbol eqMethod = getUnaryMemberMethod(
		sym.type(), Names.EQEQ, defs.ANY_TYPE);
	    Tree body = gen.Block(new Tree[]{
		gen.If(
		    gen.Apply(
			gen.Select(gen.Ident(tree.pos, defs.NULL), eqMethod),
			new Tree[]{gen.mkRef(tree.pos, mvar)}),
		    gen.Assign(gen.mkRef(tree.pos, mvar), alloc),
		    gen.Block(tree.pos, Tree.EMPTY_ARRAY)),
		gen.mkRef(tree.pos, mvar)});

	    // def m: T = { if (m$ == null[T]) m$ = new m$class; m$ }
	    sym.updateInfo(Type.PolyType(Symbol.EMPTY_ARRAY, sym.type()));
	    sym.flags |= STABLE;
	    Tree ddef = gen.DefDef(sym, body);

	    // def m_eq(m: T): Unit = { m$ = m }
            Name m_eqname = name.append(Names._EQ);
	    Symbol m_eq = new TermSymbol(
		tree.pos, m_eqname, sym.owner(), PRIVATE | SYNTHETIC);
            Symbol m_eqarg = new TermSymbol(tree.pos, name, m_eq, 0)
                .setType(sym.type());
            m_eq.setInfo(
                Type.MethodType(new Symbol[] {m_eqarg}, defs.UNIT_TYPE));
            Tree m_eqdef = gen.DefDef(m_eq,
                gen.Assign(gen.mkRef(tree.pos, mvar), gen.Ident(tree.pos, m_eqarg)));
            sym.owner().members().enterOrOverload(m_eq);

	    return new Tree[]{cdef, vdef, ddef, m_eqdef};
	}
    }

// Adding case methods --------------------------------------------------------------

    private boolean hasImplementation(Symbol clazz, Name name) {
	Symbol sym = clazz.info().lookupNonPrivate(name);
	return sym.kind == VAL &&
	    (sym.owner() == clazz ||
	     !defs.JAVA_OBJECT_CLASS.isSubClass(sym.owner()) &&
	     (sym.flags & DEFERRED) == 0);
    }

    private Symbol getMember(Type site, Name name) {
	Symbol sym = site.lookupNonPrivate(name);
	assert sym.kind == VAL;
	return sym;
    }

    private Symbol getNullaryMemberMethod(Type site, Name name) {
	Symbol sym = getMember(site, name);
	switch (sym.type()) {
	case OverloadedType(Symbol[] alts, Type[] alttypes):
	    for (int i = 0; i < alts.length; i++) {
		if (isNullaryMethod(alttypes[i])) return alts[i];
	    }
	}
	assert isNullaryMethod(sym.type())
	    : "no nullary method " + name + " among " + sym.type() + " at " + site;
	return sym;
    }

    private boolean isNullaryMethod(Type tp) {
	return tp.paramSectionCount() == 1 && tp.firstParams().length == 0;
    }

    private Symbol getUnaryMemberMethod(Type site, Name name, Type paramtype) {
	Symbol sym = getMember(site, name);
	switch (sym.type()) {
	case OverloadedType(Symbol[] alts, Type[] alttypes):
	    for (int i = 0; i < alts.length; i++) {
		if (hasParam(alttypes[i], paramtype)) return alts[i];
	    }
	}
	assert hasParam(sym.type(), paramtype)
	    : "no (" + paramtype + ")-method " + name + " among " + sym.type() + " at " + site;
	return sym;
    }

    private boolean hasParam(Type tp, Type paramtype) {
	Symbol[] params = tp.firstParams();
	return params.length == 1 && paramtype.isSubType(params[0].type());
    }

    private Tree[] caseFields(ClassSymbol clazz) {
	Symbol[] vparams = clazz.primaryConstructor().type().firstParams();
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
	    .setInfo(defs.TOSTRING.type());
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
		String str = (i == fields.length - 1) ? ")" : ",";
		body = gen.Apply(
		    gen.Select(body, defs.STRING_PLUS_ANY),
		    new Tree[]{fields[i]});
		body = gen.Apply(
		    gen.Select(body, defs.STRING_PLUS_ANY),
		    new Tree[]{gen.mkStringLit(clazz.pos, str)});
	    }
	}
	return gen.DefDef(toStringSym, body);
    }

    private Tree equalsMethod(ClassSymbol clazz) {
	Symbol equalsSym = new TermSymbol(clazz.pos, Names.equals, clazz, OVERRIDE);
	Symbol equalsParam =
	    new TermSymbol(clazz.pos, Names.that, equalsSym, PARAM)
	    .setInfo(defs.ANY_TYPE);
	equalsSym.setInfo(
	    Type.MethodType(new Symbol[]{equalsParam}, defs.BOOLEAN_TYPE));
	clazz.info().members().enter(equalsSym);
	Tree[] fields = caseFields(clazz);
	Type testtp = clazz.type();
	{
	    Symbol[] tparams = clazz.typeParams();
	    if (tparams.length != 0) {
		Type[] targs = new Type[tparams.length];
		for (int i = 0; i < targs.length; i++)
		    targs[i] = defs.ANY_TYPE;
		testtp = testtp.subst(tparams, targs);
	    }
	}
	// if (that is C) {...
	Tree cond = gen.TypeApply(
	    gen.Select(
		gen.mkRef(clazz.pos, Type.localThisType, equalsParam),
		defs.IS),
	    new Tree[]{gen.mkType(clazz.pos, testtp)});

	Tree thenpart;
	if (fields.length == 0) {
	    thenpart = gen.mkBooleanLit(clazz.pos, true);
	} else {
	    // val that1 = that as C;
	    Tree cast = gen.TypeApply(
		gen.Select(
		    gen.mkRef(clazz.pos, Type.localThisType, equalsParam),
		    defs.AS),
		new Tree[]{gen.mkType(clazz.pos, testtp)});
	    Symbol that1sym = new TermSymbol(clazz.pos, Names.that1, equalsSym, 0)
		.setType(testtp);
	    Tree that1def = gen.ValDef(that1sym, cast);

	    // this.elem_1 == that1.elem_1 && ... && this.elem_n == that1.elem_n
	    Tree cmp = eqOp(
		fields[0],
		qualCaseField(clazz,
		    gen.mkRef(clazz.pos, Type.localThisType, that1sym), 0));
	    for (int i = 1; i < fields.length; i++) {
		cmp = gen.Apply(
		    gen.Select(cmp, defs.AMPAMP()),
		    new Tree[]{
			eqOp(
			    fields[i],
			    qualCaseField(clazz,
				gen.mkRef(clazz.pos, Type.localThisType, that1sym), i))});
	    }
	    thenpart = gen.Block(new Tree[]{that1def, cmp});
	}
	Tree body = gen.If(cond, thenpart, gen.mkBooleanLit(clazz.pos, false));
	return gen.DefDef(equalsSym, body);
    }
    //where
	private Tree eqOp(Tree l, Tree r) {
	    Symbol eqMethod = getUnaryMemberMethod(l.type, Names.EQEQ, r.type);
	    return gen.Apply(gen.Select(l, eqMethod), new Tree[]{r});
	}

        private Tree qualCaseField(ClassSymbol clazz, Tree qual, int i) {
	    return gen.Select(qual, clazz.caseFieldAccessor(i));
	}

	private Tree tagMethod(ClassSymbol clazz) {
        Symbol tagSym = new TermSymbol(
            clazz.pos, Names.tag, clazz,
            clazz.isSubClass(defs.OBJECT_CLASS) ? OVERRIDE : 0)
            .setInfo(Type.MethodType(Symbol.EMPTY_ARRAY, defs.INT_TYPE));
        clazz.info().members().enter(tagSym);
        return gen.DefDef(tagSym, gen.mkIntLit(clazz.pos, clazz.tag()));
	}

    private Tree hashCodeMethod(ClassSymbol clazz) {
	Symbol hashCodeSym = new TermSymbol(
	    clazz.pos, Names.hashCode, clazz, OVERRIDE)
	    .setInfo(defs.HASHCODE.type());
	clazz.info().members().enter(hashCodeSym);
	Tree[] fields = caseFields(clazz);
	Symbol getClassMethod = getNullaryMemberMethod(clazz.type(), Names.getClass);
	Symbol addMethod = getUnaryMemberMethod(
	    defs.INT_TYPE, Names.ADD, defs.INT_TYPE);
	Symbol mulMethod = getUnaryMemberMethod(
	    defs.INT_TYPE, Names.MUL, defs.INT_TYPE);
	Tree body =
	    gen.Apply(
		gen.Select(
		    gen.Apply(
			gen.mkRef(clazz.pos, clazz.thisType(), getClassMethod),
			Tree.EMPTY_ARRAY),
		    getNullaryMemberMethod(getClassMethod.type().resultType(), Names.hashCode)),
		Tree.EMPTY_ARRAY);
	for (int i = 0; i < fields.length; i++) {
	    Tree operand = gen.Apply(
		gen.Select(
		    fields[i],
		    getNullaryMemberMethod(fields[i].type, Names.hashCode)),
		Tree.EMPTY_ARRAY);
	    body =
		gen.Apply(
		    gen.Select(
			gen.Apply(
			    gen.Select(body, mulMethod),
			    new Tree[]{gen.mkIntLit(clazz.pos, 41)}),
			addMethod),
		    new Tree[]{operand});
	}
	return gen.DefDef(hashCodeSym, body);
    }
    // where

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
	if (!hasImplementation(clazz, Names.equals))
	    ts.append(equalsMethod(clazz));
	if (!hasImplementation(clazz, Names.hashCode))
	    ts.append(hashCodeMethod(clazz));
	ts.append(tagMethod(clazz));
	if (ts.length() > 0) {
	    Tree[] stats1 = new Tree[stats.length + ts.length()];
	    System.arraycopy(stats, 0, stats1, 0, stats.length);
	    ts.copyTo(stats1, stats.length);
	    return stats1;
	} else {
	    return stats;
	}
    }

// Convert case factory calls to constructor calls ---------------------------

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
    //where
	private Tree toConstructor1(Tree tree, Symbol constr) {
	    switch (tree) {
	    case TypeApply(Tree fn, Tree[] args):
		return toMethodType(
		    copy.TypeApply(tree, toConstructor1(fn, constr), args));
	    case Ident(_):
		return toMethodType(
		    copy.Ident(tree, constr));
	    case Select(Tree qual, _):
		return toMethodType(
		    copy.Select(tree, constr, qual));
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

// Bounds checking -----------------------------------------------------------

    private void checkBounds(int pos, Symbol[] tparams, Type[] argtypes) {
	if (tparams.length == argtypes.length) {
	    try {
		infer.checkBounds(tparams, argtypes, "");
	    } catch (Type.Error ex) {
		unit.error(pos, ex.msg);
	    }
	}
    }

// Type node eliminiation ------------------------------------------------------

    private Tree elimTypeNode(Tree tree) {
	if (tree.isType() && !tree.isMissing())
	    return make.TypeTerm(tree.pos).setType(tree.type);
	else
	    return tree;
    }

// Transformation ---------------------------------------------------------------

    public Tree[] transformStats(Tree[] stats) {
	pushLevel();
	enterSyms(stats);
	int i = 0;
	while (i < stats.length) {
	    Object stat1 = transformStat(stats[i], i);
	    if (stat1 instanceof Tree) {
		stats[i] = (Tree) stat1;
		i = i + 1;
	    } else {
		Tree[] newstats = (Tree[]) stat1;
		Tree[] stats1 = new Tree[stats.length - 1 + newstats.length];
		System.arraycopy(stats, 0, stats1, 0, i);
		System.arraycopy(newstats, 0, stats1, i, newstats.length);
		System.arraycopy(stats, i + 1, stats1, i + newstats.length,
				 stats.length - i - 1);
		stats = stats1;
		i = i + newstats.length;
	    }
	}
	popLevel();
	return stats;
    }

    public Object transformStat(Tree tree, int index) {
	switch (tree) {
	case ModuleDef(int mods, Name name, Tree tpe, Tree.Template templ):
	    return transform(transformModule(tree, mods, name, tpe, templ));

	case ValDef(int mods, Name name, Tree tpe, Tree rhs):
	    Symbol sym = tree.symbol();
	    validateVariance(
		sym, sym.type(),
		((sym.flags & MUTABLE) != 0) ? NoVariance : CoVariance);
	    Tree tree1 = transform(tree);
	    //todo: handle variables
	    if (sym.isLocal() && !sym.isModule() && index <= maxindex[level]) {
		if (Global.instance.debug)
		    System.out.println(refsym[level] + ":" + refsym[level].type());
		String kind = ((sym.flags & MUTABLE) != 0) ? "variable" : "value";
		unit.error(
		    refpos[level],
		    "forward reference extends over definition of " + kind + " " +
		    normalize(name));
	    }
	    return tree1;

	default:
	    return transform(tree);
	}
    }

    public Tree transform(Tree tree) {
	Symbol sym = tree.symbol();
	switch (tree) {
	case ClassDef(_, _, Tree.AbsTypeDef[] tparams, Tree.ValDef[][] vparams, Tree tpe, Tree.Template templ):
	    Symbol enclClassPrev = enclClass;
	    enclClass = sym;
	    validateVariance(sym, sym.info(), CoVariance);
	    validateVariance(sym, sym.typeOfThis(), CoVariance);
	    Tree tree1 = super.transform(
		copy.ClassDef(tree, tree.symbol(), tparams, vparams, tpe, addCaseMethods(templ, tree.symbol())));
	    enclClass = enclClassPrev;
	    return tree1;

	case DefDef(_, _, _, _, _, _):
	    validateVariance(sym, sym.type(), CoVariance);
	    return super.transform(tree);

	case ValDef(_, _, _, _):
	    validateVariance(
		sym, sym.type(),
		((sym.flags & MUTABLE) != 0) ? NoVariance : CoVariance);
	    return super.transform(tree);

	case AbsTypeDef(_, _, _, _):
	    validateVariance(sym, sym.info(), CoVariance);
	    validateVariance(sym, sym.loBound(), ContraVariance);
	    return super.transform(tree);

	case AliasTypeDef(_, _, _, _):
	    validateVariance(sym, sym.info(), NoVariance);
	    return super.transform(tree);

	case Template(Tree[] bases, Tree[] body):
	    Tree[] bases1 = transform(bases);
	    Tree[] body1 = transformStats(body);
	    if (sym.kind == VAL) {
		Symbol owner = tree.symbol().owner();
		validateBaseTypes(owner);
		checkAllOverrides(tree.pos, owner);
	    }
	    return copy.Template(tree, bases1, body1);

	case Block(Tree[] stats):
	    Tree[] stats1 = transformStats(stats);
	    return copy.Block(tree, stats1);

	case This(_):
	    return tree;

	case PackageDef(Tree pkg, Template packaged):
	    return copy.PackageDef(tree, pkg, super.transform(packaged));

	case TypeApply(Tree fn, Tree[] args):
	    switch (fn.type) {
	    case PolyType(Symbol[] tparams, Type restp):
		checkBounds(tree.pos, tparams, Tree.typeOf(args));
	    }
	    return super.transform(tree);

	case Apply(Tree fn, Tree[] args):
	    Symbol fsym = TreeInfo.methSymbol(fn);
	    assert fsym != Symbol.NONE : tree;
	    if (fsym != null && fsym.isMethod() && !fsym.isConstructor() &&
		(fsym.flags & CASE) != 0) {
		// convert case methods to new's
		Symbol constr = fsym.type().resultType().symbol().primaryConstructor();
		tree = gen.New(toConstructor(tree, constr));
	    }

	    return super.transform(tree);

	case AppliedType(Tree tpe, Tree[] args):
	    Symbol[] tparams = tpe.type.symbol().typeParams();
	    checkBounds(tree.pos, tparams, Tree.typeOf(args));
	    return elimTypeNode(super.transform(tree));

	case CompoundType(_, _):
	    Symbol clazz = tree.type.symbol();
	    validateBaseTypes(clazz);
	    checkAllOverrides(tree.pos, clazz);
	    return elimTypeNode(super.transform(tree));

	case Ident(Name name):
	    if( TreeInfo.isWildcardPattern( tree ) )
		return elimTypeNode(tree);

	    //System.out.println("name: "+name);
	    Scope.Entry e = scopes[level].lookupEntry(name);
	    //System.out.println("sym: "+sym);
	    if (sym != null && sym.isLocal() && sym == e.sym) {
		int i = level;
		while (scopes[i] != e.owner) i--;
		int symindex = ((Integer) symIndex.get(tree.symbol())).intValue();
		if (maxindex[i] < symindex) {
		    refpos[i] = tree.pos;
		    refsym[i] = e.sym;
		    maxindex[i] = symindex;
		}
	    }
	    sym.flags |= ACCESSED;
	    return elimTypeNode(tree);

	case Select(Tree qual, Name name):
	    sym.flags |= ACCESSED;
	    if (!TreeInfo.isSelf(qual, enclClass))
		sym.flags |= SELECTOR;
	    if (qual instanceof Tree.Super && (sym.flags & DEFERRED) != 0) {
		Symbol sym1 = enclClass.thisSym().info().lookup(sym.name);
		if ((sym1.flags & OVERRIDE) == 0 || (sym1.flags & DEFERRED) != 0)
		    unit.error(tree.pos,
		        "symbol accessed from super may not be abstract");
	    }
	    return elimTypeNode(super.transform(tree));

	default:
	    return elimTypeNode(super.transform(tree));
	}
    }
}
