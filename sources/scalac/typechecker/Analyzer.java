/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

// todo: (0) propagate target type in cast.
// todo: (1) check that only stable defs override stable defs
// todo: eliminate Typed nodes.

package scalac.typechecker;

import scalac.*;
import scalac.util.*;
import scalac.ast.*;
import scalac.ast.printer.*;
import scalac.symtab.*;
import Tree.*;
import java.util.HashMap;

public class Analyzer extends Transformer implements Modifiers, Kinds {

    private final Definitions definitions;
    private final DeSugarize desugarize;
    private final AnalyzerPhase descr;
    final Infer infer;

    public Analyzer(Global global, AnalyzerPhase descr) {
        super(global);
        this.definitions = global.definitions;
	this.descr = descr;
	this.infer = new Infer(this);
	this.desugarize = new DeSugarize(this, global);
    }

    /** Phase variables, used and set in transformers;
     */
    private Unit unit;
    private Context context;
    private Type pt;
    private int mode;

    public void apply() {
	for (int i = 0; i < global.units.length; i++) {
            enterUnit(global.units[i]);
        }
	super.apply();
	int n = descr.newSources.size();
        while (n > 0) {
	    int l = global.units.length;
            Unit[] newUnits = new Unit[l + n];
            System.arraycopy(global.units, 0, newUnits, 0, l);
            for (int i = 0; i < n; i++)
                newUnits[i + l] = (Unit)descr.newSources.get(i);
            global.units = newUnits;
            descr.newSources.clear();
            for (int i = l; i < newUnits.length; i++) {
                apply(newUnits[i]);
	    }
	    n = descr.newSources.size();
        }
    }

    public void enterUnit(Unit unit) {
	enter(new Context(Tree.Empty, unit.console ? descr.consoleContext : descr.startContext), unit);
    }

    public void enter(Context context, Unit unit) {
        assert this.unit == null : "start unit non null for " + unit;
	this.unit = unit;
	this.context = context;
	ImportList prevImports = context.imports;
        descr.contexts.put(unit, context);
	enterSyms(unit.body);
	context.imports = prevImports;
        this.unit = null;
	this.context = null;
    }

    public void lateEnter(Unit unit, Symbol sym) {
	assert sym.pos == Position.NOPOS : sym;
	enterUnit(unit);
	if (sym.pos == Position.NOPOS) {
            sym.setInfo(Type.ErrorType);
	    String kind;
	    if (sym.name.isTermName()) kind = "object or method ";
	    else if (sym.name.isTypeName()) kind = "class ";
	    else kind = "constructor ";
	    throw new Type.Error("file " + unit.source + " does not define public " +
				 kind + sym.fullName());
	} else {
	    descr.newSources.add(unit);
	}
    }

    public void apply(Unit unit) {
	global.log("checking " + unit);
        assert this.unit == null : "start unit non null for " + unit;
	this.unit = unit;
        this.context = (Context)descr.contexts.remove(unit);
        assert this.context != null : "could not find context for " + unit;
	//context.imports = context.outer.imports;
	unit.body = transformStatSeq(unit.body, Symbol.NONE);
	/** todo: check what this is for
	if (global.target == global.TARGET_JAVA && unit.errors == 0) {
	    unit.symdata = new SymData(unit);
	}
	*/
        this.unit = null;
        this.context = null;
	global.operation("checked " + unit);
    }

    /** Mode constants
     */
    static final int NOmode        = 0x000;
    static final int EXPRmode      = 0x001;  // these 4 modes are mutually exclusive.
    static final int PATTERNmode   = 0x002;
    static final int CONSTRmode    = 0x004;
    static final int TYPEmode      = 0x008;

    static final int FUNmode       = 0x10;  // orthogonal to above. When set
                                             // we are looking for a method or constructor

    static final int POLYmode      = 0x020;  // orthogonal to above. When set
                                             // expression types can be polymorphic.

    static final int QUALmode      = 0x040;  // orthogonal to above. When set
                                             // expressions may be packages and
                                             // Java statics modules.

    static final int baseModes     = EXPRmode | PATTERNmode | CONSTRmode;

// Helper definitions ---------------------------------------------------------

    /** The qualifier type of a potential application of the `match' method.
     *  or NoType, if this is something else.
     */
    private Type matchQualType(Tree fn) {
	switch (fn) {
	case Select(Tree qual, _):
	    if (fn.symbol() == definitions.OBJECT_TYPE.lookup(Names.match))
		return qual.type.widen();
	    break;
	case TypeApply(Tree fn1, _):
	    return matchQualType(fn1);
	case Ident(_):
	    if (fn.symbol() == definitions.OBJECT_TYPE.lookup(Names.match))
		return context.enclClass.owner.type();
	    break;
	}
	return fn.type == Type.ErrorType ? Type.ErrorType : Type.NoType;
    }

    private Type value2Type(Object value) {
	if (value instanceof Character) return definitions.CHAR_TYPE;
	else if (value instanceof Integer) return definitions.INT_TYPE;
	else if (value instanceof Long) return definitions.LONG_TYPE;
	else if (value instanceof Float) return definitions.FLOAT_TYPE;
	else if (value instanceof Double) return definitions.DOUBLE_TYPE;
	else if (value instanceof String) return definitions.JAVA_STRING_TYPE;
	else if (value instanceof Boolean) return definitions.BOOLEAN_TYPE;
	else throw new ApplicationError();
    }

    Tree errorTree(int pos) {
	return make.Bad(pos).setSymbol(Symbol.ERROR).setType(Type.ErrorType);
    }

    Tree error(int pos, String msg) {
	unit.error(pos, msg);
	return errorTree(pos);
    }

    void explainTypes(Type found, Type required) {
	if (global.debug) {
	    Type.debugSwitch = true;
	    found.isSubType(required);
	    Type.debugSwitch = false;
	}
    }

    void typeError(int pos, Type found, Type req) {
	String explanation = "";
	switch (found) {
	case MethodType(_, Type restype):
	    if (infer.isCompatible(restype, req))
		explanation = "\n possible cause: missing arguments for method or constructor";
	}
	error(pos, infer.typeErrorMsg("type mismatch", found, req) + explanation);
    }

    void reportTypeError(int pos, Type.Error ex) {
	if (ex instanceof CyclicReference) {
	    if (global.debug) ex.printStackTrace();
	    CyclicReference cyc = (CyclicReference) ex;
	    if (cyc.info instanceof LazyTreeType) {
		switch (((LazyTreeType) cyc.info).tree) {
		case ValDef(_, _, Tree.Empty, _):
		    error(pos, "recursive " + cyc.sym + " needs type");
		    break;
		case DefDef(_, _, _, _, Tree.Empty, _):
		    error(pos, "recursive function " + cyc.sym.name + " needs result type");
		}
	    }
	}
	//throw ex;//DEBUG
	error(pos, ex.msg);
    }

// Name resolution -----------------------------------------------------------

    String decode(Name name) {
	if (name.isTypeName()) return "type " + name;
	else if (name.isConstrName()) return "constructor " + name;
	else return "value " + name.toString();
    }

    /** Is `sym' accessible as a member of tree `site' in current context?
     */
    boolean isAccessible(Symbol sym, Tree site) {
	return
	    (sym.flags & (PRIVATE | PROTECTED)) == 0
	    ||
	    accessWithin(sym.owner())
	    ||
	    ((sym.flags & PRIVATE) == 0) &&
	    site.type.symbol().isSubClass(sym.owner()) &&
	    (site instanceof Tree.Super ||
	     isSubClassOfEnclosing(site.type.symbol()));
    } //where

	/** Are we inside definition of `owner'?
	 */
	boolean accessWithin(Symbol owner) {
	    Context c = context;
	    while (c != Context.NONE && c.owner != owner) {
		c = c.outer.enclClass;
	    }
	    return c != Context.NONE;
	}

	/** Is `clazz' a subclass of an enclosing class?
	 */
        boolean isSubClassOfEnclosing(Symbol clazz) {
	    Context c = context;
	    while (c != Context.NONE && !clazz.isSubClass(c.owner)) {
		c = c.outer.enclClass;
	    }
	    return c != Context.NONE;
	}

// Checking methods ----------------------------------------------------------

    /** Check that symbol's definition is well-formed. This means:
     *   - no conflicting modifiers
     *   - def modifiers only in methods
     *   - declarations only in classes
     *   - classes with abstract members have `abstract' modifier.
     *   - symbols with `override' modifier override some other symbol.
     */
    void validate(Symbol sym) {
	checkNoConflict(sym, DEFERRED, PRIVATE);
	checkNoConflict(sym, FINAL, PRIVATE);
	checkNoConflict(sym, PRIVATE, PROTECTED);
	checkNoConflict(sym, PRIVATE, OVERRIDE);
	checkNoConflict(sym, DEFERRED, FINAL);
	if ((sym.flags & ABSTRACTCLASS) != 0 && sym.kind != CLASS) {
	    error(sym.pos, "`abstract' modifier can be used only for classes; " +
		  "\nit should be omitted for abstract members");
	}
	if ((sym.flags & OVERRIDE) != 0 && sym.kind == CLASS) {
	    error(sym.pos, "`override' modifier ot allowed for classes");
	}
	if ((sym.flags & DEF) != 0 && sym.owner().isPrimaryConstructor()) {
	    error(sym.pos, "`def' modifier not allowed for class parameters");
	}
	if ((sym.flags & DEFERRED) != 0) {
	    if (sym.owner().kind != CLASS ||
		(sym.owner().flags & MODUL) != 0 ||
		sym.owner().isAnonymousClass()) {
		error(sym.pos, abstractVarNote(sym,
		      "only classes can have declared but undefined members"));
		sym.flags &= ~DEFERRED;
	    }
	}
	if ((sym.flags & OVERRIDE) != 0) {
	    int i = -1;
	    if (sym.owner().kind == CLASS) {
		Type[] parents = sym.owner().info().parents();
		i = parents.length - 1;
		while (i >= 0 &&
		       parents[i].lookupNonPrivate(sym.name).kind == NONE)
		    i--;
	    }
	    if (i < 0) {
		error(sym.pos, sym + " overrides nothing");
		sym.flags &= ~OVERRIDE;
	    }
	}
    }

    /** Check that
     *  - all parents are class types
     *  - supertype conforms to supertypes of all mixin types.
     *  - final classes are only inherited by classes which are
     *    nested within definition of base class, or that occur within same
     *    statement sequence.
     *  - self-type of current class is a subtype of self-type of each parent class.
     *  - parent types do not refer to value parameters of class.
     */
    void validateParentClasses(Tree[] constrs, Type[] parents, Type selfType) {
	if (parents.length == 0 || !checkClassType(constrs[0].pos, parents[0])) return;
	for (int i = 0; i < parents.length; i++) {
	    if (!checkClassType(constrs[i].pos, parents[i])) return;
	    Symbol bsym = parents[i].symbol();
	    if (i == 0) {
		if ((bsym.flags & INTERFACE) != 0)
		    error(constrs[0].pos, "superclass may not be a Java interface");
	    } else {
		if ((bsym.flags & (JAVA | INTERFACE)) == JAVA)
		    error(constrs[i].pos, "Java class may not be used as mixin");
		Type[] grandparents = parents[i].parents();
		if (grandparents.length > 0 &&
		    !parents[0].isSubType(grandparents[0]))
		    error(constrs[i].pos, "illegal inheritance;\n " + parents[0] +
			  " does not conform to " + parents[i] + "'s supertype");
	    }
	    if ((bsym.flags & FINAL) != 0) {
		// are we in same scope as base type definition?
		Scope.Entry e = context.scope.lookupEntry(bsym.name);
		if (e.sym != bsym || e.owner != context.scope) {
		    // we are not within same statement sequence
		    Context c = context;
		    while (c != Context.NONE && c.owner !=  bsym)
			c = c.outer;
		    if (c == Context.NONE) {
			error(constrs[i].pos, "illegal inheritance from final class");
		    }
		}
	    }
	    if (!selfType.isSubType(parents[i].instanceType())) {
		error(constrs[i].pos, "illegal inheritance;\n self-type " +
		      selfType + " does not conform to " + parents[i] +
		      "'s selftype " + parents[i].instanceType());
	    }
	}
    }

    /** Check that type is a class type.
     */
    private boolean checkClassType(int pos, Type tp) {
	switch (tp.unalias()) {
	case TypeRef(_, Symbol sym, _):
	    if (sym.kind == CLASS) return true;
	    else if (sym.kind == ERROR) return false;
	    break;
	case ErrorType:
	    return false;
	}
	error(pos, "class type expected");
	return false;
    }

    /** Check that type is an object type
     */
    private Type checkObjectType(int pos, Type tp) {
	if (tp.isObjectType()) return tp;
	else {
	    if (tp != Type.ErrorType) error(pos, "object type expected");
	    return Type.ErrorType;
	}
    }

    /** 1. Check that only parameterless (uniform) classes are inherited several times.
     *  2. Check that all type instances of an inherited uniform class are the same.
     *  3. Check that case classes do not inherit from case classes.
     */
    void validateBaseTypes(Symbol clazz) {
	validateBaseTypes(clazz, clazz.type().parents(),
			  new boolean[clazz.closure().length], 0);
    }
    //where
        void validateBaseTypes(Symbol clazz, Type[] tps, boolean[] seen, int start) {
	    for (int i = tps.length - 1; i >= start; i--) {
		validateBaseTypes(clazz, tps[i].unalias(), seen, i == 0 ? 0 : 1);
	    }
	}

	void validateBaseTypes(Symbol clazz, Type tp, boolean[] seen, int start) {
	    Symbol baseclazz = tp.symbol();
	    if (baseclazz.kind == CLASS) {
		int index = clazz.closurePos(baseclazz);
                if (index < 0) return;
		if (seen[index]) {
		    // check that only uniform classes are inherited several times.
		    if (!clazz.isCompoundSym() && !baseclazz.isTrait()) {
			error(clazz.pos, "illegal inheritance;\n" + clazz +
			      " inherits " + baseclazz + " twice");
		    }
		    // check no two different type instances of same class
		    // are inherited.
		    Type tp1 = clazz.closure()[index];
		    if (!tp1.isSameAs(tp)) {
			if (clazz.isCompoundSym())
			    error(clazz.pos,
				  "illegal combination;\n " + "compound type " +
				  " combines different type instances of " +
				  baseclazz + ":\n" + tp + " and " + tp1);
			else
			    error(clazz.pos, "illegal inheritance;\n " + clazz +
				  " inherits different type instances of " +
				  baseclazz + ":\n" + tp + " and " + tp1);
		    }
		}
		// check that case classes do not inherit from case classes
		if (clazz.isCaseClass() && baseclazz.isCaseClass())
		    error(clazz.pos, "illegal inheritance;\n " + "case " + clazz +
			  " inherits from other case " + baseclazz);

		seen[index] = true;
		validateBaseTypes(clazz, tp.parents(), seen, start);
	    }
	}

    /** Check that type is eta-expandable (i.e. no `def' parameters)
     */
    void checkEtaExpandable(int pos, Type tp) {
	switch (tp) {
	case MethodType(Symbol[] params, Type restype):
	    for (int i = 0; i < params.length; i++) {
		if ((params[i].flags & DEF) != 0)
		    error(pos, "method with `def' parameters needs to be fully applied");
		if ((params[i].flags & REPEATED) != 0)
		    error(pos, "method with `*' parameters needs to be fully applied");
	    }
	    checkEtaExpandable(pos, restype);
	}
    }

    /** Check that `sym' does not contain both `flag1' and `flag2'
     */
    void checkNoConflict(Symbol sym, int flag1, int flag2) {
	if ((sym.flags & (flag1 | flag2)) == (flag1 | flag2)) {
	    if (flag1 == DEFERRED)
		error(sym.pos, "abstract member may not have " +
		      Modifiers.Helper.toString(flag2) + " modifier");
	    else
		error(sym.pos, "illegal combination of modifiers: " +
		      Modifiers.Helper.toString(flag1) + " and " +
		      Modifiers.Helper.toString(flag2));
	}
    }

    /** Check that type `tp' is not a subtype of itself.
     */
    public void checkNonCyclic(int pos, Type tp) {
	switch (tp) {
	case TypeRef(Type pre, Symbol sym, Type[] args):
	    sym.initialize();
	    if ((sym.flags & LOCKED) != 0) {
		error(pos, "cyclic aliasing or subtyping involving " + sym);
	    } else if (sym.kind == ALIAS || sym.kind == TYPE) {
		assert (sym.flags & LOCKED) == 0;
		sym.flags |= LOCKED;
		checkNonCyclic(
		    pos, pre.memberInfo(sym).subst(sym.typeParams(), args));
		sym.flags &= ~LOCKED;
	    }
	    break;
	case CompoundType(Type[] parents, Scope members):
	    for (int i = 0; i < parents.length; i++) {
		checkNonCyclic(pos, parents[i]);
	    }
	    break;
	case SingleType(Type pre, Symbol sym):
	    sym.initialize();
	    if ((sym.flags & LOCKED) != 0) {
		error(pos, "cyclic aliasing or subtyping involving " + sym);
	    }
	}
    }

    /** Check that type does not refer to components defined in current scope.
     */
    Type checkNoEscape(int pos, Type tp) {
	try {
	    return checkNoEscapeMap.apply(tp);
	} catch (Type.Error ex) {
	    error(pos, ex.msg + " as part of " + tp.unalias());
	    return Type.ErrorType;
	}
    }
    //where
	private Type.Map checkNoEscapeMap = new Type.Map() {
	    public Type apply(Type t) {
		switch (t) {
		case TypeRef(Type pre, Symbol sym, Type[] args):
		    if (sym.kind == ALIAS) return apply(t.unalias());
		    else if (pre instanceof Type.ThisType) checkNoEscape(t, sym);
		    break;
		case SingleType(ThisType(_), Symbol sym):
		    checkNoEscape(t, sym);
		    break;
		}
		return map(t);
	    }
    	    private void checkNoEscape(Type t, Symbol sym) {
		Scope.Entry e = context.scope.lookupEntry(sym.name);
		if (e.sym == sym && e.owner == context.scope &&
		    !(e.sym.kind == TYPE && (e.sym.flags & PARAM) != 0)) {
		    throw new Type.Error(
			"type " + t + " escapes its defining scope");
		}
	    }
	};

    /** Check that tree represents a pure definition.
     */
    void checkPureDef(Tree tree, Symbol clazz) {
	if (!TreeInfo.isPureDef(tree) && tree.type != Type.ErrorType)
	    error(tree.pos, clazz + " may contain only pure definitions");
    }

    /** Check that tree represents a pure definition.
     */
    void checkTrait(Tree tree, Symbol clazz) {
	if (!TreeInfo.isPureConstr(tree) &&
	    tree.type != Type.ErrorType /*&&
					  !tree.type.symbol().isTrait()*/)
	    error(tree.pos, " " + clazz + " may inherit only from stable trait constructors");
    }

    /** Check that tree is a stable expression .p
     */
    Tree checkStable(Tree tree) {
	if (TreeInfo.isPureExpr(tree) || tree.type == Type.ErrorType) return tree;
	//new TextTreePrinter().print(tree).end();//DEBUG
	//System.out.println(" " + tree.symbol() + ":" + tree.type);//DEBUG

	//Symbol sym = tree.symbol();
	//if (sym != null) {
	//    System.out.println(sym.kind == VAL);
	//    System.out.println(sym.type().unalias());
	//    System.out.println(sym.type().isObjectType());
	//    System.out.println(sym.owner().isPrimaryConstructor());
	//}

	return error(tree.pos, "stable identifier required");
    }

    /** Check that class can be instantiated.
     */
    void checkInstantiatable(int pos, Type tp) {
	Symbol clazz = tp.symbol();
	if (clazz.kind == CLASS) {
	    if (clazz.isAbstractClass())
		error(pos, clazz + " is abstract, so it cannot be instantiated");
	    else if (!tp.isSubType(tp.instanceType()))
		error(pos, tp + " does not conform to its self-type " +
		      tp.instanceType() + ", so it cannot be instantiated");
	}
    }

    /** Check all members of class `clazz' for overriding conditions.
     */
    void checkAllOverrides(Symbol clazz) {
	Type[] closure = clazz.closure();
	for (int i = 0; i < closure.length; i++) {
	    for (Scope.SymbolIterator it = closure[i].members().iterator();
		 it.hasNext();) {
		Symbol other = it.next();
		Symbol member = clazz.info().lookup(other.name);
		if (other != member && (other.flags & PRIVATE) == 0 &&
		    member.kind != NONE)
		    checkOverride(clazz, member, other);
		if ((member.flags & DEFERRED) != 0 &&
		    clazz.kind == CLASS &&
		    (clazz.flags & ABSTRACTCLASS) == 0) {
		    if (clazz.isAnonymousClass())
			error(clazz.pos, "object creation impossible, since " +
			      member + member.locationString() + " is not defined");
		    else
			error(clazz.pos,
			    clazz + abstractVarNote(
				member,	" needs to be abstract; it does not define " +
				member + member.locationString()));
		    clazz.flags |= ABSTRACTCLASS;
		}
	    }
	}
    }

    /** Check that all conditions for overriding `other' by `member' are met.
     */
    void checkOverride(Symbol clazz, Symbol member, Symbol other) {
	int pos;
	if (member.owner() == clazz) pos = member.pos;
	else if (!member.owner().isSubClass(other.owner())) pos = context.tree.pos;
	else return; // everything was already checked elsewhere

	if ((member.flags & PRIVATE) != 0) {
	    overrideError(pos, member, other, "should not be private");
	} else if ((other.flags & PROTECTED) != 0 && (member.flags & PROTECTED) == 0) {
	    overrideError(pos, member, other,  "needs `protected' modifier");
	} else if ((other.flags & FINAL) != 0) {
	    overrideError(pos, member, other, "cannot override final member");
	} else if ((other.flags & DEFERRED) == 0 && ((member.flags & OVERRIDE) == 0)) {
	    overrideError(pos, member, other, "needs `override' modifier");
	} else if (other.isStable() && !member.isStable()) {
	    overrideError(pos, member, other, "needs to be an immutable value");
	} else {
	    Type self = clazz.thisType();
	    switch (other.kind) {
	    case CLASS:
 		overrideError(pos, member, other, "cannot override a class");
		break;
	    case ALIAS:
		if (!self.memberInfo(member).isSameAs(self.memberInfo(other)))
		    overrideTypeError(pos, member, other, self);
		break;
	    default:
		if (other.isConstructor())
		    overrideError(pos, member, other, "cannot override a class constructor");
		Type selftype = normalizedInfo(self, member);
		Type othertype = normalizedInfo(self, other);
		if (!selftype.isSubType(othertype))
		    overrideTypeError(pos, member, other, self);
	    }
	}
    }

    void overrideError(int pos, Symbol member, Symbol other, String msg) {
	if (other.type() != Type.ErrorType && member.type() != Type.ErrorType)
	    error(pos,
		"error overriding " + other + other.locationString() +
		"; " + member + member.locationString() + " " + msg);
    }

    void overrideTypeError(int pos, Symbol member, Symbol other, Type site) {
	if (other.type() != Type.ErrorType && member.type() != Type.ErrorType) {
	    error(pos,
		member + member.locationString() +
		infoString(member, normalizedInfo(site, member)) +
		"\n cannot override " + other + other.locationString() +
		infoString(other, normalizedInfo(site, other)));
	    explainTypes(normalizedInfo(site, member), normalizedInfo(site, other));
	}
    }

    Type normalizedInfo(Type site, Symbol sym) {
	Type tp = site.memberInfo(sym);
	if (sym.kind == VAL && (sym.flags & STABLE) != 0) tp = tp.resultType();
	return tp;
    }

    String infoString(Symbol sym, Type symtype) {
	switch (sym.kind) {
	case ALIAS: return ", which equals " + symtype;
	case TYPE:  return " bounded by " + symtype;
	case VAL:   return " of type " + symtype;
	default:    return "";
	}
    }

    String abstractVarNote(Symbol member, String msg) {
	String note = ((member.flags & MUTABLE) == 0) ? ""
	    : "\n(Note that variables need to be initialized to be defined)";
	return msg + note;
    }

// Entering Symbols ----------------------------------------------------------

    Tree transformPackageId(Tree tree) {
	switch (tree) {
	case Ident(Name name):
	    return tree
		.setSymbol(packageSymbol(tree.pos, definitions.ROOT, name))
	        .setType(tree.symbol().type());
	case Select(Tree qual, Name name):
	    Tree qual1 = transformPackageId(qual);
	    return copy.Select(tree, qual1, name)
		.setSymbol(packageSymbol(tree.pos, qual1.symbol(), name))
	        .setType(tree.symbol().type());
	default:
	    return transform(tree);
	}
    }

    Symbol packageSymbol(int pos, Symbol base, Name name) {
	Symbol p = base.members().lookup(name);
	if (p.kind == NONE) {
	    p = TermSymbol.newModule(
		Position.NOPOS, name, base.moduleClass(), JAVA | PACKAGE);
	    p.moduleClass().setInfo(Type.compoundType(Type.EMPTY_ARRAY, new Scope(), p));
	    base.members().enter(p);
	} else if (!p.isPackage()) {
	    error(pos, "package and class with same name");
	    p = Symbol.ERROR;
	}
	return p;
    }

    /** If `tree' is a definition, create a symbol for it with a lazily
     *  constructed type, and enter into current scope.
     */
    Symbol enterSym(Tree tree) {
	// todo: handle override qualifiers
	Symbol owner = context.owner;
	switch (tree) {
      	case PackageDef(Tree packaged, Tree.Template templ):
	    switch (templ) {
	    case Template(_, Tree[] body):
		pushContext(tree, context.owner, context.scope);
		context.imports = null;
		((PackageDef) tree).packaged = packaged = transformPackageId(packaged);
		popContext();
		Symbol pkg = checkStable(packaged).symbol();
		if (pkg != null && pkg.kind != ERROR) {
		    if (pkg.isPackage()) {
			pushContext(templ, pkg.moduleClass(), pkg.members());
			enterSyms(body);
			popContext();
		    } else {
			error(tree.pos, "only Java packages allowed for now");
		    }
		}
		templ.setSymbol(Symbol.NONE);
		return null;
	    default:
		throw new ApplicationError();
	    }

	case ClassDef(int mods, Name name, Tree.TypeDef[] tparams, Tree.ValDef[][] vparams, _, Tree.Template templ):
	    ClassSymbol clazz = new ClassSymbol(tree.pos, name, owner, mods);
	    if (clazz.isLocalClass()) unit.mangler.setMangledName(clazz);

	    enterSym(tree, clazz.constructor());
	    if ((mods & (ABSTRACTCLASS | CASE)) == CASE) {
		// enter case constructor method.
		enterInScope(
		    new TermSymbol(
			tree.pos, name.toTermName(), owner, mods & (ACCESSFLAGS | CASE))
		    .setInfo(new LazyConstrMethodType(tree)));
	    }
	    return enterSym(tree, clazz);

	case ModuleDef(int mods, Name name, _, _):
	    TermSymbol modul = TermSymbol.newModule(tree.pos, name, owner, mods);
	    Symbol clazz = modul.moduleClass();
	    clazz.setInfo(new LazyTreeType(tree));
	    if (clazz.isLocalClass()) unit.mangler.setMangledName(clazz);
	    return enterSym(tree, modul);

       	case ValDef(int mods, Name name, _, _):
	    return enterSym(tree, new TermSymbol(tree.pos, name, owner, mods));

	case DefDef(int mods, Name name, _, _, _, _):
	    return enterSym(tree, new TermSymbol(tree.pos, name, owner, mods));

	case TypeDef(int mods, Name name, _):
	    int kind = (mods & (DEFERRED | PARAM)) != 0 ? TYPE : ALIAS;
	    return enterSym(tree, new TypeSymbol(kind, tree.pos, name, owner, mods));

	case Import(Tree expr, Name[] selectors):
	    return enterImport(tree,
			new TermSymbol(
			    tree.pos,
			    Name.fromString("import " + expr),
			    Symbol.NONE, SYNTHETIC));

	default:
	    return null;
	}
    }//where

        /** Enter `sym' in current scope and make it the symbol of `tree'.
	 */
	private Symbol enterSym(Tree tree, Symbol sym) {
	    //if (global.debug) System.out.println("entering " + sym);//DEBUG
	    sym.setInfo(new LazyTreeType(tree));
	    sym = enterInScope(sym);
	    tree.setSymbol(sym);
	    return sym;
	}

	/** Make `sym' the symbol of import `tree' and create an entry in
	 *  current imports list.
	 */
	private Symbol enterImport(Tree tree, Symbol sym) {
	    sym.setInfo(new LazyTreeType(tree));
	    tree.setSymbol(sym);
	    context.imports = new ImportList(tree, context.scope, context.imports);
	    return sym;
	}

        /** Enter symbol `sym' in current scope. Check for double definitions.
	 *  Handle overloading.
	 */
	private Symbol enterInScope(Symbol sym) {
	    // handle double and overloaded definitions
	    Symbol result = sym;
	    Scope.Entry e = context.scope.lookupEntry(sym.name);
	    if (e.owner == context.scope) {
		Symbol other = e.sym;
		if (other.isPreloaded()) {
		    // symbol was preloaded from package;
		    // need to overwrite definition.
		    if (global.debug) System.out.println(sym + " overwrites " + other);//debug
		    sym.copyTo(other);
		    if (sym.isModule()) {
			sym.moduleClass().copyTo(
			    other.moduleClass());
			sym.moduleClass().constructor().copyTo(
			    other.moduleClass().constructor());
			other.moduleClass().constructor().setInfo(
			    Type.MethodType(
				Symbol.EMPTY_ARRAY,
				other.moduleClass().typeConstructor()));
		    }
		    result = other;
		} else if (sym.owner().isPackage()) {
		    if (global.compiledNow.get(other) != null) {
			error(sym.pos, sym + " is compiled twice");
		    }
		    context.scope.unlink(e);
		    context.scope.enter(sym);
		} else if (sym.kind == VAL && other.kind == VAL) {
		    // it's an overloaded definition
		    if (((sym.flags ^ other.flags) & SOURCEFLAGS) != 0) {
			error(sym.pos,
			      "illegal overloaded definition of " + sym +
			      ": modifier lists differ in " +
			      Modifiers.Helper.toString(
				  (sym.flags ^ other.flags) & SOURCEFLAGS));
		    } else {
			e.setSymbol(other.overloadWith(sym));
		    }
		} else {
		    error(sym.pos,
			  sym.nameString() + " is already defined as " +
			  other + other.locationString());
		}
	    } else {
		context.scope.enter(sym);
	    }
	    if (result.owner().isPackage())
		global.compiledNow.put(result, unit.source);
	    return result;
	}

    /** Enter all symbols in statement list
     */
    public void enterSyms(Tree[] stats) {
	for (int i = 0; i < stats.length; i++)
	    enterSym(stats[i]);
    }

// Definining Symbols -------------------------------------------------------

    /** Define symbol associated with `tree' using given `unit' and `context'.
     */
    void defineSym(Tree tree, Unit unit, Context curcontext) {
        Unit savedUnit = this.unit;
        this.unit = unit;
	Context savedContext = this.context;
	this.context = curcontext;
	int savedMode = this.mode;
	this.mode = EXPRmode;
	Type savedPt = this.pt;
	this.pt = Type.AnyType;

	try {
	    Symbol sym = tree.symbol();
	    if (global.debug) global.log("defining " + sym);
	    Type owntype;
	    switch (tree) {
	    case ClassDef(int mods, Name name, Tree.TypeDef[] tparams, Tree.ValDef[][] vparams, Tree tpe, Tree.Template templ):
		if ((mods & LOCKED) != 0 && !sym.isAnonymousClass()) {
		    sym.setInfo(Type.ErrorType);
		    throw new CyclicReference(sym, Type.NoType);
		}
		((ClassDef) tree).mods |= LOCKED;

		pushContext(tree, sym.constructor(), new Scope(context.scope));
		Symbol[] tparamSyms = enterParams(tparams);
		Symbol[][] vparamSyms = enterParams(vparams);
		if (vparamSyms.length == 0)
		    vparamSyms = new Symbol[][]{Symbol.EMPTY_ARRAY};

		if ((mods & CASE) != 0 && vparams.length > 0)
		    templ.body = desugarize.addCaseElements(templ.body, vparams[0]);

		Type constrtype = makeMethodType(
		    tparamSyms,
		    vparamSyms,
		    Type.TypeRef(sym.owner().thisType(), sym, Symbol.type(tparamSyms)));
		sym.constructor().setInfo(constrtype);
		// necessary so that we can access tparams
		sym.constructor().flags |= INITIALIZED;

		if (tpe != Tree.Empty)
		    sym.setTypeOfThis(new LazySelfType(tpe));

		defineTemplate(templ, sym);
		owntype = templ.type;
		popContext();
		break;

	    case ModuleDef(int mods, Name name, Tree tpe, Tree.Template templ):
		Symbol clazz = sym.moduleClass();
		defineTemplate(templ, clazz);
		clazz.setInfo(templ.type);
		if (tpe == Tree.Empty) owntype = clazz.type();
		else owntype = transform(tpe, TYPEmode).type;
		break;

	    case ValDef(int mods, Name name, Tree tpe, Tree rhs):
		if (tpe == Tree.Empty) {
		    pushContext(tree, sym, context.scope);
		    if (rhs == Tree.Empty) {
			if ((sym.owner().flags & ACCESSOR) != 0) {
			    // this is the paremeter of a variable setter method.
			    ((ValDef) tree).tpe = tpe =
				gen.mkType(tree.pos, sym.owner().accessed().type());
			} else {
			    error(tree.pos, "missing parameter type");
			    ((ValDef) tree).tpe = tpe =
				gen.mkType(tree.pos, Type.ErrorType);
			}
			owntype = tpe.type;
		    } else {
			if ((mods & CASEACCESSOR) != 0) {
			    //rhs was already attributed
			} else {
			    ((ValDef) tree).rhs = rhs = transform(rhs, EXPRmode);
			}
			owntype = rhs.type;
		    }
		    popContext();
		}  else {
		    owntype = transform(tpe, TYPEmode).type;
		}
		checkNonCyclic(tree.pos, owntype);
		break;

	    case DefDef(int mods, Name name, Tree.TypeDef[] tparams, Tree.ValDef[][] vparams, Tree tpe, Tree rhs):
		pushContext(tree, sym, new Scope(context.scope));
		Symbol[] tparamSyms = enterParams(tparams);
		Symbol[][] vparamSyms = enterParams(vparams);
		Type restpe;
		if (tpe == Tree.Empty) {
		    int rhsmode = name.isConstrName() ? CONSTRmode : EXPRmode;
		    ((DefDef) tree).rhs = rhs = transform(rhs, rhsmode);
		    restpe = checkNoEscape(rhs.pos, rhs.type);
		} else {
		    restpe = checkNoEscape(
			tpe.pos, transform(tpe, TYPEmode).type);
		}
		checkNonCyclic(tree.pos, restpe);
		popContext();
		owntype = makeMethodType(tparamSyms, vparamSyms, restpe);
		//System.out.println("methtype " + name + ":" + owntype);//DEBUG
		break;

	    case TypeDef(int mods, Name name, Tree rhs):
		//todo: alwyas have context.owner as owner.
		if (sym.kind == TYPE) {
		    pushContext(rhs, context.owner, context.scope);
		    context.delayArgs = true;
		    owntype = transform(rhs, TYPEmode).type;
		    owntype.symbol().initialize();//to detect cycles
		    popContext();
		} else { // sym.kind == ALIAS
		    pushContext(tree, sym, new Scope(context.scope));
		    owntype = transform(rhs, TYPEmode | FUNmode).type;
		    popContext();
		}
		checkNonCyclic(tree.pos, owntype);
		break;

	    case Import(Tree expr, Name[] selectors):
		Tree expr1 = transform(expr, EXPRmode | QUALmode);
		((Import) tree).expr = expr1;
		checkStable(expr1);
		owntype = expr1.type;
		break;

	    default:
		throw new ApplicationError();
	    }
	    sym.setInfo(owntype);
	    validate(sym);
	    if (global.debug) global.log("defined " + sym);//debug
	} catch (Type.Error ex) {
	    reportTypeError(tree.pos, ex);
	    tree.type = Type.ErrorType;
            if (tree.hasSymbol() && tree.symbol() == null) tree.setSymbol(Symbol.ERROR);
	}

        this.unit = savedUnit;
	this.context = savedContext;
	this.mode = savedMode;
	this.pt = savedPt;
    }

    /** Definition phase for a template. This enters all symbols in template
     *  into symbol table.
     */
    void defineTemplate(Tree.Template templ, Symbol clazz) {
	// attribute parent constructors
	Tree[] constrs = transformConstrInvocations(
	    templ.pos, templ.parents, true);

	Type[] parents = new Type[constrs.length];
	for (int i = 0; i < parents.length; i++)
	    parents[i] = constrs[i].type;

	// enter all members
	Scope members = new Scope();
	pushContext(templ, clazz, members);
	if ((clazz.flags & CASE) != 0)
	    templ.body = desugarize.addCaseMethods(templ.body, clazz, parents);
	templ.body = desugarize.Statements(templ.body, false);
	enterSyms(templ.body);
	popContext();

	templ.type = Type.compoundType(parents, members, clazz);
    }

    Symbol[] enterParams(Tree[] params) {
	for (int i = 0; i < params.length; i++) {
	    enterSym(params[i]);
	    switch (params[i]) {
	    case ValDef(int mods, _, _, _):
		if ((mods & REPEATED) != 0 && params.length > 1)
		    error(params[i].pos,
			  "`*' parameter must be the only parameter a `('...`)' section");
	    }
	}
	return Tree.symbolOf(params);
    }

    Symbol[][] enterParams(Tree[][] vparams) {
	Symbol[][] vparamSyms = new Symbol[vparams.length][];
	for (int i = 0; i < vparams.length; i++) {
	    vparamSyms[i] = enterParams(vparams[i]);
	}
	return vparamSyms;
    }

    Type makeMethodType(Symbol[] tparams, Symbol[][] vparams, Type restpe) {
	if (tparams.length == 0 && vparams.length == 0) {
	    return Type.PolyType(tparams, restpe);
	} else {
	    Type result = restpe;
	    for (int i = vparams.length - 1; i >= 0; i--)
		result = Type.MethodType(vparams[i], result);
	    if (tparams.length != 0)
		result = Type.PolyType(tparams, result);
	    return result;
	}
    }

    /** Re-enter type parameters in current scope.
     */
    void reenterParams(Tree[] params) {
	for (int i = 0; i < params.length; i++) {
	    context.scope.enter(params[i].symbol());
	}
    }

    /** Re-enter value parameters in current scope.
     */
    void reenterParams(Tree[][] vparams) {
	for (int i = 0; i < vparams.length; i++)
	    reenterParams(vparams[i]);
    }

    /** Define self type of class or module `sym'
     *  associated with `tree' using given `unit' and `context'.
     */
    void defineSelfType(Symbol sym, Tree tree, Unit unit, Context curcontext) {
        Unit savedUnit = this.unit;
        this.unit = unit;
	Context savedContext = this.context;
	this.context = curcontext;

	sym.setInfo(transform(tree, TYPEmode).type);

	this.unit = savedUnit;
	this.context= savedContext;
    }

// Attribution and Transform -------------------------------------------------

    /** Attribute an identifier consisting of a simple name or an outer reference.
     *  @param tree      The tree representing the identifier.
     *  @param name      The name of the identifier.
     */
    Tree transformIdent(Tree tree, Name name) {
	// find applicable definition and assign to `sym'
	Symbol sym = Symbol.NONE;
	Type pre;
	Type symtype;

	int stopPos = Integer.MIN_VALUE;
	Context nextcontext = context;
	while (sym.kind == NONE && nextcontext != Context.NONE) {
	    sym = nextcontext.scope.lookup(name);
	    if (sym.kind != NONE) {
		stopPos = sym.pos;
	    } else {
		nextcontext = nextcontext.enclClass;
		if (nextcontext != Context.NONE) {
		    sym = nextcontext.owner.info().lookup(name);
		    if (sym.kind != NONE) {
			stopPos = nextcontext.owner.pos;
		    } else {
			nextcontext = nextcontext.outer;
		    }
		}
	    }
	}

	// find applicable import and assign to `sym1'
	ImportList nextimports = context.imports;
	ImportList lastimports = null;
	Symbol sym1 = Symbol.NONE;

//	System.out.println("name = " + name + ", pos = " + tree.pos + ", importlist = ");//DEBUG
//	for (ImportList imp = nextimports; imp != null; imp = imp.prev) {
//	    new TextTreePrinter().print("    ").print(imp.tree).println().end();//DEBUG
//	}

	while (nextimports != null && nextimports.tree.pos >= tree.pos) {
	    nextimports = nextimports.prev;
	}
	while (sym1.kind == NONE &&
	       nextimports != null && nextimports.tree.pos > stopPos) {
	    sym1 = nextimports.importedSymbol(name);
	    lastimports = nextimports;
	    nextimports = nextimports.prev;
	}

	// evaluate what was found
	if (sym1.kind == NONE) {
	    if (sym.kind == NONE) {
		return error(tree.pos, "not found: " + decode(name));
	    } else {
		sym.flags |= ACCESSED;
		if (sym.owner().kind == CLASS) {
		    pre = nextcontext.enclClass.owner.thisType();
		    if (!sym.owner().isPackage()) {
			Tree qual = gen.mkStableId(tree.pos, pre);
			tree = make.Select(tree.pos, qual, name);
			//System.out.println(name + " :::> " + tree + " " + qual.symbol());//DEBUG
		    }
		} else {
		    pre = Type.localThisType;
		}
	    }
	} else if (sym.kind != NONE && !sym.isPreloaded()) {
	    return error(tree.pos,
			 "reference to " + name + " is ambiguous;\n" +
			 "it is both defined in " + sym.owner() +
			 " and imported subsequently by \n" + nextimports.tree);
	} else {
	    // check that there are no other applicable imports in same scope.
	    while (nextimports != null &&
		   nextimports.enclscope == lastimports.enclscope) {
		if (!nextimports.sameImport(lastimports) &&
		    nextimports.importedSymbol(name).kind != NONE) {
		    return error(tree.pos,
				 "reference to " + name + " is ambiguous;\n" +
				 "it is imported twice in the same scope by\n    " +
				 lastimports.tree + "\nand " + nextimports.tree);
		}
		nextimports = nextimports.prev;
	    }
	    sym = sym1;
	    sym.flags |= (ACCESSED | SELECTOR);
	    Tree qual = lastimports.importPrefix().duplicate();
	    pre = qual.type;
	    //new TextTreePrinter().print(name + " => ").print(lastimports.tree).print("." + name).println().end();//DEBUG
	    tree = make.Select(tree.pos, qual, name);
	}
	symtype = pre.memberType(sym);
	if ((pt != null && pt.isStable() || (mode & QUALmode) != 0) && sym.isStable()) {
	    //System.out.println("making single " + sym + ":" + symtype);//DEBUG
	    symtype = Type.singleType(pre, sym);
	}
	//System.out.println(name + ":" + symtype);//DEBUG
	return tree.setSymbol(sym).setType(symtype);
    }

    /** Attribute a selection where `tree' is `qual.name'.
     *  `qual' is already attributed.
     */
    Tree transformSelect(Tree tree, Tree qual, Name name) {
	Symbol[] uninst = Symbol.EMPTY_ARRAY;
	switch (qual.type) {
	case PolyType(Symbol[] tparams, Type restype):
	    qual = infer.mkTypeApply(qual, tparams, restype, Symbol.type(tparams));
	    uninst = tparams;
	}
	Symbol sym = qual.type.lookup(name);
	if (sym.kind == NONE) {
	    //System.out.println(qual.type + " has members " + qual.type.members());//DEBUG
	    return error(tree.pos,
			 decode(name) + " is not a member of " + qual.type.widen());
	} else if (!isAccessible(sym, qual)) {
	    return error(tree.pos, sym + " cannot be accessed in " + qual.type);
	} else {
	    sym.flags |= (ACCESSED | SELECTOR);
	    Type symtype = qual.type.memberType(sym);
	    //System.out.println(sym.name + ":" + symtype);//DEBUG
	    if (uninst.length != 0) {
		switch (symtype) {
		case PolyType(Symbol[] tparams, Type restype):
		    symtype = Type.PolyType(
			tparams, Type.PolyType(uninst, restype));
		    break;
		default:
		    symtype = Type.PolyType(uninst, symtype);
		}
	    }
	    if ((pt != null && pt.isStable() || (mode & QUALmode) != 0) &&
		sym.isStable() && qual.type.isStable())
		symtype = Type.singleType(qual.type, sym);
	    //System.out.println(qual.type + ".member: " + sym + ":" + symtype);//DEBUG
	    switch (tree) {
	    case Select(_, _):
		return copy.Select(tree, qual, name)
		    .setSymbol(sym).setType(symtype);
	    case SelectFromType(_, _):
		return make.TypeTerm(tree.pos).setType(symtype);
	    default:
		throw new ApplicationError();
	    }
	}
    }

    /** Attribute a pattern matching expression where `pattpe' is the
     *  expected type of the patterns and `pt' is the expected type of the
     *  results.
     */
    Tree transformVisitor(Tree tree, Type pattpe, Type pt) {
	//System.out.println("trans visitor with " + pt);//DEBUG
	switch (tree) {
	case Visitor(Tree.CaseDef[] cases):
	    Tree.CaseDef[] cases1 = cases;
	    for (int i = 0; i < cases.length; i++)
		cases1[i] = transformCase(cases[i], pattpe, pt);

	    return copy.Visitor(tree, cases1)
		.setType(Type.lub(Tree.typeOf(cases1)));
	default:
	    throw new ApplicationError();
	}
    }

    /** Attribute a case where `pattpe' is the expected type of the pattern
     *  and `pt' is the expected type of the result.
     */
    Tree.CaseDef transformCase(Tree.CaseDef tree, Type pattpe, Type pt) {
	switch (tree) {
	case CaseDef(Tree pat, Tree guard, Tree body):
	    pushContext(tree, context.owner, new Scope(context.scope));
	    Tree pat1 = transform(pat, PATTERNmode, pattpe);
	    Tree guard1 = guard;
	    if (guard != Tree.Empty)
		guard1 = transform(guard, EXPRmode, definitions.BOOLEAN_TYPE);
	    Tree body1 = transform(body, EXPRmode, pt);
	    popContext();
	    return (Tree.CaseDef) copy.CaseDef(tree, pat1, guard1, body1)
		.setType(body1.type);
	default:
	    throw new ApplicationError();
	}
    }

    Tree[] transformStatSeq(Tree[] stats, Symbol exprOwner) {
	Tree[] stats1 = stats;
	for (int i = 0; i < stats.length; i++) {
	    Tree stat = stats[i];
	    if (context.owner.isCompoundSym() && !TreeInfo.isDeclaration(stat)) {
		error(stat.pos, "only declarations allowed here");
	    }
	    Tree stat1;
	    if (exprOwner.kind != NONE && !TreeInfo.isOwnerDefinition(stat)) {
		pushContext(stat, exprOwner, context.scope);
		if (TreeInfo.isDefinition(stat)) stat1 = transform(stat);
		else stat1 = transform(stat, EXPRmode);
		popContext();
	    } else {
		if (TreeInfo.isDefinition(stat)) stat1 = transform(stat);
		else stat1 = transform(stat, EXPRmode);
	    }
	    if (stat1 != stat && stats1 == stats) {
		stats1 = new Tree[stats.length];
		System.arraycopy(stats, 0, stats1, 0, i);
	    }
	    stats1[i] = stat1;
	}
	return stats1;
    }

    /** Attribute a sequence of constructor invocations.
     */
    Tree[] transformConstrInvocations(int pos, Tree[] constrs, boolean delayArgs) {
	for (int i = 0; i < constrs.length; i++) {
	    pushContext(constrs[i], context.owner, context.scope);
	    context.delayArgs = delayArgs;
	    constrs[i] = transform(constrs[i], CONSTRmode, Type.AnyType);
	    Symbol f = TreeInfo.methSymbol(constrs[i]);
	    if (f != null) {
		Symbol c = f.primaryConstructorClass();
		if (c.kind == CLASS) c.initialize();//to detect cycles
	    }
	    popContext();
	}
	return constrs;
    }

    /** Attribute a template
     */
    public Tree.Template transformTemplate(Tree.Template templ, Symbol owner) {
	if (global.debug) global.log("transforming " + owner);//debug
	//System.out.println(owner.info());//DEBUG
	Tree[] parents1 = transformConstrInvocations(
	    templ.pos, templ.parents, false);
	if (owner.kind != ERROR) {
	    validateParentClasses(
		templ.parents, owner.info().parents(), owner.typeOfThis());
	    validateBaseTypes(owner);
	}
	pushContext(templ, owner, owner.members());
	templ.setSymbol(gen.localDummy(templ.pos, owner));
	Tree[] body1 = transformStatSeq(templ.body, templ.symbol());
	checkAllOverrides(owner);
	popContext();
	if (owner.isTrait()) {
	    for (int i = 0; i < templ.parents.length; i++)
		checkTrait(parents1[i], owner);
	    for (int i = 0; i < templ.body.length; i++)
		checkPureDef(body1[i], owner);
	}
	Tree.Template templ1 = copy.Template(templ, parents1, body1);
	templ1.setType(owner.type());
	return templ1;
    }

    public Tree transformApply(Tree tree, Tree fn, Tree[] args) {
	Tree fn1;
	int argMode;
	if ((mode & (EXPRmode | CONSTRmode)) != 0) {
	    fn1 = transform(fn, mode | FUNmode, Type.AnyType);
	    argMode = EXPRmode;
	} else {
	    assert (mode & PATTERNmode) != 0;
	    fn1 = transform(fn, mode | FUNmode, pt);
	    argMode = PATTERNmode;
	}

	// if function is overloaded with one alternative whose arity matches
	// argument length, preselect this alternative.
	switch (fn1.type) {
	case OverloadedType(Symbol[] alts, Type[] alttypes):
	    int matching1 = -1;
	    int matching2 = -1;
	    for (int i = 0; i < alttypes.length; i++) {
		Type alttp = alttypes[i];
		switch (alttp) {
		case PolyType(_, Type restp): alttp = restp;
		}
		switch (alttp) {
		case MethodType(Symbol[] params, _):
		    if (params.length == args.length ||
			params.length == 1 && (params[0].flags & REPEATED) != 0) {
			matching2 = matching1;
			matching1 = i;
		    }
		}
	    }
	    if (matching1 >= 0 && matching2 < 0)
		fn1.setSymbol(alts[matching1]).setType(alttypes[matching1]);
	}

	// handle the case of application of match to a visitor specially
	if (args.length == 1 && args[0] instanceof Visitor) {
	    Type pattp = matchQualType(fn1);
	    switch (fn1.type) {
	    case PolyType(Symbol[] tparams, _):
		if (pattp.containsSome(tparams)) {
		    if (global.debug) System.out.println(fn1.type + "+" + pattp);//debug
		    pattp = Type.AnyType; // so isFullyDefined fails below.
		}
	    }
	    if (pattp == Type.ErrorType) {
		return tree.setType(Type.ErrorType);
	    } else if (pattp != Type.NoType) {
		if (infer.isFullyDefined(pattp)) {
		    Tree fn2 = desugarize.postMatch(fn1, context.enclClass.owner);
		    Tree arg1 = transformVisitor(args[0], pattp, pt);
		    return copy.Apply(tree, fn2, new Tree[]{arg1})
			.setType(arg1.type);
		} else {
		    return error(tree.pos, "expected pattern type of cases could not be determined");
		}
	    }
	}

	// return prematurely if delayArgs is true and no type arguments
	// need to be inferred.
	if (context.delayArgs) {
	    switch (fn1.type) {
	    case MethodType(_, Type restp):
		return copy.Apply(tree, fn1, args).setType(restp);
	    }
	}

	// type arguments with formals as prototypes if they exist.
	fn1.type = infer.freshInstance(fn1.type);
	Type[] argtypes = transformArgs(
	    tree.pos, fn1.symbol(), Symbol.EMPTY_ARRAY, fn1.type, argMode, args, pt);

	// propagate errors in arguments
	if (argtypes == null) {
	    return tree.setType(Type.ErrorType);
	}
	for (int i = 0; i < argtypes.length; i++) {
	    if (argtypes[i] == Type.ErrorType) {
		return tree.setType(Type.ErrorType);
	    }
	}

	// resolve overloading
	switch (fn1.type) {
	case OverloadedType(Symbol[] alts, Type[] alttypes):
	    try {
		infer.methodAlternative(fn1, alts, alttypes, argtypes, pt);
	    } catch (Type.Error ex) {
		error(tree.pos, ex.msg);
	    }
	}

	switch (fn1.type) {
	case PolyType(Symbol[] tparams, Type restp):
	    // if method is polymorphic,
	    // infer instance, and adapt arguments to instantiated formals
	    try {
		fn1 = infer.methodInstance(fn1, tparams, restp, argtypes);
	    } catch (Type.Error ex) {
		error(tree.pos, ex.msg);
	    }
	    switch (fn1.type) {
	    case MethodType(Symbol[] params, Type restp1):
		Type[] formals = infer.formalTypes(params, args.length);
		for (int i = 0; i < args.length; i++) {
		    args[i] = adapt(args[i], argMode, formals[i]);
		}
		return copy.Apply(tree, fn1, args)
		    .setType(restp1);
	    }
	    break;
	case MethodType(Symbol[] params, Type restp):
	    // if method is monomorphic,
	    // check that it can be applied to arguments.
	    if (infer.isApplicable(fn1.type, argtypes, Type.AnyType)) {
		return copy.Apply(tree, fn1, args)
		    .setType(restp);
	    }
	}

	if (fn1.type == Type.ErrorType)
	    return tree.setType(Type.ErrorType);

	//new TextTreePrinter().print(tree).println().end();//DEBUG
	return error(tree.pos,
	    infer.applyErrorMsg(
		"", fn1, " cannot be applied to ", argtypes, pt));

    }

    /** Attribute an argument list.
     *  @param pos      Position for error reporting
     *  @param meth     The symbol of the called method, or `null' if none exists.
     *  @param tparams  The type parameters that need to be instantiated
     *  @param methtype The method's type w/o type parameters
     *  @param argMode  The argument mode (either EXPRmode or PATTERNmode)
     *  @param args     The actual arguments
     *  @param pt       The proto-resulttype.
     *  @return         The vector of instantiated argument types, or null if error.
     */
    Type[] transformArgs(int pos, Symbol meth, Symbol[] tparams, Type methtype,
			 int argMode, Tree[] args, Type pt) {
	//System.out.println("trans args " + meth + ArrayApply.toString(tparams) + ":" + methtype + "," + pt);//DEBUG
	Type[] argtypes = new Type[args.length];
	switch (methtype) {
	case MethodType(Symbol[] params, Type restp):
	    Type[] formals = infer.formalTypes(params, args.length);
	    if (formals.length != args.length) {
		error(pos, "wrong number of arguments" +
		      (meth == null ? "" : " for " + meth));
		return null;
	    }
	    if (tparams.length == 0) {
		for (int i = 0; i < args.length; i++) {
		    args[i] = transform(args[i], argMode, formals[i]);
		    argtypes[i] = args[i].type;
		}
	    } else {
		// targs: the type arguments inferred from the prototype
		Type[] targs = infer.protoTypeArgs(tparams, restp, pt, params);

		// argpts: prototypes for arguments
		Type[] argpts = new Type[formals.length];
		for (int i = 0; i < formals.length; i++)
		    argpts[i] = formals[i].subst(tparams, targs);

 		// transform arguments with [targs/tparams]formals as prototypes
		for (int i = 0; i < args.length; i++)
		    args[i] = transform(
			args[i], argMode | POLYmode, formals[i].subst(tparams, targs));

		// targs1: same as targs except that every AnyType is mapped to
		// formal parameter type.
		Type[] targs1 = new Type[targs.length];
		for (int i = 0; i < targs.length; i++)
		    targs1[i] = (targs[i] != Type.AnyType) ? targs[i]
			: tparams[i].type();

		for (int i = 0; i < args.length; i++) {
		    argtypes[i] = args[i].type;
		    switch (argtypes[i]) {
		    case PolyType(Symbol[] tparams1, Type restype1):
			argtypes[i] = infer.argumentTypeInstance(
			    tparams1, restype1,
			    formals[i].subst(tparams, targs1),
			    argpts[i]);
		    }
		}
	    }
	    return argtypes;

	case PolyType(Symbol[] tparams1, Type restp):
	    Symbol[] tparams2;
	    if (tparams.length == 0) tparams2 = tparams1;
	    else {
		tparams2 = new Symbol[tparams.length + tparams1.length];
		System.arraycopy(tparams, 0, tparams2, 0, tparams.length);
		System.arraycopy(tparams1, 0, tparams2, tparams.length, tparams1.length);
	    }
	    return transformArgs(pos, meth, tparams2, restp, argMode, args, pt);

	default:
	    for (int i = 0; i < args.length; i++) {
		args[i] = transform(args[i], argMode, Type.AnyType);
		argtypes[i] = args[i].type;
	    }
	    return argtypes;
	}
    }

    /** Atribute an expression or pattern with prototype `pt'.
     *  Check that expression's type conforms to `pt'.
     *  Resolve overloading and apply parameterless functions.
     *  Insert `apply' function if needed.
     */
    Tree transform(Tree tree, int mode, Type pt) {
	int savedMode = this.mode;
	Type savedPt = this.pt;
	this.mode = mode;
	this.pt = pt;
	Tree tree1 = adapt(transform(tree), mode, pt);

	//new TextTreePrinter().print(tree1).print(": " + tree1.type).println().end();//DEBUG

	this.mode = savedMode;
	this.pt = savedPt;
	return tree1;
    }

    Tree adapt(Tree tree, int mode, Type pt) {
	//new TextTreePrinter().print(tree).print(" adapt " + pt).println().end();//DEBUG
	switch (tree.type) {
	case OverloadedType(Symbol[] alts, Type[] alttypes):
	    // resolve overloading
	    if ((mode & FUNmode) == 0) {
		try {
		    infer.exprAlternative(tree, alts, alttypes, pt);
		} catch (Type.Error ex) {
		    error(tree.pos, ex.msg);
		}
		switch (tree.type) {
		case OverloadedType(_, _):
		    // overload resolution failed bcs no alternative matched prototype.
		    typeError(tree.pos, tree.type, pt);
		    tree.setSymbol(Symbol.ERROR).setType(Type.ErrorType);
		    break;
		default:
		    return adapt(tree, mode, pt);
		}
	    }
	    break;

	case PolyType(Symbol[] tparams, Type restp):
	    // apply parameterless functions
	    // instantiate polymorphic expressions
	    if (tparams.length == 0) {
		return adapt(tree.setType(restp), mode, pt);
	    } else if ((mode & (FUNmode | POLYmode)) == 0) {
		try {
		    tree = infer.exprInstance(tree, tparams, restp, pt);
		} catch (Type.Error ex) {
		    tree = error(tree.pos, ex.msg);
		}
		return adapt(tree, mode, pt);
//	    } else if ((mode & EXPRmode) != 0) {
//		// will be instantiated later
//		return tree;
	    }
	    break;

	case MethodType(_, _):
	    // convert unapplied methods to functions.
	    if ((mode & (EXPRmode | FUNmode)) == EXPRmode &&
		infer.isCompatible(tree.type, pt)) {
		checkEtaExpandable(tree.pos, tree.type);
		return transform(desugarize.etaExpand(tree, tree.type), mode, pt);
	    } else if ((mode & (CONSTRmode | FUNmode)) == CONSTRmode) {
		return error(tree.pos, "missing arguments for class constructor");
	    }
	}
	if ((mode & PATTERNmode) != 0) {
	    if (tree.isType()) {
		Symbol clazz = tree.type.unalias().symbol();
		if (clazz.isCaseClass()) {
		    // set type to instantiated case class constructor
		    tree.type = clazz.constructor().type();
		    switch (tree.type) {
		    case PolyType(Symbol[] tparams, Type restp):
			try {
			    infer.constructorInstance(tree, tparams, restp, pt);
			} catch (Type.Error ex) {
			    if (pt != Type.ErrorType) error(tree.pos, ex.msg);
			    return tree.setType(Type.ErrorType);
			}
			if (!(tree.type instanceof Type.MethodType))
			    tree = make.Apply(tree.pos, tree, Tree.EMPTY_ARRAY)
				.setType(tree.type);
		    }
		} else if (clazz.isSubClass(definitions.SEQ_CLASS)) {
		    // set type to instantiated sequence class constructor
		    Type seqtp = dropVarArgs(pt.baseType(clazz));
		    if (seqtp != Type.NoType) {
			tree.type = seqConstructorType(seqtp, pt);
		    } else {
			error(tree.pos, "expected pattern type " + pt +
			      " does not conform to sequence " + clazz);
		    }
		} else if (tree.type != Type.ErrorType) {
		    error(tree.pos, tree.type.symbol() +
			  " is neither a case class constructor nor a sequence class constructor");
		}
	    }
	    if ((mode & FUNmode) != 0) {
		return tree;
	    } else {
		Symbol sym = tree.symbol();
		// convert nullary case methods to types
		// check that other idents or selects are stable.
		switch (tree) {
		case Ident(Name name):
		    if (sym != null && isNullaryMethod(sym) && (sym.flags & CASE) != 0)
			return transform(
			    make.Apply(
				tree.pos,
				copy.Ident(tree, name.toTypeName()),
				Tree.EMPTY_ARRAY),
			    mode, pt);
		    else
			checkStable(tree);
		    break;
		case Select(Tree qual, Name name):
		    if (sym != null && isNullaryMethod(sym) && (sym.flags & CASE) != 0)
			return transform(
			    make.Apply(
				tree.pos,
				copy.Select(tree, qual, name.toTypeName()),
				Tree.EMPTY_ARRAY),
			    mode, pt);
		    else
			checkStable(tree);
		}
	    }
	} else if ((mode & EXPRmode) != 0) {
	    if ((mode & FUNmode) != 0) {
		if (tree.type.isObjectType()) {
		    // insert apply method
		    Symbol applyMeth = tree.type.lookup(Names.apply);
		    if (applyMeth != Symbol.NONE && isAccessible(applyMeth, tree)) {
			applyMeth.flags |= (ACCESSED | SELECTOR);
			tree = make.Select(tree.pos, tree, Names.apply)
			    .setSymbol(applyMeth)
			    .setType(tree.type.memberType(applyMeth));
			return adapt(tree, mode, pt);
		    }
		}
	    } else if ((mode & QUALmode) == 0) {
		// check that packages and static modules are not used as values
		Symbol sym = tree.symbol();
		if (sym != null && sym.kind != ERROR && !sym.isValue() &&
		    tree.isTerm()) {
		    error(tree.pos, tree.symbol() + " is not a value");
		}
	    }
	}

	Type owntype = tree.type;
	if ((mode & (CONSTRmode | FUNmode)) == (CONSTRmode))
	    owntype = owntype.instanceType();
	if (!(owntype instanceof Type.PolyType || owntype.isSubType(pt))) {
	    typeError(tree.pos, owntype, pt);
	    explainTypes(owntype, pt);
	    tree.type = Type.ErrorType;
	}
	return tree;
    }
    //where
	boolean isNullaryMethod(Symbol sym) {
	    switch (sym.type()) {
	    case PolyType(_, Type restpe):
		return !(restpe instanceof Type.MethodType);
	    default:
		return false;
	    }
	}

	Type dropVarArgs(Type tp) {
	    switch (tp) {
	    case TypeRef(Type pre, Symbol sym, Type[] targs):
		Type[] targs1 = targs;
		for (int i = 0; i < targs.length; i++) {
		    Type targ = targs[i];
		    Type targ1 = targ.dropVariance();
		    if (targ != targ1 && targs1 == targs) {
			targs1 = new Type[targs.length];
			System.arraycopy(targs, 0, targs1, 0, i);
		    }
		    targs1[i] = targ1;
		}
		if (targs1 == targs) return tp;
		else return Type.TypeRef(pre, sym, targs1);
	    default:
		return tp;
	    }
	}

	Type seqConstructorType(Type paramtp, Type resulttp) {
	    Symbol constr = resulttp.symbol().constructor();
	    Symbol param = new TermSymbol(
		Position.NOPOS, Names.WILDCARD, constr, PARAM | REPEATED).setInfo(
		    paramtp);
	    return Type.MethodType(new Symbol[]{param}, resulttp);
	}

    /** Transform expression or type with a given mode.
     */
    public Tree transform(Tree tree, int mode) {
	if ((mode & TYPEmode) == 0)
	    return transform(tree, mode, Type.AnyType);

	int savedMode = this.mode;
	this.mode = mode;
	Tree tree1 = transform(tree);
	this.mode = savedMode;

	Symbol sym = tree1.symbol();
	if ((mode & FUNmode) == 0 && sym != null && sym.typeParams().length != 0)
	    return error(tree.pos, sym + " takes type parameters.");
//	else if (tree1.isType())
//	    return gen.mkType(tree1.pos, tree1.type);
	else
	    return tree1;
    }

    Tree[] transform(Tree[] trees, int mode) {
	for (int i = 0; i < trees.length; i++)
	    trees[i] = transform(trees[i], mode);
	return trees;
    }

    /** The main attribution function
     */
    public Tree transform(Tree tree) {
	Symbol sym = tree.symbol();
	if (sym != null && !sym.isInitialized()) sym.initialize();
	if (global.debug && TreeInfo.isDefinition(tree)) global.log("transforming " + sym);
	try {
	    switch (tree) {

	    case Bad():
		return tree.setSymbol(Symbol.ERROR).setType(Type.ErrorType);

	    case Empty:
		tree.type = Type.NoType;
		return tree;

	    case PackageDef(Tree pkg, Tree.Template templ):
		switch (templ) {
		case Template(Tree[] parents, Tree[] body):
		    Symbol pkgSym = pkg.symbol();
		    if (pkgSym != null && pkgSym.isPackage()) {
			pushContext(templ, pkgSym, pkgSym.members());
			Tree[] body1 = transform(body);
			popContext();
			Tree.Template templ1 = copy.Template(templ, parents, body1);
			templ1.setType(Type.NoType).setSymbol(Symbol.NONE);
			return copy.PackageDef(tree, pkg, templ1)
			    .setType(definitions.UNIT_TYPE);
		    }
		}
		return tree.setType(Type.ErrorType);

	    case ClassDef(int mods, Name name, Tree.TypeDef[] tparams, Tree.ValDef[][] vparams, Tree tpe, Tree.Template templ):
		pushContext(tree, sym.constructor(), new Scope(context.scope));
		reenterParams(tparams);
		Tree.TypeDef[] tparams1 = transform(tparams);
		reenterParams(vparams);
		Tree.ValDef[][] vparams1 = transform(vparams);
		Tree tpe1 = transform(tpe);
		Tree.Template templ1 = transformTemplate(templ, sym);
		popContext();
		return copy.ClassDef(tree, mods, name, tparams1, vparams1, tpe1, templ1)
		    .setType(definitions.UNIT_TYPE);

	    case ModuleDef(int mods, Name name, Tree tpe, Tree.Template templ):
		Tree tpe1 = transform(tpe, TYPEmode);
		Tree.Template templ1 = transformTemplate(templ, sym.moduleClass());
		return copy.ModuleDef(tree, mods, name, tpe1, templ1)
		    .setType(definitions.UNIT_TYPE);

	    case ValDef(int mods, Name name, Tree tpe, Tree rhs):
		Tree tpe1 = transform(tpe, TYPEmode);
		Tree rhs1 = rhs;
		if (tpe1 == Tree.Empty) {
		    tpe1 = gen.mkType(rhs.pos, rhs.type);
		    // rhs already attributed by defineSym in this case
		} else if (rhs != Tree.Empty) {
		    if ((mods & CASEACCESSOR) != 0) {
			//rhs was already attribute
		    } else {
			pushContext(tree, sym, context.scope);
			rhs1 = transform(rhs, EXPRmode, sym.type());
			popContext();
		    }
		}
		return copy.ValDef(tree, mods, name, tpe1, rhs1)
		    .setType(definitions.UNIT_TYPE);

	    case DefDef(int mods, Name name, Tree.TypeDef[] tparams, Tree.ValDef[][] vparams, Tree tpe, Tree rhs):
		pushContext(tree, sym, new Scope(context.scope));
		reenterParams(tparams);
		Tree.TypeDef[] tparams1 = transform(tparams);
		reenterParams(vparams);
		Tree.ValDef[][] vparams1 = transform(vparams);
		Tree tpe1 = transform(tpe, TYPEmode);
		Tree rhs1 = rhs;
		if (tpe1 == Tree.Empty) {
		    tpe1 = gen.mkType(rhs1.pos, rhs1.type);
		    //System.out.println("infer " + sym + ":" + rhs.type);//DEBUG
		    //rhs already attributed by defineSym in this case
		} else if (rhs != Tree.Empty) {
		    rhs1 = transform(rhs, EXPRmode,
			tpe1.type == Type.NoType ? Type.AnyType : tpe1.type);
		}
		popContext();
		return copy.DefDef(tree, mods, name, tparams1, vparams1, tpe1, rhs1)
		    .setType(definitions.UNIT_TYPE);

	    case TypeDef(int mods, Name name, Tree rhs):
		pushContext(tree, sym, new Scope(context.scope));
		int mode = TYPEmode;
		if (sym.kind == ALIAS) mode |= FUNmode;
		Tree rhs1 = transform(rhs, mode);
		popContext();
		return copy.TypeDef(tree, mods, name, rhs1)
		    .setType(definitions.UNIT_TYPE);

	    case Import(Tree expr, Name[] selectors):
		context.imports = new ImportList(tree, context.scope, context.imports);
		return Tree.Empty;

	    case Block(Tree[] stats):
		pushContext(tree, context.owner, new Scope(context.scope));
		int lastmode = mode & ~FUNmode;
		Tree[] stats1 = desugarize.Statements(stats, true);
		enterSyms(stats1);
		context.imports = context.outer.imports;
		for (int i = 0; i < stats1.length - 1; i++)
		    stats1[i] = transform(stats1[i], EXPRmode);
		Type tp;
		if (stats1.length > 0) {
		    stats1[stats1.length - 1] =
			transform(stats1[stats1.length - 1], lastmode, pt);
		    tp = checkNoEscape(tree.pos, stats1[stats1.length - 1].type);
		} else {
		    tp = definitions.UNIT_TYPE;
		}
		popContext();
		return copy.Block(tree, stats1)
		    .setType(tp);

	    case Visitor(Tree.CaseDef[] cases):
		if (pt.symbol().isSubClass(definitions.PARTIALFUNCTION_CLASS)) {
		    Type pft = pt.baseType(definitions.PARTIALFUNCTION_CLASS);
		    Type[] pftargs = pft.typeArgs();
		    if (pft.typeArgs().length == 2 && infer.isFullyDefined(pftargs[0])) {
			Type pattpe = pftargs[0];
			Type restpe = pftargs[1];
			Tree tree1 = transformVisitor(tree, pattpe, restpe);
			if (!infer.isFullyDefined(restpe)) restpe = tree1.type;
			return transform(
			    desugarize.partialFunction(
				tree, pattpe, restpe.dropVariance()));
		    } else {
			return error(tree.pos, "expected pattern type of cases could not be determined");
		    }
		} else {
		    return transform(desugarize.Visitor(tree));
		}

	    case Assign(Apply(Tree funarray, Tree[] vparam), Tree rhs):
		return transform(desugarize.Update(tree));

	    case Assign(Tree lhs, Tree rhs):
		Tree lhs1 = transform(lhs, EXPRmode);
		Symbol varsym = lhs1.symbol();
		if (varsym != null && (varsym.flags & ACCESSOR) != 0) {
		    return transform(desugarize.Assign(tree.pos, lhs, rhs));
		} else if (varsym == null || (varsym.flags & MUTABLE) == 0) {
		    return error(tree.pos, "assignment to non-variable");
		} else {
		    Tree rhs1 = transform(rhs, EXPRmode, lhs1.type);
		    return copy.Assign(tree, lhs1, rhs1)
			.setType(definitions.UNIT_TYPE);
		}

	    case If(Tree cond, Tree thenp, Tree elsep):
		Tree cond1 = transform(cond, EXPRmode, definitions.BOOLEAN_TYPE);
		if (elsep == Tree.Empty) {
		    Tree thenp1 =
			transform(thenp, EXPRmode, definitions.UNIT_TYPE);
		    Tree elsep1 = make.Block(tree.pos, Tree.EMPTY_ARRAY)
			.setType(definitions.UNIT_TYPE);
		    return copy.If(tree, cond1, thenp1, elsep1)
			.setType(definitions.UNIT_TYPE);
		} else {
		    Tree thenp1 = transform(thenp, EXPRmode, pt);
		    Tree elsep1 = transform(elsep, EXPRmode, pt);
		    return copy.If(tree, cond1, thenp1, elsep1)
			.setType(Type.lub(new Type[]{thenp1.type, elsep1.type}));
		}

	    case New(Tree.Template templ):
		switch (templ) {
	        case Template(Tree[] parents, Tree[] body):
		    if (parents.length == 1 && body.length == 0) {
			Tree parent1 = transform(parents[0], CONSTRmode, pt);
			Tree.Template templ1 = (Tree.Template)
			    copy.Template(templ, new Tree[]{parent1}, body)
			    .setType(parent1.type).setSymbol(Symbol.NONE);
			Type owntype = parent1.type;
			checkInstantiatable(tree.pos, owntype);
			return copy.New(tree, templ1)
			    .setType(owntype.instanceType());
		    } else {
			pushContext(tree, context.owner, new Scope(context.scope));
			Tree cd = make.ClassDef(
			    templ.pos,
			    0,
			    Names.ANON_CLASS_NAME.toTypeName(),
			    Tree.ExtTypeDef.EMPTY_ARRAY,
			    new ValDef[][]{Tree.ExtValDef.EMPTY_ARRAY},
			    Tree.Empty,
			    templ);
			//new TextTreePrinter().print(cd).println().end();//DEBUG
			enterSym(cd);
			cd = transform(cd);
			Symbol clazz = cd.symbol();
			if (clazz.kind != CLASS) return errorTree(tree.pos);

			// compute template's type with new refinement scope.
			Type[] parentTypes = clazz.info().parents();
			Scope refinement = new Scope();
			Type base = Type.compoundType(parentTypes, Scope.EMPTY);
			Type tp = Type.compoundType(
			    parentTypes, refinement, clazz);
			Scope.SymbolIterator it = clazz.members().iterator();
			while (it.hasNext()) {
			    Symbol sym1 = it.next();
			    Symbol basesym1 = base.lookupNonPrivate(sym1.name);
			    if (basesym1.kind != NONE &&
				!base.symbol().thisType().memberType(basesym1)
				    .isSameAs(sym1.type()))
				refinement.enter(sym1);
			}
			if (refinement.elems == Scope.Entry.NONE &&
			    parentTypes.length == 1)
			    tp = parentTypes[0];
			else
			    tp = checkNoEscape(tree.pos, tp);

			Tree alloc =
			    gen.Typed(
				gen.New(
				    gen.Apply(
					gen.mkRef(tree.pos,
						  Type.localThisType, clazz.constructor()),
					Tree.EMPTY_ARRAY)),
				tp);
			popContext();
			return make.Block(tree.pos, new Tree[]{cd, alloc})
			    .setType(tp);
		    }
		default:
		    throw new ApplicationError();
		}

	    case Typed(Tree expr, Tree tpe):
		Tree tpe1 = transform(tpe, TYPEmode);
		Tree expr1 = transform(expr, mode & baseModes, tpe1.type);
		return copy.Typed(tree, expr1, tpe1)
		    .setType(tpe1.type);

	    case Function(Tree.ValDef[] vparams, Tree body):
		pushContext(tree, context.owner, new Scope(context.scope));
		Type restype = desugarize.preFunction(vparams, pt);
		enterParams(vparams);
		Tree body1 = transform(body, EXPRmode, restype);
		if (!infer.isFullyDefined(restype)) restype = body1.type;
		popContext();
		Tree tree1 = copy.Function(tree, vparams, body1);
		Tree tree2 = transform(desugarize.Function(tree1, restype));
		return desugarize.postFunction(tree2);

	    case TypeApply(Tree fn, Tree[] args):
		Tree fn1 = transform(fn, EXPRmode | FUNmode, Type.AnyType);
		Tree[] args1 = transform(args, TYPEmode);
		Type[] argtypes = Tree.typeOf(args1);

		// resolve overloading
		switch (fn1.type) {
		case OverloadedType(Symbol[] alts, Type[] alttypes):
		    try {
			infer.polyAlternative(fn1, alts, alttypes, args.length);
		    } catch (Type.Error ex) {
			error(tree.pos, ex.msg);
		    }
		}

		// match against arguments
		switch (fn1.type) {
		case PolyType(Symbol[] tparams, Type restp):
		    if (tparams.length == argtypes.length) {
			int i = 0;
			while (i < tparams.length &&
			       (context.delayArgs ||
				argtypes[i].isSubType(
				    tparams[i].info().subst(tparams, argtypes))))
			    i++;
			if (i == tparams.length) {
			    return copy.TypeApply(tree, fn1, args1)
				.setType(restp.subst(tparams, argtypes));
			}
		    }
		    break;
		case ErrorType:
		    return tree.setType(Type.ErrorType);
		}
		return error(tree.pos,
		    infer.toString(fn1.symbol(), fn1.type) +
		    " cannot be applied to " +
		    ArrayApply.toString(argtypes, "[", ",", "]"));

	    case Apply(Tree fn, Tree[] args):
		return transformApply(tree, fn, args);

	    case Super(Tree tpe):
		Symbol enclClazz = context.enclClass.owner;
		if (enclClazz != null) {
                    // we are in a class or module
		    Tree tpe1 = transform(tpe, TYPEmode); // ignored for now.
		    switch (enclClazz.info()) {
		    case CompoundType(Type[] parents, _):
			return copy.Super(tree, tpe1)
			    .setType(Type.compoundType(parents, Scope.EMPTY).symbol().thisType());
		    case ErrorType:
			return tree.setType(Type.ErrorType);
		    default:
			throw new ApplicationError();
		    }
		} else {
		    return error(tree.pos,
                        "super can be used only in a class, object, or template");
		}

	    case This(Tree qual):
		Symbol clazz;
		Tree tree1;
		if (qual == Tree.Empty) {
		    clazz = context.enclClass.owner;
		    if (clazz != null) {
			tree1 = gen.mkStableId(tree.pos, clazz.thisType());
		    } else {
			return error(
			    tree.pos, tree +
			    " can be used only in a class, object, or template");
		    }
		} else {
		    Tree qual1 = transform(qual, TYPEmode | FUNmode);
		    clazz = qual1.type.symbol();
		    if (clazz.kind == CLASS) {
			Context clazzContext = context.outerContext(clazz);
			if (clazzContext != Context.NONE) {
			    if (!(qual1 instanceof Tree.Ident))
				qual1 = gen.Ident(tree.pos, clazz);
			    tree1 = copy.This(tree, qual1);
			} else {
			    return error(qual.pos,
				clazz.name + " is not an enclosing class");
			}
		    } else {
			return error(qual.pos, "class identifier expected");
		    }
		}
		return tree1.setType(
		    (pt != null && pt.isStable() || (mode & QUALmode) != 0)
		    ? clazz.thisType() : clazz.typeOfThis());

	    case Select(Tree qual, Name name):
		int qualmode = EXPRmode | POLYmode | QUALmode;
		Tree qual1 = transform(qual, qualmode);
		if (name.isTypeName()) qual1 = checkStable(qual1);
		return transformSelect(
		    tree, adapt(qual1, qualmode, Type.AnyType), name);

	    case Ident(Name name):
		if (mode  == PATTERNmode && name.isVariable()) {
		    //System.out.println("pat var " + name + ":" + pt);//DEBUG
		    Symbol vble = new TermSymbol(
			tree.pos, name, context.owner, 0).setType(pt);
		    if (name != Names.WILDCARD) enterInScope(vble);
		    return tree.setSymbol(vble).setType(pt);
		} else {
		    return transformIdent(tree, name);
		}

	    case Literal(Object value):
		return tree.setType(value2Type(value));

	    case TypeTerm():
		return tree;

	    case SingletonType(Tree ref):
		Tree ref1 = transform(ref, EXPRmode | QUALmode, Type.AnyType);
		return make.TypeTerm(tree.pos)
		    .setType(checkObjectType(tree.pos, ref1.type.resultType()));

	    case SelectFromType(Tree qual, Name name):
		Tree qual1 = transform(qual, TYPEmode);
		return transformSelect(tree, qual1, name);

	    case CompoundType(Tree[] parents, Tree[] refinements):
		Tree[] parents1 = transform(parents, TYPEmode);
		Type[] ptypes = Tree.typeOf(parents);
		Scope members = new Scope();
		Type self = Type.compoundType(ptypes, members);
		Symbol clazz = self.symbol();
		validateBaseTypes(clazz);
		pushContext(tree, clazz, members);
		for (int i = 0; i < refinements.length; i++) {
		    enterSym(refinements[i]).flags |= OVERRIDE;
		}
		Tree[] refinements1 = transformStatSeq(refinements, Symbol.NONE);
		checkAllOverrides(clazz);
		popContext();
		return make.TypeTerm(tree.pos)
		    .setType(self);

	    case AppliedType(Tree tpe, Tree[] args):
		Tree tpe1 = transform(tpe, TYPEmode | FUNmode);
		Tree[] args1 = transform(args, TYPEmode);
		Type[] argtypes = Tree.typeOf(args);
		//todo: this needs to be refined.
		Symbol[] tparams =
		    (Type.isSameAs(tpe1.type.typeArgs(), Symbol.type(tpe1.type.typeParams())))
		    ? tpe1.type.typeParams()
		    : Symbol.EMPTY_ARRAY;
		Type owntype = Type.ErrorType;
		if (tpe1.type != Type.ErrorType) {
		    if (tparams.length == args.length) {
			try {
			    if (!context.delayArgs)
				infer.checkBounds(tparams, argtypes, "");
			    owntype = Type.appliedType(tpe1.type, argtypes);
			} catch (Type.Error ex) {
			    error(tree.pos, ex.msg);
			}
		    } else {
			if (tparams.length == 0)
			    error(tree.pos, tpe1.type +
				  " does not take type parameters");
			else
			    error(tree.pos,
				  "wrong number of type arguments for " +
				  tpe1.type);
		    }
		}
		return make.TypeTerm(tree.pos).setType(owntype);

	    case CovariantType(Tree tpe):
		Tree tpe1 = transform(tpe, TYPEmode);
		return make.TypeTerm(tree.pos)
		    .setType(Type.covarType(tpe1.type));

	    case FunType(_, _):
		return transform(desugarize.FunType(tree));

	    default:
		throw new ApplicationError("illegal tree: " + tree);
	    }
	} catch (Type.Error ex) {
	    reportTypeError(tree.pos, ex);
	    tree.type = Type.ErrorType;
            if (tree.hasSymbol() && tree.symbol() == null) tree.setSymbol(Symbol.ERROR);
	    return tree;
	}
    }

// Contexts -------------------------------------------------------------------

    /** Push new context associated with given tree, owner, and scope on stack.
     *  Fields `imports' and, possibly, `enclClass' are inherited from parent.
     */
    void pushContext(Tree tree, Symbol owner, Scope scope) {
	context = new Context(tree, owner, scope, context);
    }

    /** Pop context from stack.
     */
    void popContext() {
	context = context.outer;
    }

// Lazy Types ------------------------------------------------------------------

    /** A lazy type which, when forced returns the type of a symbol defined
     *  in `tree'.
     */
    class LazyTreeType extends Type.LazyType {
	Tree tree;
        Unit u;
	Context c;

	LazyTreeType(Tree tree) {
	    this.tree = tree;
            this.u = unit;
	    this.c = context;
	}
	public void complete(Symbol sym) {
	    //System.out.println("completing " + sym);//DEBUG
	    //if (sym.isConstructor()) sym.constructorClass().initialize();
	    //else if (sym.isModule()) sym.moduleClass().initialize();
	    defineSym(tree, u, c);
	}
    }

    /** A lazy type for case constructor methods (whose name is a term name)
     *  which sets the method's type to the class constructor type.
     */
    class LazyConstrMethodType extends LazyTreeType {
	LazyConstrMethodType(Tree tree) {
	    super(tree);
	}
	public void complete(Symbol sym) {
	    Type constrtype = tree.symbol().constructor().type().instanceType();
	    switch (tree) {
	    case ClassDef(_, _, _, ValDef[][] vparams, _, _):
		if (vparams.length == 0) {
		    constrtype = removeMethod(constrtype);
		}
	    }
	    sym.setInfo(constrtype);
	}
	private Type removeMethod(Type tp) {
	    switch (tp) {
	    case MethodType(_, Type restp):
		return restp;
	    case PolyType(Symbol[] tparams, Type restp):
		return Type.PolyType(tparams, removeMethod(restp));
	    default:
		return tp;
	    }
	}
    }

    /** A lazy type for self types
     */
    class LazySelfType extends LazyTreeType {
	LazySelfType(Tree tree) {
	    super(tree);
	}
	public void complete(Symbol sym) {
	    defineSelfType(sym, tree, u, c);
	}
    }
}

