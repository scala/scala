/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**
** $Id$
\*                                                                      */

//todo check significance of JAVA flag.

package scalac.symtab;

import scalac.ApplicationError;
import scalac.Global;
import scalac.PhaseDescriptor;
import scalac.util.ArrayApply;
import scalac.util.Name;
import scalac.util.Names;
import scalac.util.NameTransformer;
import scalac.util.Position;
import scalac.util.Debug;
import scalac.symtab.classfile.*;


public abstract class Symbol implements Modifiers, Kinds {

    /** An empty symbol array */
    public static final Symbol[] EMPTY_ARRAY = new Symbol[0];

    /** An empty array of symbol arrays */
    public static final Symbol[][] EMPTY_ARRAY_ARRAY = new Symbol[0][];

    /** The error symbol */
    public static final ErrorSymbol ERROR = new ErrorSymbol();

    /** The absent symbol */
    public static final NoSymbol NONE = new NoSymbol();

// Fields -------------------------------------------------------------

    /** The kind of the symbol */
    public int kind;

    /** The position of the symbol */
    public int pos;

    /** The name of the symbol */
    public Name name;

    /** The modifiers of the symbol */
    public int flags;

    /** The owner of the symbol */
    private Symbol owner;

    /** The infos of the symbol */
    private TypeIntervalList infos = TypeIntervalList.EMPTY;

// Constructors -----------------------------------------------------------

    /** Generic symbol constructor */
    public Symbol(int kind, int pos, Name name, Symbol owner, int flags) {
	assert (!isTerm() || !name.isTypeName()) && (!isType() || name.isTypeName());

        this.kind = kind;
        this.pos = pos;
        this.name = name;
        this.owner = owner;
        this.flags = flags & ~(INITIALIZED | LOCKED); // safety first
    }

    /** Return a fresh symbol with the same fields as this one.
     */
    public abstract Symbol cloneSymbol();

    /** copy all fields to `sym'
     */
    public void copyTo(Symbol sym) {
	sym.kind = kind;
	sym.pos = pos;
	sym.name = name;
	sym.flags = flags;
	sym.owner = owner;
	sym.infos = infos;
    }

// Setters ---------------------------------------------------------------

    /** Set the mangled name of this Symbol */
    public Symbol setMangledName(Name name) {
        throw new ApplicationError("illegal operation on " + getClass());
    }

    /** Set owner */
    public Symbol setOwner(Symbol owner) {
        this.owner = owner;
        return this;
    }

    /** Set information, except if symbol is both initialized and locked.
     */
    public Symbol setInfo(Type info) {
	return setInfo(info, currentPhaseId());
    }

    public Symbol setInfo(Type info, int limit) {
	if ((flags & (INITIALIZED | LOCKED)) != (INITIALIZED | LOCKED)) {
	    if (infos == TypeIntervalList.EMPTY)
		infos = new TypeIntervalList(TypeIntervalList.EMPTY);
	    infos.limit = limit;
	    infos.info = info;
	}
        return this;
    }

    /** Set type -- this is an alias for setInfo(Type info) */
    public Symbol setType(Type info) { return setInfo(info); }

    public Symbol updateInfo(Type info) {
	// Global.instance.currentPhase.setInfo(this, info);
	if (infos.limit <= Global.instance.currentPhase.id) {
	    infos = new TypeIntervalList(infos);
	    infos.limit = Global.instance.currentPhase.id + 1;
	} else {
	    assert infos.limit == Global.instance.currentPhase.id + 1 : this;
	}
	infos.info = info;
	return this;
    }

// Symbol classification ----------------------------------------------------

    /** Does this symbol denote a type? */
    public final boolean isType() {
	return kind == TYPE || kind == CLASS || kind == ALIAS;
    }

    /** Does this symbol denote a term? */
    public final boolean isTerm() {
	return kind == VAL;
    }

    /** Does this symbol denote a value? */
    public final boolean isValue() {
        return kind == VAL && !(isModule() && isJava()) && !isPackage();
    }

    /** Does this symbol denote a variable? */
    public final boolean isVariable() {
        return kind == VAL && (flags & MUTABLE) != 0;
    }

    /** Does this symbol denote a method?
     */
    public final boolean isInitializedMethod() {
	if (infos.limit < 0) return false;
	switch (rawInfo()) {
	case MethodType(_, _):
	case PolyType(_, _): return true;
	default: return false;
	}
    }

    public final boolean isMethod() {
	initialize();
	return isInitializedMethod();
    }

    /* Does this symbol denote an anonymous class? */
    public final boolean isAnonymousClass() {
	return kind == CLASS &&
	    (name == Names.EMPTY.toTypeName() ||
	     name == Names.ANON_CLASS_NAME.toTypeName());
    }

    /** Does this symbol denote the root class or root module?
     */
    public final boolean isRoot() {
	return this.moduleClass() == Global.instance.definitions.ROOT_CLASS;
    }

    /** Does this symbol denote something loaded from a Java class? */
    public final boolean isJava() {
        return (flags & JAVA) != 0;
    }

    /** Does this symbol denote a Java package? */
    public final boolean isPackage() {
        return (flags & PACKAGE) != 0;
    }

    /** Does this symbol denote a module? */
    public final boolean isModule() {
        return kind == VAL && (flags & MODUL) != 0;
    }

    /** Does this symbol denote a module? */
    public final boolean isModuleClass() {
        return kind == CLASS && (flags & MODUL) != 0;
    }

    /** Does this symbol denote a module? */
    public final boolean isClass() {
        return kind == CLASS;
    }

    /** Does this symbol denote a case class?
     */
    public final boolean isCaseClass() {
	return kind == CLASS && (flags & CASE) != 0;
    }

    /** Does this symbol denote a uniform (i.e. parameterless) class? */
    public final boolean isTrait() {
	return kind == CLASS && (flags & TRAIT) != 0;
    }

    /** Does this class symbol denote a compound type symbol?
     */
    public final boolean isCompoundSym() {
	return name == Names.COMPOUND_NAME.toTypeName();
    }

    /** Does this symbol denote an interface? */
    public final boolean isInterface() {
        return (flags & INTERFACE) != 0;
    }

    /** Does this symbol denote a static member? */
    public final boolean isStatic() {
        return (flags & STATIC) != 0;
    }

    /** Is this symbol locally defined? I.e. not a member of a class or module */
    public final boolean isLocal() {
	return owner.kind == VAL && !owner.isPrimaryConstructor();
    }

    /** Is this symbol a parameter? Includes type parameters of methods.
     */
    public final boolean isParameter() {
	return (flags & PARAM) != 0;
    }

    /** Is this symbol a def parameter?
     */
    public final boolean isDefParameter() {
	return (flags & (PARAM | DEF)) == (PARAM | DEF);
    }

    /** Is this class locally defined?
     *  A class is local, if
     *   - it is anonymous, or
     *   - its owner is a value
     *   - it is defined within a local class
     */
    public final boolean isLocalClass() {
	return kind == CLASS &&
	    !isPackage() &&
	    (name == Names.EMPTY.toTypeName() ||
	     owner.isValue() ||
	     owner.isLocalClass());
    }

    /** Is this symbol a constructor? */
    public final boolean isConstructor() {
	return name.isConstrName();
    }

    /** Is this symbol the primary constructor of a type? */
    public final boolean isPrimaryConstructor() {
	return isConstructor() && this == constructorClass().constructor();
    }

    public boolean isGenerated() {
	return name.pos((byte)'$') < name.length();
    }

    /** Symbol was preloaded from package
     */
    public boolean isPreloaded() {
	return owner.isPackage() && pos == Position.NOPOS;
    }

// Symbol names ----------------------------------------------------------------

    /** Get the fully qualified name of this Symbol
     *  (this is always a normal name, never a type name)
     */
    public Name fullName() {
        return name.toTermName();
    }

    /** Get the mangled name of this Symbol
     *  (this is always a normal name, never a type name)
     */
    public Name mangledName() {
        return name.toTermName();
    }

    /** Get the fully qualified mangled name of this Symbol */
    public Name mangledFullName() {
        return fullName().replace((byte)'.', (byte)'$');
    }

// Acess to related symbols -----------------------------------------------------

    /** Get type parameters */
    public Symbol[] typeParams() {
	return EMPTY_ARRAY;
    }

    /** Get primary constructor of class */
    public Symbol constructor() {
        return NONE;
    }

    /** Get module associated with class */
    public Symbol module() {
        return NONE;
    }

    /** Get owner */
    public Symbol owner() {
        return owner;
    }

    /** Get owner, but if owner is primary constructor of a class,
     *  get class symbol instead. This is useful for type parameters
     *  and value parameters in classes which have the primary constructor
     *  as owner.
     */
    public Symbol classOwner() {
	Symbol owner = owner();
	Symbol clazz = owner.constructorClass();
	if (clazz.constructor() == owner) return clazz;
	else return owner;
    }

    /** The next enclosing class */
    public Symbol enclClass() {
        return owner().enclClass();
    }

    /** The top-level class enclosing `sym'
     */
    Symbol enclToplevelClass() {
	Symbol sym = this;
	while (sym.kind == VAL ||
	       (sym.kind == CLASS && !sym.owner().isPackage())) {
	    sym = sym.owner();
	}
	return sym;
    }

     /* If this is a constructor, return the class it constructs.
     *  Otherwise return the symbol itself.
     */
    public Symbol constructorClass() {
	return this;
    }

     /* If this is a module, return its class.
     *  Otherwise return the symbol itself.
     */
    public Symbol moduleClass() {
	return this;
    }

    /** The symbol accessed by this accessor function.
     */
    public Symbol accessed() {
	assert (flags & ACCESSOR) != 0;
	Name name1 = name;
	if (name1.endsWith(Names._EQ))
	    name1 = name1.subName(0, name1.length() - Names._EQ.length());
	return owner.info().lookup(Name.fromString(name1 + "$"));
    }

    /** The members of this class or module symbol
     */
    public Scope members() {
	return info().members();
    }

// Symbol types --------------------------------------------------------------

    /** Was symbol's type updated during phase `id'?
     */
    public boolean isUpdated(int id) {
	return infos.limit >= id;
    }

    /** the current phase id, or the id after analysis, whichever is larger.
     */
    int currentPhaseId() {
	int id = Global.instance.currentPhase.id;
	if (id > Global.instance.POST_ANALYZER_PHASE_ID)
	    id = Global.instance.POST_ANALYZER_PHASE_ID;
	return id;
    }

    /** Is this symbol initialized? */
    public final boolean isInitialized() {
        return (flags & INITIALIZED) != 0;
    }

    /** Initialize the symbol */
    public final Symbol initialize() {
	info();
        return this;
    }

    /** Get info; This is:
     *  for a term symbol, its type
     *  for a type variable, its bound
     *  for a type alias, its right-hand side
     *  for a class symbol, the compound type consisting of
     *  its baseclasses and members.
     */
    public Type info() {
	if ((flags & INITIALIZED) == 0) {
	    int id = currentPhaseId();
	    Type info = rawInfoAt(id);
	    assert info != null : this;

	    if ((flags & LOCKED) != 0) {
	        setInfo(Type.ErrorType);
		flags |= INITIALIZED;
		throw new CyclicReference(this, info);
	    }
	    flags |= LOCKED;
	    //System.out.println("completing " + this);//DEBUG
	    info.complete(this);
            flags = flags & ~LOCKED;
	    if (info instanceof SourceCompleter && (flags & SNDTIME) == 0) {
		flags |= SNDTIME;
		return info();
	    } else {
		assert !(rawInfoAt(id) instanceof Type.LazyType) : this;
		flags |= INITIALIZED;
	    }
	    //System.out.println("done: " + this);//DEBUG
	}
	return rawInfoAt(Global.instance.currentPhase.id);
    }

    /** Get info at phase #id
     */
    public Type infoAt(int id) {
	info();
	return rawInfoAt(id);
    }

    /** get info at phase #id, without forcing lazy types.
     */
    private Type rawInfoAt(int id) {
	int nextid = infos.limit;
	assert infos != TypeIntervalList.EMPTY : this;
	if (nextid < id) {
	    do {
		Type newInfo =
		    Global.instance.phases[nextid].transformInfo(this, infos.info);
		if (newInfo != infos.info) {
		    infos = new TypeIntervalList(infos);
		    infos.info = newInfo;
		}
		nextid++;
		infos.limit = nextid;
	    } while (nextid < id);
	    return infos.info;
	} else {
	    TypeIntervalList infos1 = infos;
	    while (infos1.prev.limit >= id) {
		infos1 = infos1.prev;
	    }
	    return infos1.info;
	}
    }

    public Type rawInfo() {
	return rawInfoAt(Global.instance.currentPhase.id);
    }

    /** The type of a symbol is:
     *  for a type symbol, the type corresponding to the symbol itself
     *  for a term symbol, its usual type
     */
    public Type type() {
	return info();
    }

    /** The type at phase #id
     */
    public Type typeAt(int id) {
	return infoAt(id);
    }

    /** The types of these symbols as an array.
     */
    static public Type[] type(Symbol[] syms) {
	Type[] tps = new Type[syms.length];
	for (int i = 0; i < syms.length; i++)
	    tps[i] = syms[i].type();
	return tps;
    }

    /** Get this.type corresponding to this symbol
     */
    public Type thisType() {
	return Type.localThisType;
    }

    public Type typeOfThis() {
	return Type.localThisType;
    }

    /** A total ordering between symbols that refines the class
     *  inheritance graph (i.e. subclass.isLess(superclass) always holds).
     */
    public boolean isLess(Symbol that) {
	if (this == that) return false;
	int diff;
	if (this.isType()) {
	    if (that.isType()) {
		diff = this.closure().length - that.closure().length;
		if (diff > 0) return true;
		if (diff < 0) return false;
	    } else {
		return true;
	    }
	} else if (that.isType()) {
	    return false;
	}

	diff = that.mangledName().index - this.mangledName().index;
	if (diff > 0) return true;
	if (diff < 0) return false;

	diff = that.mangledFullName().index - this.mangledFullName().index;
	if (diff > 0) return true;
	if (diff < 0) return false;

	throw new ApplicationError(
	    "Giving up: can't order two incarnations of class " +
	    this.mangledFullName());
    }

    /** Return the symbol's type itself followed by all its direct and indirect
     *  base types, sorted by isLess(). Overridden for class symbols.
     */
    public Type[] closure() {
	return info().closure();
    }

    /** Return position of `c' in the closure of this type; -1 if not there.
     */
    public int closurePos(Symbol c) {
	if (this == c) return 0;
	if (c.isCompoundSym()) return -1;
	Type[] closure = closure();
	int lo = 0;
	int hi = closure.length - 1;
	while (lo <= hi) {
	    int mid = (lo + hi) / 2;
	    Symbol clsym = closure[mid].symbol();
	    if (c == clsym) return mid;
	    else if (c.isLess(clsym)) hi = mid - 1;
	    else if (clsym.isLess(c)) lo = mid + 1;
	    else throw new ApplicationError();
	}
	return -1;
    }

    Type baseType(Symbol sym) {
	int i = closurePos(sym);
	if (i >= 0) return closure()[i];
	else return Type.NoType;
    }

    /** Is this class a subclass of `c'? I.e. does it have a type instance
     *  of `c' as indirect base class?
     */
    public boolean isSubClass(Symbol c) {
	return this == c || c.kind == Kinds.ERROR || closurePos(c) >= 0;
    }

// ToString -------------------------------------------------------------------

    /** String representation of symbol's simple name.
     *  Translates expansions of operators back to operator symbol. E.g.
     *  $eq => =.
     */
    public String nameString() {
	return NameTransformer.decode(name).toString();
    }

    /** String representation of symbol's full name.
     *  Translates expansions of operators back to operator symbol. E.g.
     *  $eq => =.
     */
    public String fullNameString() {
	return NameTransformer.decode(fullName()).toString();
    }

    public String idString() {
	if (Global.instance.uniqid &&
	    (kind == TYPE || Global.instance.debug))
	    return "#" + Global.instance.uniqueID.id(this);
	else return "";
    }

    /** String representation, including symbol's kind
     *  e.g., "class Foo", "function Bar".
     */
    public String toString() {
	if (isRoot()) return "<root package>";
	if (isAnonymousClass()) return "<template>";
	String kstr = kindString();
	String str;
	if (kstr.length() == 0) str = fullNameString();
	else str = kstr + " " + fullNameString();
	return str + idString();
    }

    /** String representation of location.
     */
    public String locationString() {
	if (owner.kind == CLASS && !owner.isAnonymousClass())
	    return " in " + owner;
	else
	    return "";
    }

    /** String representation of definition.
     */
    public String defString() {
	String inner;
	if (kind == CLASS) inner = " extends ";
	else if (kind == TYPE) inner = " <: ";
	else if (kind == ALIAS) inner = " = ";
	else inner = " : ";
	return
	    (isParameter() ? "" : defKeyword() + " ") +
	    nameString() + idString() + inner +
            (rawInfoAt(Global.instance.POST_ANALYZER_PHASE_ID)
	       instanceof Type.LazyType ? "?" : info());
    }

    public static String[] defString(Symbol[] defs) {
	String[] strs = new String[defs.length];
	for (int i = 0; i < defs.length; i++)
	    strs[i] = defs[i].defString();
	return strs;
    }

    /** String representation of kind */
    public String kindString() {
	switch (kind) {
        case CLASS:
	    if ((flags & TRAIT) != 0)
		return "trait";
	    else if ((flags & MODUL) != 0 && Global.instance.debug)
		return "module class";
	    else
		return "class";
	case TYPE:
        case ALIAS:
	    return "type";
        case VAL:
	    if (isVariable()) return "variable";
	    else if (isModule()) return "module";
	    else if (isConstructor()) return "constructor";
	    else if (isInitializedMethod()) return "method";
	    else return "value";
	default: return "";
	}
    }

    /** Definition keyword of kind
     */
    public String defKeyword() {
	switch (kind) {
        case CLASS: if ((flags & TRAIT) != 0) return "trait"; else return "class";
	case TYPE:
        case ALIAS: return "type";
        case VAL:
	    if (isVariable()) return "var";
	    else if (isModule()) return "module";
	    else if (isInitializedMethod()) return "def";
	    else return "val";
	default: return "";
	}
    }

// Overloading and Overriding -------------------------------------------

    /** Add another overloaded alternative to this symbol.
     */
    public Symbol overloadWith(Symbol that) {
        assert isTerm() : this;
	assert this.name == that.name : this + " " + that;
	assert this.owner == that.owner : this + " " + that;
	assert (this.flags & that.flags & JAVA) != 0 ||
	    (this.flags & (SOURCEFLAGS | JAVA) & ~ACCESSFLAGS) ==
	    (that.flags & (SOURCEFLAGS | JAVA) & ~ACCESSFLAGS) : this + " " + that;
        TermSymbol overloaded = new TermSymbol(
            pos, name, owner,
	    ((this.flags | that.flags) & (SOURCEFLAGS | JAVA) & ~ACCESSFLAGS) |
	    (this.flags & that.flags & ACCESSFLAGS));
        overloaded.setInfo(new LazyOverloadedType(this, that));
        return overloaded;
    }

    /** A lazy type which, when forced computed the overloaded type
     *  of symbols `sym1' and `sym2'. It also checks that this type is well-formed.
     */
    private static class LazyOverloadedType extends Type.LazyType {
	Symbol sym1;
	Symbol sym2;
	LazyOverloadedType(Symbol sym1, Symbol sym2) {
	    this.sym1 = sym1;
	    this.sym2 = sym2;
	}
	private Symbol[] alts(Symbol sym) {
	    if (sym == null) return Symbol.EMPTY_ARRAY;
	    switch (sym.type()) {
	    case OverloadedType(Symbol[] alts, _): return alts;
	    default: return new Symbol[]{sym};
	    }
	}
	private Type[] alttypes(Symbol sym) {
	    if (sym == null) return Type.EMPTY_ARRAY;
	    switch (sym.type()) {
	    case OverloadedType(_, Type[] alttypes): return alttypes;
	    default: return new Type[]{sym.type()};
	    }
	}
	public void complete(Symbol overloaded) {
	    if (sym1 != null) sym1.initialize();
	    if (sym2 != null) sym2.initialize();

	    Symbol[] alts1 = alts(sym1);
	    Symbol[] alts2 = alts(sym2);
	    Symbol[] alts3 = new Symbol[alts1.length + alts2.length];
	    System.arraycopy(alts1, 0, alts3, 0, alts1.length);
	    System.arraycopy(alts2, 0, alts3, alts1.length, alts2.length);

	    Type[] alttypes1 = alttypes(sym1);
	    Type[] alttypes2 = alttypes(sym2);
	    Type[] alttypes3 = new Type[alttypes1.length + alttypes2.length];
	    System.arraycopy(alttypes1, 0, alttypes3, 0, alttypes1.length);
	    System.arraycopy(alttypes2, 0, alttypes3, alttypes1.length, alttypes2.length);
	    overloaded.setInfo(Type.OverloadedType(alts3, alttypes3));
	}
    }

    /** All the alternatives of this symbol if it's overloaded, the
     * symbol alone otherwise.
     */
    public Symbol[] alternatives() {
	switch (type()) {
	case OverloadedType(Symbol[] alts, _): return alts;
	default: return new Symbol[]{this};
        }
    }

    /** The symbol which is overridden by this symbol in base class `base'
     *  `base' must be a superclass of this.owner().
     */
    public Symbol overriddenSymbol(Type base) {
	Symbol sym1 = base.lookupNonPrivate(name);
	if (sym1.kind == Kinds.NONE || (sym1.flags & STATIC) != 0) {
	    return Symbol.NONE;
	} else {
	    //System.out.println(this + ":" + this.type() + locationString() + " overrides? " + sym1 + sym1.type() + sym1.locationString()); //DEBUG

	    Type symtype = owner.thisType().memberType(this);
	    //todo: try whether we can do: this.type(); instead
	    Type sym1type = owner.thisType().memberType(sym1);
	    switch (sym1type) {
	    case OverloadedType(Symbol[] alts, Type[] alttypes):
		for (int i = 0; i < alts.length; i++) {
		    if (symtype.isSameAs(alttypes[i])) return alts[i];
		}
		return Symbol.NONE;
	    default:
		if (symtype.isSameAs(sym1type)) return sym1;
		else {
		    System.out.println(this + locationString() + " does not override " + sym1 + sym1.locationString() + ", since " + symtype + " # " + sym1type);//DEBUG
		    return Symbol.NONE;
		}
	    }
	}
    }
}

/** A class for term symbols
 */
public class TermSymbol extends Symbol {

    private Symbol clazz;

    /** Constructor */
    public TermSymbol(int pos, Name name, Symbol owner, int flags) {
        super(VAL, pos, name, owner, flags);
    }

    public static TermSymbol newConstructor(Symbol clazz, int flags) {
        TermSymbol sym = new TermSymbol(
	    clazz.pos, clazz.name.toConstrName(), clazz.owner(),
	    flags | FINAL);
	sym.clazz = clazz;
	return sym;
    }

    public static TermSymbol newJavaConstructor(Symbol clazz) {
	return newConstructor(clazz, clazz.flags & (ACCESSFLAGS | QUALIFIED | JAVA));
    }

    public static TermSymbol newModule(int pos, Name name, Symbol owner, int flags) {
	TermSymbol sym = new TermSymbol(pos, name, owner, flags | MODUL | FINAL);
        Symbol clazz = new ClassSymbol(
	    pos, name.toTypeName(), owner, flags | MODUL | FINAL, sym);
	Type clazztype = Type.TypeRef(owner.thisType(), clazz, Type.EMPTY_ARRAY);
        clazz.constructor().setInfo(Type.MethodType(Symbol.EMPTY_ARRAY, clazztype));
	sym.clazz = clazz;
	sym.setInfo(clazztype);
	return sym;
    }

    /** Constructor for companion modules to classes, which need to be completed.
     */
    public static TermSymbol newCompanionModule(Symbol clazz, int flags, Type.LazyType parser) {
        TermSymbol sym = newModule(Position.NOPOS, clazz.name.toTermName(), clazz.owner(),
				   FINAL | flags);
        sym.clazz.setInfo(parser);
	return sym;
    }

    /** Java package module constructor
     */
    public static TermSymbol newJavaPackageModule(Name name, Symbol owner, Type.LazyType parser) {
        TermSymbol sym = newModule(Position.NOPOS, name, owner, JAVA | PACKAGE);
        sym.clazz.flags |= SYNTHETIC;
        sym.clazz.setInfo(parser);
	return sym;
    }

    /** Get this.type corresponding to this class or module
     */
    public Type thisType() {
	if ((flags & MODUL) != 0) return moduleClass().thisType();
	else return Type.localThisType;
    }
    /** Get the fully qualified name of this Symbol */
    public Name fullName() {
	if ((flags & MODUL) != 0) return moduleClass().fullName();
	else return super.fullName();
    }

    /** Return a fresh symbol with the same fields as this one.
     */
    public Symbol cloneSymbol() {
        assert !isPrimaryConstructor() : Debug.show(this);
        TermSymbol other;
	if (isModule()) {
	    other = newModule(pos, name, owner(), flags);
	} else {
	    other = new TermSymbol(pos, name, owner(), flags);
	    other.clazz = clazz;
	}
        other.setInfo(info());
        return other;
    }

    public Symbol constructorClass() {
	return isConstructor() ? clazz : this;
    }

    public Symbol moduleClass() {
	return (flags & MODUL) != 0 ? clazz : this;
    }
}

/** A class for (abstract and alias) type symbols. It has ClassSymbol as a subclass.
 */
public class TypeSymbol extends Symbol {

     /** A cache for closures
     */
    private ClosureIntervalList closures = ClosureIntervalList.EMPTY;

    /** The symbol's type template */
    private Type template;

    /** The primary constructor of this type */
    public final Symbol constructor;

    /** Constructor */
    public TypeSymbol(int kind, int pos, Name name, Symbol owner, int flags) {
        super(kind, pos, name, owner, flags);
        this.constructor = TermSymbol.newConstructor(this, flags);
	if (kind == TYPE) { // provide a constructor type
	    this.constructor().setInfo(
		Type.PolyType(Symbol.EMPTY_ARRAY, Type.NoType));
	}
    }

    /** Return a fresh symbol with the same fields as this one.
     */
    public Symbol cloneSymbol() {
	if (Global.instance.debug) System.out.println("cloning " + this + this.locationString());
        TypeSymbol other = new TypeSymbol(kind, pos, name, owner(), flags);
        other.setInfo(info());
	other.constructor.setInfo(constructor.info());
        return other;
    }

    /** Get self type */
    public Type type() {
	if (template == null || template.typeArgs().length != typeParams().length) {
	    template = Type.TypeRef(
		owner().thisType(), this, type(typeParams()));
	}
	return template;
    }

    //todo: needed?
    public Type typeAt(int id) {
	return type();
    }

    /** Get type parameters */
    public Symbol[] typeParams() {
	return constructor.info().typeParams();
    }

    /** Get primary constructor */
    public Symbol constructor() {
        return constructor;
    }

    public Type[] closure() {
	if (kind == ALIAS) return info().symbol().closure();
	int id = Global.instance.currentPhase.id;
	if (closures.limit < id) {
	    if (id == 0 || changes(closureAt(id - 1))) {
		closures = new ClosureIntervalList(closures);
		closures.limit = id;
		computeClosure();
	    } else {
		closures.limit = id;
	    }
	    return closures.closure;
	} else {
	    ClosureIntervalList closures1 = closures;
	    while (closures1.prev.limit >= id) {
		closures1 = closures1.prev;
	    }
	    return closures1.closure;
	}
    }

    //todo: needed?
    private Type[] closureAt(int id) {
	PhaseDescriptor savedPhase = Global.instance.currentPhase;
	Global.instance.currentPhase = Global.instance.phases[id];
	Type[] c = closure();
	Global.instance.currentPhase = savedPhase;
	return c;
    }

    private boolean changes(Type[] closure) {
	for (int i = 0; i < closure.length; i++) {
	    Symbol c = closure[i].symbol();
	    if (c.infoAt(Global.instance.currentPhase.id - 1) != c.info())
		return true;
	}
	return false;
    }

    private static Type[] BAD_CLOSURE = new Type[0];

    /** Return the type itself followed by all direct and indirect
     *  base types of this type, sorted by isLess().
     */
    private void computeClosure() {
	assert closures.closure != BAD_CLOSURE : this;
	closures.closure = BAD_CLOSURE; // to catch cycles.
	SymSet closureClassSet = inclClosureBases(SymSet.EMPTY, this);
	Symbol[] closureClasses = new Symbol[closureClassSet.size() + 1];
	closureClasses[0] = this;
	closureClassSet.copyToArray(closureClasses, 1);
	//System.out.println(ArrayApply.toString(closureClasses));//DEBUG
	closures.closure = Symbol.type(closureClasses);
	//System.out.println(ArrayApply.toString(closures.closure));//DEBUG
	adjustType(type());
	//System.out.println("closure(" + this + ") at " + Global.instance.currentPhase.name() + " = " + ArrayApply.toString(closures.closure));//DEBUG
    }
    //where

 	private SymSet inclClosureBases(SymSet set, Symbol c) {
	    Type[] parents = c.type().parents();
	    for (int i = 0; i < parents.length; i++) {
		set = inclClosure(set, parents[i].symbol());
	    }
	    return set;
	}

 	private SymSet inclClosure(SymSet set, Symbol c) {
	    Symbol c1 = c;
	    while (c1.kind == ALIAS) c1 = c1.info().symbol();
	    return inclClosureBases(set.incl(c1), c1);
	}

	void adjustType(Type tp) {
	    Type tp1 = tp.unalias();
	    int pos = closurePos(tp1.symbol());
	    assert pos >= 0 : this + " " + tp1 + " " + tp1.symbol();
	    closures.closure[pos] = tp1;
	    Type[] parents = tp1.parents();
	    for (int i = 0; i < parents.length; i++) {
		adjustType(parents[i]);
	    }
	}
}

/** A class for class symbols. It has JavaClassSymbol as a subclass.
 */
public class ClassSymbol extends TypeSymbol {

    /** The mangled class name */
    private Name mangled;

    /** The module belonging to the class. This means:
     *  For Java classes, its statics parts.
     *  For module classes, the corresponding module.
     *  For other classes, null.
     */
    private Symbol module = NONE;

    /** Principal Constructor for module classes and classes with static members.
     */
    public ClassSymbol(int pos, Name name, Symbol owner, int flags) {
        super(CLASS, pos, name, owner, flags);
        this.mangled = name;
    }

    /** Constructor for module classes and classes with static members.
     */
    public ClassSymbol(int pos, Name name, Symbol owner, int flags, Symbol module) {
	this(pos, name, owner, flags);
	this.module = module;
    }

    /** Constructor for classes to load as source files
     */
    public ClassSymbol(Name name, Symbol owner, SourceCompleter parser) {
	this(Position.NOPOS, name, owner, 0);
	this.module = TermSymbol.newCompanionModule(this, 0, parser);
        this.mangled = name;
        this.setInfo(parser);
    }

    /** Constructor for classes to load as class files.
     */
    public ClassSymbol(Name name, Symbol owner, ClassParser parser) {
	super(CLASS, Position.NOPOS, name, owner, JAVA);
	this.module = TermSymbol.newCompanionModule(this, JAVA, parser.staticsParser(this));
        this.mangled = name;
        this.setInfo(parser);
    }

    /** Return a fresh symbol with the same fields as this one.
     */
    public Symbol cloneSymbol() {
        ClassSymbol other = new ClassSymbol(pos, name, owner(), flags);
        other.setInfo(info());
	other.constructor.setInfo(constructor.info());
	other.mangled = mangled;
	other.module = module;
        return other;
    }

   /** Get module */
    public Symbol module() {
        return module;
    }

    /** Set the mangled name of this Symbol */
    public Symbol setMangledName(Name name) {
        this.mangled = name;
        return this;
    }

    /** Get the fully qualified name of this Symbol */
    public Name fullName() {
        if (owner().kind == CLASS && owner().name.length() != 0)
            return Name.fromString(owner().fullName() + "." + name);
        else
            return name.toTermName();
    }

    /** Get the mangled name of this Symbol */
    public Name mangledName() {
        return mangled;
    }

    /** Get the fully qualified mangled name of this Symbol */
    public Name mangledFullName() {
	if (mangled == name) {
	    return fullName().replace((byte)'.', (byte)'$');
	} else {
	    return Name.fromString(
		enclToplevelClass().mangledFullName() + "$" + mangled);
	}
    }

    private Type thistp = Type.ThisType(this);

    public Type thisType() {
	return thistp;
    }

    /** Return the next enclosing class */
    public Symbol enclClass() {
        return this;
    }

    public Symbol caseFieldAccessor(int index) {
	assert (flags & CASE) != 0 : this;
	Scope.SymbolIterator it = info().members().iterator();
	Symbol sym = null;
	for (int i = 0; i <= index; i++) {
	    do {
		sym = it.next();
	    } while (sym.kind != VAL || (sym.flags & CASE) == 0);
	}
	//System.out.println(this + ", case field[" + index + "] = " + sym);//DEBUG
	assert sym != null : this;
	return sym;
    }
}

/** A class for error symbols.
 */
public final class ErrorSymbol extends Symbol {

    /** Constructor */
    public ErrorSymbol() {
        super(Kinds.ERROR, Position.NOPOS, Name.fromString("<error>"), null,
	      INITIALIZED);
        this.setOwner(this);
        this.setInfo(Type.ErrorType);
    }

    public Symbol cloneSymbol() {
	return this;
    }

    /** Set the mangled name of this Symbol */
    public Symbol mangled(Name name) {
        return this;
    }

    /** Set owner */
    public Symbol setOwner(Symbol owner) {
        if (owner != this)
            throw new ApplicationError("illegal operation on " + getClass());
        return super.setOwner(owner);
    }

    /** Set type */
    public Symbol setInfo(Type info) {
        if (info != Type.ErrorType)
            throw new ApplicationError("illegal operation on " + getClass());
        return super.setInfo(info);
    }

    /** Get primary constructor */
    public Symbol constructor() {
	return TermSymbol.newConstructor(this, 0).setInfo(Type.ErrorType);
    }

    /** Return the next enclosing class */
    public Symbol enclClass() {
        return this;
    }
}

/** The class of Symbol.NONE
 */
public final class NoSymbol extends Symbol {

    /** Constructor */
    public NoSymbol() {
        super(Kinds.NONE, Position.NOPOS, Name.fromString("<none>"), null, INITIALIZED);
        this.setInfo(Type.NoType);
        this.setOwner(this);
    }

    /** Return a fresh symbol with the same fields as this one.
     */
    public Symbol cloneSymbol() {
        return this;
    }

    /** Set the mangled name of this Symbol */
    public Symbol mangled(Name name) {
        throw new ApplicationError("illegal operation on " + getClass());
    }

    /** Set owner */
    public Symbol setOwner(Symbol owner) {
        if (owner != this)
            throw new ApplicationError("illegal operation on " + getClass());
        return super.setOwner(owner);
    }

    /** Set type */
    public Symbol setInfo(Type info) {
        if (info != Type.NoType)
            throw new ApplicationError("illegal operation on " + getClass());
        return super.setInfo(info);
    }

    /** Return the next enclosing class */
    public Symbol enclClass() {
        return this;
    }

    public Symbol owner() {
	throw new ApplicationError();
    }
}

/** A class for symbols generated in label definitions.
 */
public class LabelSymbol extends TermSymbol {

    /** give as argument the symbol of the function that triggered
	the creation of this label */
    public LabelSymbol(Symbol f) {
	super(f.pos, f.name, f, LABEL);
    }
}

/** An exception for signalling cyclic references.
 */
public class CyclicReference extends Type.Error {
    public Symbol sym;
    public Type info;
    public CyclicReference(Symbol sym, Type info) {
	super("illegal cyclic reference involving " + sym);
	this.sym = sym;
	this.info = info;
    }
}

/** A class for types indexed by phase numbers.
 */
class TypeIntervalList {
    int limit;
    Type info;
    TypeIntervalList prev;
    TypeIntervalList(TypeIntervalList prev) {
	this.prev = prev;
    }
    static TypeIntervalList EMPTY = new TypeIntervalList(null);

  static {
	EMPTY.limit = -1;
    }
}

/** A class for closures indexed by phase numbers.
 */
class ClosureIntervalList {
    int limit;
    Type[] closure;
    ClosureIntervalList prev;
    ClosureIntervalList(ClosureIntervalList prev) {
	this.prev = prev;
    }
    static ClosureIntervalList EMPTY = new ClosureIntervalList(null);
    static {
	EMPTY.limit = -1;
    }
}

