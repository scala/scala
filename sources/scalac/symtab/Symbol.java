/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**
** $Id$
\*                                                                      */

//todo check significance of JAVA flag.

package scalac.symtab;

import ch.epfl.lamp.util.Position;
import scalac.ApplicationError;
import scalac.Global;
import scalac.Phase;
import scalac.util.ArrayApply;
import scalac.util.Name;
import scalac.util.Names;
import scalac.util.NameTransformer;
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
        this.kind = kind;
        this.pos = pos;
        this.name = name;
        this.owner = owner;
        this.flags = flags & ~(INITIALIZED | LOCKED); // safety first
    }

    protected void update(int pos, int flags) {
	this.pos = pos;
	this.flags = (flags & ~(INITIALIZED | LOCKED)) |
	    (this.flags & (INITIALIZED | LOCKED));
    }

    /** Return a fresh symbol with the same fields as this one.
     */
    public final Symbol cloneSymbol() {
        return cloneSymbol(owner);
    }

    /** Return a fresh symbol with the same fields as this one and the
     * given owner.
     */
    public abstract Symbol cloneSymbol(Symbol owner);

    /** Returns a shallow copy of the given array. */
    public static Symbol[] cloneArray(Symbol[] array) {
        Symbol[] clone = new Symbol[array.length];
        for (int i = 0; i < clone.length; i++) clone[i] = array[i];
        return clone;
    }

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
        assert !isModuleClass() : Debug.show(this);
        assert !isPrimaryConstructor() : Debug.show(this);
        setOwner(this, owner);
        return this;
    }
    private static void setOwner(Symbol symbol, Symbol owner) {
        assert symbol != null;
        assert symbol != Symbol.NONE;
        assert symbol != Symbol.ERROR;
        if (symbol.isModule()) setOwner(symbol.moduleClass(), owner);
        if (symbol.isClass()) {
	    Symbol[] alts = symbol.allConstructors().alternativeSymbols();
	    for (int i = 0; i < alts.length; i++) setOwner(alts[i], owner);
	}
        symbol.owner = owner;
    }

    /** Set information, except if symbol is both initialized and locked.
     */
    public Symbol setInfo(Type info) {
	return setInfoAt(info, currentPhaseId());
    }

    public Symbol setFirstInfo(Type info) {
        return setInfoAt(info, 0);
    }

    private Symbol setInfoAt(Type info, int limit) {
	assert !isConstructor()
	    || info instanceof Type.LazyType
	    || info == Type.NoType
	    || info == Type.ErrorType
	    || info instanceof Type.MethodType
	    || info instanceof Type.OverloadedType
	    || info instanceof Type.PolyType
	    : "illegal type for " + this + ": " + info;
	if (infos == TypeIntervalList.EMPTY) {
	    infos = new TypeIntervalList(TypeIntervalList.EMPTY);
	}
	infos.limit = limit;
	infos.info = info;
	if (info instanceof Type.LazyType) flags &= ~INITIALIZED;
	else flags |= INITIALIZED;
        return this;
    }

    /** Set type -- this is an alias for setInfo(Type info)
     */
    public Symbol setType(Type info) { return setInfo(info); }

    /** Set type of `this' in current class
     */
    public Symbol setTypeOfThis(Type tp) {
	throw new ApplicationError(this + ".setTypeOfThis");
    }

    public Symbol updateInfo(Type info) {
        assert infos.limit <= Global.instance.currentPhase.id + 1 : this;
	if (infos.limit > Global.instance.currentPhase.id) infos.limit--;
        infos = new TypeIntervalList(infos);
        infos.limit = Global.instance.currentPhase.id + 1;
	infos.info = info;
	return this;
    }

    /** Set the low bound of this type variable
     */
    public Symbol setLoBound(Type lobound) {
	throw new ApplicationError("setLoBound inapplicable for " + this);
    }

    /** Add an auxiliary constructor to class; return created symbol.
     */
    public Symbol addConstructor() {
	throw new ApplicationError("addConstructor inapplicable for " + this);
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
	preInitialize();
        return kind == VAL && !(isModule() && isJava()) && !isPackage();
    }

    /** Does this symbol denote a stable value? */
    public final boolean isStable() {
	return kind == VAL &&
	    ((flags & STABLE) != 0 ||
	     (flags & MUTABLE) == 0 && type().isObjectType());
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
	case PolyType(_, _):
	    return true;
	case OverloadedType(Symbol[] alts, _):
	    for (int i = 0; i < alts.length; i++)
		if (alts[i].isMethod()) return true;
	    return false;
	default:
	    return false;
	}
    }

    public final boolean isMethod() {
	initialize();
	return isInitializedMethod();
    }

    public final boolean isCaseFactory() {
	return isMethod() && !isConstructor() && (flags & CASE) != 0;
    }

    public final boolean isAbstractClass() {
	preInitialize();
	return (flags & ABSTRACTCLASS) != 0 &&
	    this != Global.instance.definitions.ARRAY_CLASS;
    }

    /* Does this symbol denote an anonymous class? */
    public final boolean isAnonymousClass() {
	return isClass() && name.startsWith(Names.ANON_CLASS_NAME);
    }

    /** Does this symbol denote the root class or root module?
     */
    public final boolean isRoot() {
	return this.moduleClass() == Global.instance.definitions.ROOT_CLASS;
    }

    /** Does this symbol denote something loaded from a Java class? */
    public final boolean isJava() {
	preInitialize();
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

    /** Does this symbol denote a global module? */
    public final  boolean isGlobalModule() {
	return isModule() && (owner().isPackage()
            //|| owner().isGlobalModule() // add later? translation does not work (yet?)
        );
    }

    /** Does this symbol denote a module? */
    public final boolean isModuleClass() {
        return kind == CLASS && (flags & MODUL) != 0;
    }

    /** Does this symbol denote a class? */
    public final boolean isClass() {
        return kind == CLASS && (flags & PACKAGE) == 0;
    }

    /** Does this symbol denote a case class?
     */
    public final boolean isCaseClass() {
	preInitialize();
	return kind == CLASS && (flags & CASE) != 0;
    }

    /** Does this symbol denote a uniform (i.e. parameterless) class? */
    public final boolean isTrait() {
	//preInitialize(); todo: enable, problem is that then we cannot print
	// during unpickle
	return kind == CLASS && (flags & TRAIT) != 0;
    }

    /** Does this class symbol denote a compound type symbol?
     */
    public final boolean isCompoundSym() {
	return name == Names.COMPOUND_NAME.toTypeName();
    }

    /** Does this symbol denote an interface? */
    public final boolean isInterface() {
	preInitialize();
        return (flags & INTERFACE) != 0;
    }

    /** Does this symbol denote a public symbol? */
    public final boolean isPublic() {
        return !isProtected() && !isPrivate();
    }

    /** Does this symbol denote a protected symbol? */
    public final boolean isProtected() {
	preInitialize();
        return (flags & PROTECTED) != 0;
    }

    /** Does this symbol denote a private symbol? */
    public final boolean isPrivate() {
	preInitialize();
        return (flags & PRIVATE) != 0;
    }

    /** Does this symbol denote a deferred symbol? */
    public final boolean isDeferred() {
        return (flags & DEFERRED) != 0;
    }

    /** Does this symbol denote a synthetic symbol? */
    public final boolean isSynthetic() {
        return (flags & SYNTHETIC) != 0;
    }

    /** Does this symbol denote a static member? */
    public final boolean isStatic() {
        return (flags & STATIC) != 0;
    }

    /** Does this symbol denote an accessor? */
    public final boolean isAccessor() {
        return (flags & ACCESSOR) != 0;
    }

    /** Is this symbol locally defined? I.e. not a member of a class or module */
    public final boolean isLocal() {
	return owner.kind == VAL &&
	    !((flags & PARAM) != 0 && owner.isPrimaryConstructor());
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
	return isClass() &&
	    (isAnonymousClass() ||
	     owner.isValue() ||
	     owner.isLocalClass());
    }

    /** Is this symbol a constructor? */
    public boolean isConstructor() {
	return false;
    }

    /** Is this symbol the primary constructor of a type? */
    public final boolean isPrimaryConstructor() {
	return isConstructor() && this == constructorClass().primaryConstructor();
    }

    public final boolean isGenerated() {
	return name.pos((byte)'$') < name.length();
    }

    /** Symbol was preloaded from package
     */
    public final boolean isExternal() {
	return pos == Position.NOPOS;
    }

    /** Is this symbol an overloaded symbol? */
    public final boolean isOverloaded() {
        switch (info()) {
        case OverloadedType(_,_): return true;
        default                 : return false;
        }
    }

    /** Does this symbol denote a label? */
    public final boolean isLabel() {
        return (flags & LABEL) != 0;
    }

    /** The variance of this symbol as an integer
     */
    public int variance() {
	if ((flags & COVARIANT) != 0) return 1;
	else if ((flags & CONTRAVARIANT) != 0) return -1;
	else return 0;
    }

// Symbol names ----------------------------------------------------------------

    /** Get the fully qualified name of this Symbol
     *  (this is always a normal name, never a type name)
     */

    /** Get the simple name of this Symbol (this is always a term name)
     */
    public Name simpleName() {
	return isConstructor() ? constructorClass().name.toTermName()
	    : name.toTermName();
    }

    /** Get the fully qualified name of this Symbol */
    public Name fullName() {
	return simpleName();
    }

    /** Get the mangled name of this Symbol
     *  (this is always a normal name, never a type name)
     */
    public Name mangledName() {
        return isConstructor() ? constructorClass().name.toTermName() : name.toTermName();
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

    /** Get value parameters */
    public Symbol[] valueParams() {
	return EMPTY_ARRAY;
    }

    /** Get all constructors of class */
    public Symbol allConstructors() {
        return NONE;
    }

    /** Get primary constructor of class */
    public Symbol primaryConstructor() {
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
	if (clazz.primaryConstructor() == owner) return clazz;
	else return owner;
    }

    /** The next enclosing class */
    public Symbol enclClass() {
        return owner().enclClass();
    }

    /** The next enclosing method */
    public Symbol enclMethod() {
        return isMethod() ? this : owner().enclMethod();
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

     /* If this is a primary constructor, return the class it constructs.
     *  Otherwise return the symbol itself.
     */
    public Symbol constructorClass() {
	return this;
    }

    /** Return first alternative if this has a (possibly lazy)
     *  overloaded type, otherwise symbol itself.
     *  Needed in ClassSymbol.primaryConstructor() and in UnPickle.
     */
    public Symbol firstAlternative() {
	if (infos.info instanceof Type.OverloadedType)
	    return infos.info.alternativeSymbols()[0];
	else if (infos.info instanceof LazyOverloadedType)
	    return ((LazyOverloadedType) infos.info).sym1.firstAlternative();
	else
	    return this;
    }

     /* If this is a module, return its class.
     *  Otherwise return the symbol itself.
     */
    public Symbol moduleClass() {
	return this;
    }

    /** if type is a (possibly lazy) overloaded type, return its alternatves
     *  else return array consisting of symbol itself
     */
    public Symbol[] alternativeSymbols() {
	Symbol[] alts = type().alternativeSymbols();
	if (alts.length == 0) return new Symbol[]{this};
	else return alts;
    }

    /** if type is a (possibly lazy) overloaded type, return its alternatves
     *  else return array consisting of type itself
     */
    public Type[] alternativeTypes() {
	return type().alternativeTypes();
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

    /** Lookup symbol with given name; return Symbol.NONE if not found.
     */
    public Symbol lookup(Name name) {
        return info().lookup(name);
    }

// Symbol types --------------------------------------------------------------

    /** Was symbol's type updated during phase `id'?
     */
    public boolean isUpdated(int id) {
	return infos.limit >= id;
    }

    /** the current phase id, or the id after analysis, whichever is larger.
     */
    static int currentPhaseId() {
	return Global.instance.currentPhase.id;
    }

    public int definedPhaseId() {
	TypeIntervalList i = infos;
	while (i.prev != TypeIntervalList.EMPTY) i = i.prev;
	return i.limit;
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

    /** Make sure symbol is entered
     */
    public final void preInitialize() {
	//todo: clean up
	if (infos.info instanceof ClassParser ||
	    infos.info instanceof SourceCompleter ||
	    infos.info instanceof ClassParser.StaticsParser)
	    infos.info.complete(this);
    }

    /** Get info; This is:
     *  for a term symbol, its type
     *  for a type variable, its bound
     *  for a type alias, its right-hand side
     *  for a class symbol, the compound type consisting of
     *  its baseclasses and members.
     */
    public Type info() {
	//if (isModule()) moduleClass().initialize();
	int id = currentPhaseId();
	if ((flags & INITIALIZED) == 0) {
	    Type info = rawFirstInfo();
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
 		Type tp = info();
 		flags &= ~SNDTIME;
 	    } else {
 		assert !(rawInfoAt(id) instanceof Type.LazyType) : this;
 		//flags |= INITIALIZED;
 	    }
	    //System.out.println("done: " + this);//DEBUG
	}
	return rawInfoAt(id);
    }

    /** Get info at phase #id
     */
    public Type infoAt(int id) {
	info();
	return rawInfoAt(id);
    }

    /** Get info at next phase
     */
    public Type nextInfo() {
	Global.instance.nextPhase();
	Type info = info();
	Global.instance.prevPhase();
        return info;
    }

    /** get info at phase #id, without forcing lazy types.
     */
    private Type rawInfoAt(int id) {
	//if (infos == TypeIntervalList.EMPTY) return Type.NoType;//DEBUG
	assert infos != TypeIntervalList.EMPTY : this;
	int nextid = infos.limit;
	if (nextid < id) {
	    Phase curphase = Global.instance.currentPhase;
	    do {
		Global.instance.currentPhase = Global.instance.phases[nextid];
		Type newInfo =
		    Global.instance.currentPhase.transformInfo(this, infos.info);
		if (newInfo != infos.info) {
		    infos = new TypeIntervalList(infos);
		    infos.info = newInfo;
		}
		nextid++;
		infos.limit = nextid;
	    } while (nextid < id);
	    Global.instance.currentPhase = curphase;
	    return infos.info;
	} else {
	    TypeIntervalList infos1 = infos;
	    while (infos1.prev.limit >= id) {
		infos1 = infos1.prev;
	    }
	    return infos1.info;
	}
    }

    public Type rawFirstInfo() {
        TypeIntervalList infos1 = infos;
        while (infos1.prev.limit >= 0) {
            infos1 = infos1.prev;
        }
        return infos1.info;
    }

    public Type rawInfo() {
	return rawInfoAt(currentPhaseId());
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

    /** The type constructor of a symbol is:
     *  For a type symbol, the type corresponding to the symbol itself, excluding
     *  parameters.
     *  Not applicable for term symbols.
     */
    public Type typeConstructor() {
	throw new ApplicationError("typeConstructor inapplicable for " + this);
    }

    /** The low bound of this type variable
     */
    public Type loBound() {
	return Global.instance.definitions.ALL_TYPE;
    }

    /** Get this.type corresponding to this symbol
     */
    public Type thisType() {
	return Type.localThisType;
    }

    /** Get type of `this' in current class.
     */
    public Type typeOfThis() {
	return type();
    }

    /** Get this symbol of current class
     */
    public Symbol thisSym() { return this; }


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

	diff = that.hashCode() - this.hashCode();
	if (diff > 0) return true;
	if (diff < 0) return false;

	if (owner().isLess(that.owner())) return true;
	if (that.owner().isLess(owner())) return false;

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

    public Type baseType(Symbol sym) {
	int i = closurePos(sym);
	if (i >= 0) return closure()[i];
	else return Type.NoType;
    }

    /** Is this class a subclass of `c'? I.e. does it have a type instance
     *  of `c' as indirect base class?
     */
    public boolean isSubClass(Symbol c) {
	return this == c ||
	    c.kind == Kinds.ERROR ||
	    closurePos(c) >= 0 ||
	    this == Global.instance.definitions.ALL_CLASS ||
	    (this == Global.instance.definitions.ALLREF_CLASS &&
	     c != Global.instance.definitions.ALL_CLASS &&
	     c.isSubClass(Global.instance.definitions.ANYREF_CLASS));
    }

    /** Get base types of this symbol */
    public Type[] parents() {
        return info().parents();
    }

// ToString -------------------------------------------------------------------

    /** String representation of symbol's simple name.
     *  Translates expansions of operators back to operator symbol. E.g.
     *  $eq => =.
     */
    public String nameString() {
	return NameTransformer.decode(simpleName()).toString();
    }

    /** String representation of symbol's full name.
     *  Translates expansions of operators back to operator symbol. E.g.
     *  $eq => =.
     */
    public String fullNameString() {
	return NameTransformer.decode(fullName()).toString();
    }

    /** String representation, including symbol's kind
     *  e.g., "class Foo", "function Bar".
     */
    public String toString() {
        return new SymbolTablePrinter().printSymbol(this).toString();
    }

    /** String representation of location.
     */
    public String locationString() {
	if (owner.kind == CLASS &&
	    !owner.isAnonymousClass() && !owner.isCompoundSym() ||
	    Global.instance.debug)
	    return " in " + owner;
	else
	    return "";
    }

    /** String representation of definition.
     */
    public String defString() {
        return new SymbolTablePrinter().printSignature(this).toString();
    }

    public static String[] defString(Symbol[] defs) {
	String[] strs = new String[defs.length];
	for (int i = 0; i < defs.length; i++)
	    strs[i] = defs[i].defString();
	return strs;
    }

// Overloading and Overriding -------------------------------------------

    /** Add another overloaded alternative to this symbol.
     */
    public Symbol overloadWith(Symbol that) {
        assert isTerm() : Debug.show(this);
	assert this.name == that.name : Debug.show(this) + " <> " + Debug.show(that);
	assert this.owner == that.owner : Debug.show(this) + " != " + Debug.show(that);
	assert (this.flags & that.flags & JAVA) != 0 ||
	    (this.flags & OVERLOADFLAGS) == (that.flags & OVERLOADFLAGS)
	    : Integer.toHexString(this.flags) + "@" + Debug.show(this) + " <> " + Integer.toHexString(that.flags) + "@" + Debug.show(that);
	assert this.isConstructor() == that.isConstructor();
	int overflags =
	    ((this.flags | that.flags) & (JAVA | OVERLOADFLAGS)) |
	    (this.flags & that.flags & ACCESSFLAGS);
	TermSymbol overloaded = (this.isConstructor())
	    ? TermSymbol.newConstructor(this.constructorClass(), overflags)
	    : new TermSymbol(pos, name, owner, overflags);
        overloaded.setInfo(new LazyOverloadedType(this, that));
        return overloaded;
    }

    /** A lazy type which, when forced computed the overloaded type
     *  of symbols `sym1' and `sym2'. It also checks that this type is well-formed.
     */
    public static class LazyOverloadedType extends Type.LazyType {
	Symbol sym1;
	Symbol sym2;
	LazyOverloadedType(Symbol sym1, Symbol sym2) {
	    this.sym1 = sym1;
	    this.sym2 = sym2;
	}

	public Symbol[] alternativeSymbols() {
	    Symbol[] alts1 = sym1.alternativeSymbols();
	    Symbol[] alts2 = sym2.alternativeSymbols();
	    Symbol[] alts3 = new Symbol[alts1.length + alts2.length];
	    System.arraycopy(alts1, 0, alts3, 0, alts1.length);
	    System.arraycopy(alts2, 0, alts3, alts1.length, alts2.length);
	    return alts3;
	}

	public Type[] alternativeTypes() {
	    Type[] alts1 = sym1.alternativeTypes();
	    Type[] alts2 = sym2.alternativeTypes();
	    Type[] alts3 = new Type[alts1.length + alts2.length];
	    System.arraycopy(alts1, 0, alts3, 0, alts1.length);
	    System.arraycopy(alts2, 0, alts3, alts1.length, alts2.length);
	    return alts3;
	}

	public void complete(Symbol overloaded) {
	    overloaded.setInfo(
		Type.OverloadedType(
		    alternativeSymbols(), alternativeTypes()));
	}

	public String toString() {
	    return "LazyOverloadedType(" + sym1 + "," + sym2 + ")";
	}
    }

    /** The symbol which is overridden by this symbol in base class `base'
     *  `base' must be a superclass of this.owner().
     */
    public Symbol overriddenSymbol(Type base) {
	assert !isOverloaded() : this;
	Symbol sym1 = base.lookupNonPrivate(name);
	if (sym1.kind == Kinds.NONE || (sym1.flags & STATIC) != 0) {
	    return Symbol.NONE;
	} else {
	    //System.out.println(this + ":" + this.type() + locationString() + " overrides? " + sym1 + sym1.type() + sym1.locationString()); //DEBUG
	    //System.out.println(owner.thisType());//DEBUG

	    Type symtype = this.type();//owner.thisType().memberType(this);
	    //todo: try whether we can do: this.type(); instead
	    Type sym1type = owner.thisType().memberType(sym1);
	    switch (sym1type) {
	    case OverloadedType(Symbol[] alts, Type[] alttypes):
		for (int i = 0; i < alts.length; i++) {
		    if (symtype.isSubType(alttypes[i])) return alts[i];
		}
		return Symbol.NONE;
	    default:
		if (symtype.isSubType(sym1type)) return sym1;
		else {
		    if (Global.instance.debug) System.out.println(this + locationString() + " does not override " + sym1 + sym1.locationString() + ", since " + symtype + " !<= " + sym1type);//DEBUG
		    return Symbol.NONE;
		}
	    }
	}
    }

    /** The symbol which is overridden by this symbol in base class `base'
     *  `base' must be a superclass of this.owner().
     */
    public Symbol overridingSymbol(Type sub) {
	assert !isOverloaded() : this;
	Symbol sym1 = sub.lookup(name);
	if (sym1.kind == Kinds.NONE || (sym1.flags & STATIC) != 0) {
	    return Symbol.NONE;
	} else {
	    //System.out.println(this + ":" + this.type() + locationString() + " overrides? " + sym1 + sym1.type() + sym1.locationString()); //DEBUG
	    //System.out.println(owner.thisType());//DEBUG

	    Type symtype = sub.memberType(this);
	    Type sym1type = sub.memberType(sym1);
	    switch (sym1type) {
	    case OverloadedType(Symbol[] alts, Type[] alttypes):
		for (int i = 0; i < alts.length; i++) {
		    if (alttypes[i].isSubType(symtype)) return alts[i];
		}
		return Symbol.NONE;
	    default:
		if (sym1type.isSubType(symtype)) return sym1;
		else {
		    if (Global.instance.debug) System.out.println(this + locationString() + " is not overridden by " + sym1 + sym1.locationString() + ", since " + sym1type + " !<= " + symtype);//DEBUG
		    return Symbol.NONE;
		}
	    }
	}
    }

    /** Does this symbol override that symbol?
     */
    public boolean overrides(Symbol that) {
	return
	    ((this.flags | that.flags) & (PRIVATE | STATIC)) == 0 &&
	    this.name == that.name &&
	    owner.thisType().memberType(this).isSubType(
		owner.thisType().memberType(that));
    }

    /** Reset symbol to initial state
     */
    public void reset(Type completer) {
	this.flags &= SOURCEFLAGS;
	this.pos = 0;
	this.infos = TypeIntervalList.EMPTY;
	this.setInfo(completer);
    }

    /** return a tag which (in the ideal case) uniquely identifies
     *  class symbols
     */
  	public int tag() {
  		return name.toString().hashCode();
  	}
}

/** A class for term symbols
 */
public class TermSymbol extends Symbol {

    private Symbol clazz;

    /** Constructor */
    public TermSymbol(int pos, Name name, Symbol owner, int flags) {
        super(VAL, pos, name, owner, flags);
	assert !name.isTypeName() : this;
    }

    public static TermSymbol define(
	int pos, Name name, Symbol owner, int flags, Scope scope) {
	Scope.Entry e = scope.lookupEntry(name);
	if (e.owner == scope && e.sym.isExternal() && e.sym.kind == VAL) {
	    TermSymbol sym = (TermSymbol) e.sym;
	    sym.update(pos, flags);
	    return sym;
	} else {
	    return new TermSymbol(pos, name, owner, flags);
	}
    }

    public static TermSymbol newConstructor(Symbol clazz, int flags) {
        TermSymbol sym = new TermSymbol(
	    clazz.pos, Names.CONSTRUCTOR, clazz.owner(), flags);
	sym.clazz = clazz;
	return sym;
    }

    public static TermSymbol newJavaConstructor(Symbol clazz) {
	return newConstructor(clazz, clazz.flags & (ACCESSFLAGS | JAVA));
    }

    public TermSymbol makeModule(ClassSymbol clazz) {
	flags |= MODUL | FINAL;
	this.clazz = clazz;
	clazz.setModule(this);
	setInfo(clazz.typeConstructor());
	return this;
    }

    public TermSymbol makeModule() {
        ClassSymbol clazz = new ClassSymbol(
	    pos, name.toTypeName(), owner(), flags | MODUL | FINAL);
        clazz.primaryConstructor().setInfo(
	    Type.MethodType(Symbol.EMPTY_ARRAY, clazz.typeConstructor()));
	return makeModule(clazz);
    }

    /** Constructor for companion modules to classes, which need to be completed.
     */
    public static TermSymbol newCompanionModule(Symbol clazz, int flags, Type.LazyType parser) {
        TermSymbol sym = new TermSymbol(
	    Position.NOPOS, clazz.name.toTermName(), clazz.owner(), flags)
	    .makeModule();
        sym.clazz.setInfo(parser);
	return sym;
    }

    /** Java package module constructor
     */
    public static TermSymbol newJavaPackageModule(Name name, Symbol owner, Type.LazyType parser) {
        TermSymbol sym = new TermSymbol(Position.NOPOS, name, owner, JAVA | PACKAGE)
	    .makeModule();
        sym.clazz.flags |= SYNTHETIC;
        sym.clazz.setInfo(parser != null ? parser : Type.compoundType(Type.EMPTY_ARRAY, new Scope(), sym));
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
	if (clazz != null) return clazz.fullName();
	else return super.fullName();
    }

    /** Is this symbol a constructor? */
    public boolean isConstructor() {
	return name == Names.CONSTRUCTOR;
    }

    /** Return a fresh symbol with the same fields as this one.
     */
    public Symbol cloneSymbol(Symbol owner) {
        assert !isPrimaryConstructor() : Debug.show(this);
        TermSymbol other;
	if (isModule()) {
	    other = new TermSymbol(pos, name, owner, flags).makeModule();
	} else {
	    other = new TermSymbol(pos, name, owner, flags);
	    other.clazz = clazz;
	}
        other.setInfo(info());
        return other;
    }

    public Symbol[] typeParams() {
	return type().typeParams();
    }

    public Symbol[] valueParams() {
	return type().valueParams();
    }

    public Symbol constructorClass() {
	return isConstructor() && clazz != null ? clazz : this;
    }

    public Symbol moduleClass() {
	return (flags & MODUL) != 0 ? clazz : this;
    }
}

/** A base class for all type symbols.
 *  It has AliasTypeSymbol, AbsTypeSymbol, ClassSymbol as subclasses.
 */
public abstract class TypeSymbol extends Symbol {

     /** A cache for closures
     */
    private ClosureIntervalList closures = ClosureIntervalList.EMPTY;

    /** The symbol's type template */
    private Type template = null;

    /** A cache for type constructors
     */
    private Type tycon = null;

    /** The primary constructor of this type */
    private Symbol constructor;

    /** Constructor */
    public TypeSymbol(int kind, int pos, Name name, Symbol owner, int flags) {
        super(kind, pos, name, owner, flags);
	assert name.isTypeName() : this;
	if (kind != TYPE)
	    this.constructor = TermSymbol.newConstructor(this, flags & ~MODUL);
    }

    protected void update(int pos, int flags) {
	super.update(pos, flags);
	this.template = null;
    }

    /** copy all fields to `sym'
     */
    public void copyTo(Symbol sym) {
	super.copyTo(sym);
	if (kind != TYPE) {
	    Symbol symconstr = ((TypeSymbol) sym).constructor;
	    constructor.copyTo(symconstr);
	    if (constructor.isInitialized())
		symconstr.setInfo(fixConstrType(symconstr.type(), sym));
	}
    }

    protected void copyConstructorInfo(TypeSymbol other) {
	other.primaryConstructor().setInfo(
	    fixConstrType(
		primaryConstructor().info().cloneType(
		    primaryConstructor(), other.primaryConstructor()),
		other));
	Symbol[] alts = allConstructors().alternativeSymbols();
	for (int i = 1; i < alts.length; i++) {
	    Symbol constr = other.addConstructor();
	    constr.setInfo(
		fixConstrType(
		    alts[i].info().cloneType(alts[i], constr),
		    other));
	}
    }

    private Type fixConstrType(Type type, Symbol clone) {
        switch (type) {
        case MethodType(Symbol[] vparams, Type result):
            result = fixConstrType(result, clone);
            return new Type.MethodType(vparams, result);
        case PolyType(Symbol[] tparams, Type result):
            result = fixConstrType(result, clone);
            return new Type.PolyType(tparams, result);
        case TypeRef(Type pre, Symbol sym, Type[] args):
            assert sym == this : Debug.show(sym) + " != " + Debug.show(this);
            return new Type.TypeRef(pre, clone, args);
	case LazyType():
	    return type;
        default:
            throw Debug.abort("unexpected constructor type:" + clone + ":" + type);
        }
    }

    /** add a constructor
     */
    public Symbol addConstructor() {
	Symbol constr = TermSymbol.newConstructor(this, flags & ~MODUL);
	constructor = constructor.overloadWith(constr);
	return constr;
    }

    /** Get primary constructor */
    public Symbol primaryConstructor() {
        return (kind == TYPE) ? Symbol.NONE : constructor.firstAlternative();
    }

    /** Get all constructors */
    public Symbol allConstructors() {
	return (kind == TYPE) ? Symbol.NONE : constructor;
    }

    /** Get type parameters */
    public Symbol[] typeParams() {
	return (kind == TYPE) ? Symbol.EMPTY_ARRAY
	    : primaryConstructor().info().typeParams();
    }

    /** Get value parameters */
    public Symbol[] valueParams() {
	return (kind == CLASS) ? primaryConstructor().info().valueParams()
	    : Symbol.EMPTY_ARRAY;
    }

    /** Get type constructor */
    public Type typeConstructor() {
	if (tycon == null)
	    tycon = Type.TypeRef(owner().thisType(), this, Type.EMPTY_ARRAY);
	return tycon;
    }

    public Symbol setOwner(Symbol owner) {
        tycon = null;
	template = null;
        return super.setOwner(owner);
    }

    /** Get type */
    public Type type() {
	Symbol[] tparams = typeParams();
	if (template != null) {
	    switch (template) {
	    case TypeRef(_, _, Type[] targs):
		if (targs.length == tparams.length)
		    return template;
	    }
	}
	if (tparams.length == 0)
	    template = typeConstructor();
	else
	    template = Type.TypeRef(
		owner().thisType(), this, type(tparams));
	return template;
    }

    /** Get type at phase id */
    public Type typeAt(int id) {
	return type();
    }

    public Type[] closure() {
	if (kind == ALIAS) return info().symbol().closure();
	int id = currentPhaseId();
	if (closures.limit < id) {
	    if (id <= definedPhaseId() || changes(closureAt(id - 1))) {
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
	Phase savedPhase = Global.instance.currentPhase;
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
	// todo: why can't we do: inclClosure(SymSet.EMPTY, this) ?
	//System.out.println("computing closure of " + this);//DEBUG
	SymSet closureClassSet = inclClosure(SymSet.EMPTY, type().parents());
	Symbol[] closureClasses = new Symbol[closureClassSet.size() + 1];
	closureClasses[0] = this;
	closureClassSet.copyToArray(closureClasses, 1);
	//System.out.println(ArrayApply.toString(closureClasses));//DEBUG
	closures.closure = Symbol.type(closureClasses);
	//System.out.println("closure(" + this + ") = " + ArrayApply.toString(closures.closure));//DEBUG
	adjustType(type());
	//System.out.println("closure(" + this + ") = " + ArrayApply.toString(closures.closure));//DEBUG

    }
    //where

 	private SymSet inclClosure(SymSet set, Type[] tps) {
	    for (int i = 0; i < tps.length; i++) {
		Type tp = tps[i].unalias();
		switch (tp) {
		case CompoundType(Type[] parents, _):
		    set = inclClosure(set, parents);
		    break;
		default:
		    set = inclClosure(set, tp.symbol());
		}
	    }
	    return set;
	}

 	private SymSet inclClosure(SymSet set, Symbol c) {
	    Symbol c1 = c;
	    while (c1.kind == ALIAS) c1 = c1.info().symbol();
	    return inclClosure(set.incl(c1), c1.type().parents());
	}

	void adjustType(Type tp) {
	    Type tp1 = tp.unalias();
	    switch (tp) {
	    case CompoundType(Type[] parents, _):
		break;
	    default:
		int pos = closurePos(tp1.symbol());
		assert pos >= 0 : this + " " + tp1 + " " + tp1.symbol();
		closures.closure[pos] = tp1;
	    }
	    Type[] parents = tp1.parents();
	    for (int i = 0; i < parents.length; i++) {
		adjustType(parents[i]);
	    }
	}

    public void reset(Type completer) {
	super.reset(completer);
	closures = ClosureIntervalList.EMPTY;
	tycon = null;
	template = null;
    }
}

public class AliasTypeSymbol extends TypeSymbol {

    /** Constructor */
    public AliasTypeSymbol(int pos, Name name, Symbol owner, int flags) {
        super(ALIAS, pos, name, owner, flags);
    }

    public static AliasTypeSymbol define(
	int pos, Name name, Symbol owner, int flags, Scope scope) {
	Scope.Entry e = scope.lookupEntry(name);
	if (e.owner == scope && e.sym.isExternal() && e.sym.kind == ALIAS) {
	    AliasTypeSymbol sym = (AliasTypeSymbol) e.sym;
	    sym.update(pos, flags);
	    return sym;
	} else {
	    return new AliasTypeSymbol(pos, name, owner, flags);
	}
    }

    /** Return a fresh symbol with the same fields as this one.
     */
    public Symbol cloneSymbol(Symbol owner) {
        AliasTypeSymbol other = new AliasTypeSymbol(pos, name, owner, flags);
        other.setInfo(info());
	copyConstructorInfo(other);
        return other;
    }
}

public class AbsTypeSymbol extends TypeSymbol {

    private Type lobound = null;

    /** Constructor */
    public AbsTypeSymbol(int pos, Name name, Symbol owner, int flags) {
        super(TYPE, pos, name, owner, flags);
    }

    public static AbsTypeSymbol define(
	int pos, Name name, Symbol owner, int flags, Scope scope) {
	Scope.Entry e = scope.lookupEntry(name);
	if (e.owner == scope && e.sym.isExternal() && e.sym.kind == TYPE) {
	    AbsTypeSymbol sym = (AbsTypeSymbol) e.sym;
	    sym.update(pos, flags);
	    return sym;
	} else {
	    return new AbsTypeSymbol(pos, name, owner, flags);
	}
    }

    /** Return a fresh symbol with the same fields as this one.
     */
    public Symbol cloneSymbol(Symbol owner) {
        TypeSymbol other = new AbsTypeSymbol(pos, name, owner, flags);
        other.setInfo(info());
	other.setLoBound(loBound());
        return other;
    }

    /** copy all fields to `sym'
     */
    public void copyTo(Symbol sym) {
	super.copyTo(sym);
	((AbsTypeSymbol) sym).lobound = lobound;
    }

    public Type loBound() {
	initialize();
	return lobound == null ? Global.instance.definitions.ALL_TYPE : lobound;
    }

    public Symbol setLoBound(Type lobound) {
	this.lobound = lobound;
	return this;
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

    /** The given type of self, or NoType, if no explicit type was given.
     */
    private Symbol thisSym = this;

    public Symbol thisSym() { return thisSym; }

    /** A cache for this.thisType()
     */
    final private Type thistp = Type.ThisType(this);

    /** Principal Constructor
     */
    public ClassSymbol(int pos, Name name, Symbol owner, int flags) {
        super(CLASS, pos, name, owner, flags);
        this.mangled = name;
    }

    public static ClassSymbol define(
	int pos, Name name, Symbol owner, int flags, Scope scope) {
	Scope.Entry e = scope.lookupEntry(name);
	if (e.owner == scope && e.sym.isExternal() && e.sym.kind == CLASS) {
	    ClassSymbol sym = (ClassSymbol) e.sym;
	    sym.update(pos, flags);
	    return sym;
	} else {
	    return new ClassSymbol(pos, name, owner, flags);
	}
    }

    /** Constructor for classes to load as source files
     */
    public ClassSymbol(Name name, Symbol owner, SourceCompleter parser) {
	this(Position.NOPOS, name, owner, 0);
	this.module = TermSymbol.newCompanionModule(this, 0, parser);
        this.setInfo(parser);
    }

    /** Constructor for classes to load as class files.
     */
    public ClassSymbol(Name name, Symbol owner, ClassParser parser) {
	this(Position.NOPOS, name, owner, JAVA);
	this.module = TermSymbol.newCompanionModule(this, JAVA, parser.staticsParser(this));
        this.setInfo(parser);
    }

    /** Return a fresh symbol with the same fields as this one.
     */
    public Symbol cloneSymbol(Symbol owner) {
        ClassSymbol other = new ClassSymbol(pos, name, owner, flags);
	other.module = module;
        other.setInfo(info());
	copyConstructorInfo(other);
	other.mangled = mangled;
	if (thisSym != this) other.setTypeOfThis(typeOfThis());
        return other;
    }

    /** copy all fields to `sym'
     */
    public void copyTo(Symbol sym) {
	super.copyTo(sym);
	if (thisSym != this) sym.setTypeOfThis(typeOfThis());
    }

   /** Get module */
    public Symbol module() {
        return module;
    }

    /** Set module; only used internally from TermSymbol
     */
    void setModule(Symbol module) { this.module = module; }

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
	    Symbol tc = enclToplevelClass();
	    if (tc != this) {
		return Name.fromString(
		    enclToplevelClass().mangledFullName() + "$" + mangled);
	    } else {
		return mangled;
	    }
	}
    }

    public Type thisType() {
	return thistp;
    }

    public Type typeOfThis() {
	return thisSym.type();
    }

    public Symbol setTypeOfThis(Type tp) {
	thisSym = new TermSymbol(this.pos, Names.this_, this, SYNTHETIC);
	thisSym.setInfo(tp);
	return this;
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
	    } while (sym.kind != VAL || (sym.flags & CASEACCESSOR) == 0 || !sym.isMethod());
	}
	//System.out.println(this + ", case field[" + index + "] = " + sym);//DEBUG
	assert sym != null : this;
	return sym;
    }

    public void reset(Type completer) {
	super.reset(completer);
	module().reset(completer);
	thisSym = this;
    }
}

/** A class for error symbols.
 */
public final class ErrorSymbol extends Symbol {

    /** Constructor */
    public ErrorSymbol() {
        super(Kinds.ERROR, Position.NOPOS, Name.ERROR, null, INITIALIZED);
        super.setOwner(this);
        super.setInfo(Type.ErrorType);
    }

    public Symbol cloneSymbol(Symbol owner) {
        assert owner == this : Debug.show(owner);
	return this;
    }

    /** Set the mangled name of this Symbol */
    public Symbol mangled(Name name) {
        return this;
    }

    /** Set owner */
    public Symbol setOwner(Symbol owner) {
        assert owner == this : Debug.show(owner);
        return this;
    }

    /** Set type */
    public Symbol setInfo(Type info) {
        assert info == Type.ErrorType : info;
        return this;
    }

    /** Get primary constructor */
    public Symbol primaryConstructor() {
	return TermSymbol.newConstructor(this, 0).setInfo(Type.ErrorType);
    }

    /** Return the next enclosing class */
    public Symbol enclClass() {
        return this;
    }

    /** Return the next enclosing method */
    public Symbol enclMethod() {
        return this;
    }

    public Type loBound() {
	return Type.ErrorType;
    }

    public void reset(Type completer) {
    }
}

/** The class of Symbol.NONE
 */
public final class NoSymbol extends Symbol {

    /** Constructor */
    public NoSymbol() {
        super(Kinds.NONE, Position.NOPOS, Names.NOSYMBOL, null, INITIALIZED);
        super.setOwner(this);
        super.setInfo(Type.NoType);
    }

    /** Return a fresh symbol with the same fields as this one.
     */
    public Symbol cloneSymbol(Symbol owner) {
        assert owner == this : Debug.show(owner);
        return this;
    }

    /** Set the mangled name of this Symbol */
    public Symbol mangled(Name name) {
        throw new ApplicationError("illegal operation on " + getClass());
    }

    /** Set owner */
    public Symbol setOwner(Symbol owner) {
        assert owner == this : Debug.show(owner);
        return this;
    }

    /** Set type */
    public Symbol setInfo(Type info) {
        assert info == Type.NoType : info;
        return this;
    }

    /** Return the next enclosing class */
    public Symbol enclClass() {
        return this;
    }

    /** Return the next enclosing method */
    public Symbol enclMethod() {
        return this;
    }

    public Symbol owner() {
	throw new ApplicationError();
    }

    public void reset(Type completer) {
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

