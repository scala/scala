/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

//todo check significance of JAVA flag.

package scalac.symtab;

import scala.tools.util.Position;

import scalac.ApplicationError;
import scalac.Global;
import scalac.Phase;
import scalac.framework.History;
import scalac.util.ArrayApply;
import scalac.util.Name;
import scalac.util.Names;
import scalac.util.NameTransformer;
import scalac.util.Debug;


public abstract class Symbol implements Modifiers, Kinds {

    /** An empty symbol array */
    public static final Symbol[] EMPTY_ARRAY = new Symbol[0];

    /** An empty array of symbol arrays */
    public static final Symbol[][] EMPTY_ARRAY_ARRAY = new Symbol[0][];

    /** The absent symbol */
    public static final Symbol NONE = new NoSymbol();

// Attribues -------------------------------------------------------------

    public static final int IS_ROOT         = 0x00000001;
    public static final int IS_ANONYMOUS    = 0x00000002;
    public static final int IS_LABEL        = 0x00000010;
    public static final int IS_CONSTRUCTOR  = 0x00000020;
    public static final int IS_ACCESSMETHOD = 0x00000100;
    public static final int IS_ERROR        = 0x10000000;
    public static final int IS_THISTYPE     = 0x20000000;
    public static final int IS_LOCALDUMMY   = 0x40000000;
    public static final int IS_COMPOUND     = 0x80000000;

// Fields -------------------------------------------------------------

    /** The unique identifier generator */
    private static int ids;

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
    private TypeIntervalList infos;

    /** The attributes of the symbol */
    private final int attrs;

    /** The unique identifier */
    public final int id;


// Constructors -----------------------------------------------------------

    /** Generic symbol constructor */
    public Symbol(int kind, Symbol owner, int pos, int flags, Name name, int attrs) {
        this.kind = kind;
        this.pos = pos;
        this.name = name;
        this.owner = owner == null ? this : owner;
        this.flags = flags & ~(INITIALIZED | LOCKED); // safety first
        this.attrs = attrs;
        this.id = ids++;
    }

// Factories --------------------------------------------------------------

    /** Creates a new term owned by this symbol. */
    public final Symbol newTerm(int pos, int flags, Name name) {
        return newTerm(pos, flags, name, 0);
    }

    /** Creates a new constructor of this symbol. */
    public final Symbol newConstructor(int pos, int flags) {
        assert isType(): Debug.show(this);
        return new ConstructorSymbol(this, pos, flags);
    }

    /** Creates a new method owned by this symbol. */
    public final Symbol newMethod(int pos, int flags, Name name) {
        assert isClass(): Debug.show(this);
        return newTerm(pos, flags, name, 0);
    }

    /** Creates a new access method owned by this symbol. */
    public final Symbol newAccessMethod(int pos, Name name) {
        assert isClass(): Debug.show(this);
        int flags = PRIVATE | FINAL | SYNTHETIC;
        return newTerm(pos, flags, name, IS_ACCESSMETHOD);
    }

    /** Creates a new function owned by this symbol. */
    public final Symbol newFunction(int pos, int flags, Name name) {
        assert isTerm(): Debug.show(this);
        return newTerm(pos, flags, name, 0);
    }

    /** Creates a new method or function owned by this symbol. */
    public final Symbol newMethodOrFunction(int pos, int flags, Name name){
        assert isClass() || isTerm(): Debug.show(this);
        return newTerm(pos, flags, name, 0);
    }

    /** Creates a new label owned by this symbol. */
    public final Symbol newLabel(int pos, Name name) {
        assert isTerm(): Debug.show(this);
        return newTerm(pos, 0, name, IS_LABEL);
    }

    /** Creates a new field owned by this symbol. */
    public final Symbol newField(int pos, int flags, Name name) {
        assert isClass(): Debug.show(this);
        return newTerm(pos, flags, name, 0);
    }

    /** Creates a new variable owned by this symbol. */
    public final Symbol newVariable(int pos, int flags, Name name) {
        assert isTerm(): Debug.show(this);
        return newTerm(pos, flags, name, 0);
    }

    /** Creates a new variable owned by this symbol. */
    public final Symbol newFieldOrVariable(int pos, int flags, Name name) {
        assert isClass() || isTerm(): Debug.show(this);
        return newTerm(pos, flags, name, 0);
    }

    /** Creates a new pattern variable owned by this symbol. */
    public final Symbol newPatternVariable(int pos, Name name) {
        return newVariable(pos, 0, name);
    }

    /** Creates a new value parameter owned by this symbol. */
    public final Symbol newVParam(int pos, int flags, Name name) {
        assert isTerm(): Debug.show(this);
        return newTerm(pos, flags | PARAM, name);
    }

    /**
     * Creates a new value parameter owned by this symbol and
     * initializes it with the type.
     */
    public final Symbol newVParam(int pos, int flags, Name name, Type type) {
        Symbol tparam = newVParam(pos, flags, name);
        tparam.setInfo(type);
        return tparam;
    }

    /**
     * Creates a new initialized dummy symbol for template of this
     * class.
     */
    public final Symbol newLocalDummy() {
        assert isClass(): Debug.show(this);
        Symbol local = newTerm(pos, 0, Names.LOCAL(this), IS_LOCALDUMMY);
        local.setInfo(Type.NoType);
        return local;
    }

    /** Creates a new module owned by this symbol. */
    public final Symbol newModule(int pos, int flags, Name name) {
        return new ModuleSymbol(this, pos, flags, name);
    }

    /**
     * Creates a new package owned by this symbol and initializes it
     * with an empty scope.
     */
    public final Symbol newPackage(int pos, Name name) {
        return newPackage(pos, name, null);
    }

    /**
     * Creates a new package owned by this symbol, initializes it with
     * the loader and enters it in the scope if it's non-null.
     */
    public final Symbol newLoadedPackage(Name name, SymbolLoader loader,
        Scope scope)
    {
        assert loader != null: Debug.show(this) + " - " + name;
        Symbol peckage = newPackage(Position.NOPOS, name, loader);
        if (scope != null) scope.enterNoHide(peckage);
        return peckage;
    }

    /**
     * Creates a new error value owned by this symbol and initializes
     * it with an error type.
     */
    public Symbol newErrorValue(Name name) {
        Symbol symbol = newTerm(pos, SYNTHETIC, name, IS_ERROR);
        symbol.setInfo(Type.ErrorType);
        return symbol;
    }

    /** Creates a new type alias owned by this symbol. */
    public final Symbol newTypeAlias(int pos, int flags, Name name) {
        return new AliasTypeSymbol(this, pos, flags, name, 0);
    }

    /** Creates a new abstract type owned by this symbol. */
    public final Symbol newAbstractType(int pos, int flags, Name name) {
        return new AbsTypeSymbol(this, pos, flags, name, 0);
    }

    /** Creates a new type parameter owned by this symbol. */
    public final Symbol newTParam(int pos, int flags, Name name) {
        assert isTerm(): Debug.show(this);
        return newAbstractType(pos, flags | PARAM, name);
    }

    /**
     * Creates a new type parameter owned by this symbol and
     * initializes it with the type.
     */
    public final Symbol newTParam(int pos, int flags, Name name, Type type) {
        Symbol tparam = newTParam(pos, flags, name);
        tparam.setInfo(type);
        return tparam;
    }

    /**
     * Creates a new type alias owned by this symbol and initializes
     * it with the info.
     */
    public final Symbol newTypeAlias(int pos, int flags, Name name, Type info){
        Symbol alias = newTypeAlias(pos, flags, name);
        alias.setInfo(info);
        alias.allConstructors().setInfo(Type.MethodType(EMPTY_ARRAY, info));
        return alias;
    }

    /** Creates a new class owned by this symbol. */
    public final ClassSymbol newClass(int pos, int flags, Name name) {
        return newClass(pos, flags, name, 0);
    }

    /** Creates a new anonymous class owned by this symbol. */
    public final ClassSymbol newAnonymousClass(int pos, Name name) {
        assert isTerm(): Debug.show(this);
        return newClass(pos, 0, name, IS_ANONYMOUS);
    }

    /**
     * Creates a new class with a linked module, both owned by this
     * symbol, initializes them with the loader and enters the class
     * and the module in the scope if it's non-null.
     */
    public final ClassSymbol newLoadedClass(int flags, Name name,
        SymbolLoader loader, Scope scope)
    {
        assert isPackageClass(): Debug.show(this);
        assert loader != null: Debug.show(this) + " - " + name;
        ClassSymbol clasz = new LinkedClassSymbol(this, flags, name);
        clasz.setInfo(loader);
        clasz.allConstructors().setInfo(loader);
        clasz.linkedModule().setInfo(loader);
        clasz.linkedModule().moduleClass().setInfo(loader);
        if (scope != null) scope.enterNoHide(clasz);
        if (scope != null) scope.enterNoHide(clasz.linkedModule());
        return clasz;
    }

    /**
     * Creates a new error class owned by this symbol and initializes
     * it with an error type.
     */
    public ClassSymbol newErrorClass(Name name) {
        ClassSymbol symbol = newClass(pos, SYNTHETIC, name, IS_ERROR);
        Scope scope = new ErrorScope(this);
        symbol.setInfo(Type.compoundType(Type.EMPTY_ARRAY, scope, this));
        symbol.allConstructors().setInfo(Type.ErrorType);
        return symbol;
    }

    /** Creates a new term owned by this symbol. */
    final Symbol newTerm(int pos, int flags, Name name, int attrs) {
        return new TermSymbol(this, pos, flags, name, attrs);
    }

    /** Creates a new package owned by this symbol. */
    final Symbol newPackage(int pos, Name name, Type info) {
        assert isPackageClass(): Debug.show(this);
        Symbol peckage = newModule(pos, JAVA | PACKAGE, name);
        if (info == null) info = Type.compoundType(
            Type.EMPTY_ARRAY, new Scope(), peckage.moduleClass());
        peckage.moduleClass().setInfo(info);
        return peckage;
    }

    /** Creates a new class owned by this symbol. */
    final ClassSymbol newClass(int pos, int flags, Name name, int attrs) {
        return new ClassSymbol(this, pos, flags, name, attrs);
    }

    /** Creates a new compound class owned by this symbol. */
    final ClassSymbol newCompoundClass(Type info) {
        int pos = Position.FIRSTPOS;
        Name name = Names.COMPOUND_NAME.toTypeName();
        int flags = ABSTRACT | SYNTHETIC;
        int attrs = IS_COMPOUND;
        ClassSymbol clasz = newClass(pos, flags, name, attrs);
        clasz.setInfo(info);
        clasz.primaryConstructor().setInfo(
            Type.MethodType(Symbol.EMPTY_ARRAY, clasz.typeConstructor()));
        return clasz;
    }

// Copying & cloning ------------------------------------------------------

    /** Return a fresh symbol with the same fields as this one.
     */
    public final Symbol cloneSymbol() {
        return cloneSymbol(owner);
    }

    /** Return a fresh symbol with the same fields as this one and the
     * given owner.
     */
    public final Symbol cloneSymbol(Symbol owner) {
        Symbol clone = cloneSymbolImpl(owner, attrs);
        clone.setInfo(info());
        return clone;
    }

    protected abstract Symbol cloneSymbolImpl(Symbol owner, int attrs);

    /** Returns a shallow copy of the given array. */
    public static Symbol[] cloneArray(Symbol[] array) {
        return cloneArray(0, array, 0);
    }

    /**
     * Returns a shallow copy of the given array prefixed by "prefix"
     * null items.
     */
    public static Symbol[] cloneArray(int prefix, Symbol[] array) {
        return cloneArray(prefix, array, 0);
    }

    /**
     * Returns a shallow copy of the given array suffixed by "suffix"
     * null items.
     */
    public static Symbol[] cloneArray(Symbol[] array, int suffix) {
        return cloneArray(0, array, suffix);
    }

    /**
     * Returns a shallow copy of the given array prefixed by "prefix"
     * null items and suffixed by "suffix" null items.
     */
    public static Symbol[] cloneArray(int prefix, Symbol[] array, int suffix) {
        assert prefix >= 0 && suffix >= 0: prefix + " - " + suffix;
        int size = prefix + array.length + suffix;
        if (size == 0) return EMPTY_ARRAY;
        Symbol[] clone = new Symbol[size];
        for (int i = 0; i < array.length; i++) clone[prefix + i] = array[i];
        return clone;
    }

    /** Returns the concatenation of the two arrays. */
    public static Symbol[] concat(Symbol[] array1, Symbol[] array2) {
        if (array1.length == 0) return array2;
        if (array2.length == 0) return array1;
        Symbol[] clone = cloneArray(array1.length, array2);
        for (int i = 0; i < array1.length; i++) clone[i] = array1[i];
        return clone;
    }

// Setters ---------------------------------------------------------------

    /** Set owner */
    public Symbol setOwner(Symbol owner) {
        assert !isConstructor() && !isNone() && !isError(): Debug.show(this);
        setOwner0(owner);
        return this;
    }
    protected void setOwner0(Symbol owner) {
        this.owner = owner;
    }

    /** Set type -- this is an alias for setInfo(Type info) */
    public final Symbol setType(Type info) { return setInfo(info); }

    /**
     * Set initial information valid from start of current phase. This
     * information is visible in the current phase and will be
     * transformed by the current phase (except if current phase is
     * the first one).
     */
    public Symbol setInfo(Type info) {
        return setInfoAt(info, Global.instance.currentPhase);
    }

    /**
     * Set initial information valid from start of given phase. This
     * information is visible in the given phase and will be
     * transformed by the given phase.
     */
    private final Symbol setInfoAt(Type info, Phase phase) {
        assert info != null: Debug.show(this);
        assert phase != null: Debug.show(this);
        assert !isConstructor()
            || info instanceof Type.LazyType
            || info == Type.NoType
            || info == Type.ErrorType
            || info instanceof Type.MethodType
            || info instanceof Type.OverloadedType
            || info instanceof Type.PolyType
            : "illegal type for " + this + ": " + info;
        infos = new TypeIntervalList(null, info, phase);
        if (info instanceof Type.LazyType) flags &= ~INITIALIZED;
        else flags |= INITIALIZED;
        return this;
    }

    /**
     * Set new information valid from start of next phase. This
     * information is only visible in next phase or through
     * "nextInfo". It will not be transformed by the current phase.
     */
    public final Symbol updateInfo(Type info) {
        return updateInfoAt(info, Global.instance.currentPhase.next);
    }

    /**
     * Set new information valid from start of given phase. This
     * information is only visible from the start of the given phase
     * which is also the first phase that will transform this
     * information.
     */
    private final Symbol updateInfoAt(Type info, Phase phase) {
        assert info != null: Debug.show(this);
        assert phase != null: Debug.show(this);
        assert infos != null: Debug.show(this);
        assert !phase.precedes(infos.limit()) :
            Debug.show(this) + " -- " + phase + " -- " + infos.limit();
        if (infos.limit() == phase) {
            if (infos.start == phase)
                infos = infos.prev;
            else
                infos.setLimit(infos.limit().prev);
        }
        infos = new TypeIntervalList(infos, info, phase);
        return this;
    }

    /** Set type of `this' in current class
     */
    public Symbol setTypeOfThis(Type tp) {
        throw new ApplicationError(this + ".setTypeOfThis");
    }

    /** Set the low bound of this type variable
     */
    public Symbol setLoBound(Type lobound) {
        throw new ApplicationError("setLoBound inapplicable for " + this);
    }

    /** Set the view bound of this type variable
     */
    public Symbol setVuBound(Type lobound) {
        throw new ApplicationError("setVuBound inapplicable for " + this);
    }

    /** Add an auxiliary constructor to class.
     */
    public void addConstructor(Symbol constr) {
        throw new ApplicationError("addConstructor inapplicable for " + this);
    }

// Symbol classification ----------------------------------------------------

    /** Does this symbol denote an error symbol? */
    public final boolean isError() {
        return (attrs & IS_ERROR) != 0;
    }

    /** Does this symbol denote the none symbol? */
    public final boolean isNone() {
        return kind == Kinds.NONE;
    }

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
	    ((flags & DEF) == 0) &&
            ((flags & STABLE) != 0 ||
             (flags & MUTABLE) == 0 && type().isObjectType());
    }

    /** Does this symbol have the STABLE flag? */
    public final boolean hasStableFlag() {
        return (flags & STABLE) != 0;
    }

    /** Is this symbol static (i.e. with no outer instance)? */
    public final boolean isStatic() {
        return isRoot() || owner.isStaticOwner();
    }

    /** Does this symbol denote a class that defines static symbols? */
    public final boolean isStaticOwner() {
        return isPackageClass() || (isStatic() && isModuleClass()
            // !!! remove later? translation does not work (yet?)
            && isJava());
    }

    /** Is this symbol final?
     */
    public final boolean isFinal() {
	return
	    (flags & (FINAL | PRIVATE)) != 0 || isLocal() || owner.isModuleClass();
    }

    /** Does this symbol denote a variable? */
    public final boolean isVariable() {
        return kind == VAL && (flags & MUTABLE) != 0;
    }

    /** Does this symbol denote a view bounded type variable? */
    public final boolean isViewBounded() {
	Global global = Global.instance;
        return kind == TYPE && (flags & VIEWBOUND) != 0 &&
	    global.currentPhase.id <= global.PHASE.REFCHECK.id();
    }

    /**
     * Does this symbol denote a final method? A final method is one
     * that can't be overridden in a subclass. This method assumes
     * that this symbol denotes a method. It doesn't test it.
     */
    public final boolean isMethodFinal() {
        return (flags & FINAL) != 0 || isPrivate() || isLifted();
    }

    /** Does this symbol denote a sealed class symbol? */
    public final boolean isSealed() {
        return (flags & SEALED) != 0;
    }

    /** Does this symbol denote a method?
     */
    public final boolean isInitializedMethod() {
        if (infos == null) return false;
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
        return kind == CLASS && (flags & ABSTRACT) != 0 &&
            this != Global.instance.definitions.ARRAY_CLASS;
    }

    public final boolean isAbstractOverride() {
        preInitialize();
        return (flags & (ABSTRACT | OVERRIDE)) == (ABSTRACT | OVERRIDE);
    }

    /* Does this symbol denote an anonymous class? */
    public final boolean isAnonymousClass() {
        return isClass() && (attrs & IS_ANONYMOUS) != 0;
    }

    /** Does this symbol denote the root class or root module?
     */
    public final boolean isRoot() {
        return (attrs & IS_ROOT) != 0;
    }

    /** Does this symbol denote something loaded from a Java class? */
    public final boolean isJava() {
        preInitialize();
        return (flags & JAVA) != 0;
    }

    /** Does this symbol denote a Java package? */
    public final boolean isPackage() {
        return kind == VAL && (flags & PACKAGE) != 0;
    }

    /** Does this symbol denote a Java package class? */
    public final boolean isPackageClass() {
        return kind == CLASS && (flags & PACKAGE) != 0;
    }

    /** Does this symbol denote a module? */
    public final boolean isModule() {
        return kind == VAL && (flags & MODUL) != 0;
    }

    /** Does this symbol denote a module class? */
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

    /** Does this class symbol denote a compound type symbol? */
    public final boolean isCompoundSym() {
        return (attrs & IS_COMPOUND) != 0;
    }

    /** Does this symbol denote a this symbol? */
    public final boolean isThisSym() {
        return (attrs & IS_THISTYPE) != 0;
    }

    /** Does this symbol denote an interface? */
    public final boolean isInterface() {
        info(); // force delayed transformInfos that may change this flag
        return (flags & INTERFACE) != 0;
    }

    /** Does this symbol denote a type alias? */
    public final boolean isTypeAlias() {
        return kind == ALIAS;
    }

    /** Does this symbol denote an abstract type? */
    public final boolean isAbstractType() {
        return kind == TYPE;
    }

    /** Does this symbol denote a class type? */
    public final boolean isClassType() {
        return kind == CLASS;
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

    /** Has this symbol been lifted? */
    public final boolean isLifted() {
        preInitialize();
        return (flags & LIFTED) != 0;
    }

    /** Does this symbol denote a deferred symbol? */
    public final boolean isDeferred() {
        return (flags & DEFERRED) != 0;
    }

    /** Does this symbol denote a synthetic symbol? */
    public final boolean isSynthetic() {
        return (flags & SYNTHETIC) != 0;
    }

    /** Does this symbol denote an accessor? */
    public final boolean isAccessor() {
        return (flags & ACCESSOR) != 0;
    }

    /** Does this symbol denote an access method? (a method to access
     * private of protected members from inner classes) */
    public final boolean isAccessMethod() {
        return (attrs & IS_ACCESSMETHOD) != 0;
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

    /** Is this symbol an instance initializer? */
    public boolean isInitializer() {
        return false;
    }

    /** Is this symbol a constructor? */
    public final boolean isConstructor() {
        return (attrs & IS_CONSTRUCTOR) != 0;
    }

    /** Is this symbol the primary constructor of a type? */
    public final boolean isPrimaryConstructor() {
        return isConstructor() && this == constructorClass().primaryConstructor();
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
        return (attrs & IS_LABEL) != 0;
    }

    /** Is this symbol accessed? */
    public final boolean isAccessed() {
        return (flags & ACCESSED) != 0;
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
        if (isConstructor()) return constructorClass().name.toTermName();
        return name;
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

    /** Get result type */
    public final Type resultType() {
        return type().resultType();
    }

    /** Get type parameters at start of next phase */
    public final Symbol[] nextTypeParams() {
        Global.instance.nextPhase();
        Symbol[] tparams = typeParams();
        Global.instance.prevPhase();
        return tparams;
    }

    /** Get value parameters at start of next phase */
    public final Symbol[] nextValueParams() {
        Global.instance.nextPhase();
        Symbol[] vparams = valueParams();
        Global.instance.prevPhase();
        return vparams;
    }

    /** Get result type at start of next phase */
    public final Type nextResultType() {
        return nextType().resultType();
    }

    /** Get all constructors of class */
    public Symbol allConstructors() {
        return NONE;
    }

    /** Get primary constructor of class */
    public Symbol primaryConstructor() {
        return NONE;
    }

    /**
     * Returns the class linked to this module or null if there is no
     * such class. The returned value remains the same for the whole
     * life of the symbol.
     *
     * See method "linkedModule" to learn more about linked modules
     * and classes.
     */
    public ClassSymbol linkedClass() {
        assert isModule(): "not a module: " + Debug.show(this);
        return null;
    }

    /**
     * Returns the module linked to this class or null if there is no
     * such module. The returned value remains the same for the whole
     * life of the symbol.
     *
     * Linked modules and classes are intended to be used by the
     * symbol table loader. They are needed because it is impossible
     * to know from the name of a class or source file if it defines a
     * class or a module. For that reason a class and a module (each
     * linked to the other) are created for each of those files. Once
     * the file is read the class, the module or both are initialized
     * depending on what the file defines.
     *
     * It is guaranteed that if a class "c" has a linked module then
     * "c.linkedModule().linkedClasss() == c" and that if a module "m"
     * has a linked class then "m.linkedClasss().linkedModule() == m".
     *
     * The linked module of a Java class, is the module that contains
     * the static members of that class. A Java class has always a
     * linked module.
     *
     * The linked module of a Scala class, is the module with the same
     * name declared in the same scope. A Scala class may or may not
     * have a linked module. However, this does not depend on the
     * presence or absence of a module with the same name but on how
     * the class is created. Therefore a Scala class may have no
     * linked module even though there exists a module with the same
     * name in the same scope. A Scala class may also have a linked
     * module even though there exists no module with the same name in
     * the same scope. In the latter case, the linked would be
     * initialized to NoType (which prevents accesses to it).
     *
     * There is a last catch about linked modules. It may happen that
     * the symbol returned by "linkedModule" is not a module (and that
     * method "linkedClass" works on a non-module symbol). At creation
     * time, linked modules are always modules, but at initialization
     * time, it may be discovered that the module is in fact a case
     * class factory method. In that case, the module is downgraded to
     * a non-module term. This implies that from then on calls to its
     * method "moduleClass" will fail, but the links defined by the
     * methods "linkedModule" and "linkedClass" remain unchanged.
     */
    public ModuleSymbol linkedModule() {
        assert isClassType(): "not a class: " + Debug.show(this);
        return null;
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

    /** If this is a constructor, return the class it constructs.
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
        if (infos == null)
            return this;
        else if (infos.info instanceof Type.OverloadedType)
            return infos.info.alternativeSymbols()[0];
        else if (infos.info instanceof LazyOverloadedType)
            return ((LazyOverloadedType) infos.info).sym1.firstAlternative();
        else
            return this;
    }

    /**
     * Returns the class of this module. This method may be invoked
     * only on module symbols. It returns always a non-null module
     * class symbol whose identity never changes.
     */
    public ModuleClassSymbol moduleClass() {
        throw Debug.abort("not a module", this);
    }

    /**
     * Returns the source module of this module class. This method may
     * be invoked only on module class symbols. It returns always a
     * non-null module symbol whose identity never changes.
     *
     * This method should be used with great care. If possible, one
     * should always use moduleClass instead. For example, one should
     * write "m.moduleClass()==c" rather than "m==c.sourceModule()".
     *
     * This method is problematic because the module - module-class
     * relation is not a one - one relation. There might be more than
     * one module that share the same module class. In that case, the
     * source module of the module class is the module that created
     * the class. This implies that "m.moduleClass().sourceModule()"
     * may be different of "m". However, its is guaranteed that
     * "c.sourceModule().moduleClass()" always returns "c".
     *
     * Phases like "AddInterfaces" and "ExpandMixins" are two examples
     * of phases that create additional modules referring the same
     * module class.
     *
     * Even if a module class is related to only one module, the use
     * of this method may still be dangerous. The problem is that
     * modules and module classes are not always as related as one
     * might expect. For example, modules declared in a function are
     * lifted out of the function by phase "LambdaLift". During this
     * process, the module value is transformed into a module method
     * with a "Ref" argument. If the "sourceModule" method is used to
     * replace references to module classes by references to their
     * source modules and this is done it naively with the class of a
     * lifted module, it will yield wrong code because the the "Ref"
     * argument will be missing.
     */
    public ModuleSymbol sourceModule() {
        throw Debug.abort("not a module class", this);
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
        String name1 = name.toString();
        if (name1.endsWith(Names._EQ.toString()))
            name1 = name1.substring(0, name1.length() - Names._EQ.length());
        return owner.lookup(Name.fromString(name1 + "$"));
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

    /** Was symbol's type updated during given phase? */
    public final boolean isUpdatedAt(Phase phase) {
        Phase next = phase.next;
        TypeIntervalList infos = this.infos;
        while (infos != null) {
            if (infos.start == next) return true;
            if (infos.limit().precedes(next)) return false;
            infos = infos.prev;
        }
        return false;
    }

    /** Is this symbol locked? */
    public final boolean isLocked() {
        return (flags & LOCKED) != 0;
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
        if (infos.info instanceof SymbolLoader)
            infos.info.complete(this);
    }

    /** Get info at start of current phase; This is:
     *  for a term symbol, its type
     *  for a type variable, its bound
     *  for a type alias, its right-hand side
     *  for a class symbol, the compound type consisting of
     *  its baseclasses and members.
     */
    public final Type info() {
        //if (isModule()) moduleClass().initialize();
        if ((flags & INITIALIZED) == 0) {
            Global global = Global.instance;
            Phase current = global.currentPhase;
            global.currentPhase = rawFirstInfoStartPhase();
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
                assert !(rawInfo() instanceof Type.LazyType) : this;
                //flags |= INITIALIZED;
            }
            //System.out.println("done: " + this);//DEBUG
            global.currentPhase = current;
        }
        return rawInfo();
    }

    /** Get info at start of next phase
     */
    public final Type nextInfo() {
        Global.instance.nextPhase();
        Type info = info();
        Global.instance.prevPhase();
        return info;
    }

    /** Get info at start of given phase
     */
    protected final Type infoAt(Phase phase) {
        Global global = phase.global;
        Phase current = global.currentPhase;
        global.currentPhase = phase;
        Type info = info();
        global.currentPhase = current;
        return info;
    }

    /** Get info at start of current phase, without forcing lazy types.
     */
    public final Type rawInfo() {
        return rawInfoAt(Global.instance.currentPhase);
    }

    /** Get info at start of next phase, without forcing lazy types.
     */
    public final Type rawNextInfo() {
        Global.instance.nextPhase();
        Type info = rawInfo();
        Global.instance.prevPhase();
        return info;
    }

    /** Get info at start of given phase, without forcing lazy types.
     */
    private final Type rawInfoAt(Phase phase) {
        //if (infos == null) return Type.NoType;//DEBUG
        assert infos != null : this;
        assert phase != null : this;
        if (infos.limit().id <= phase.id) {
            switch (infos.info) {
            case LazyType():
                // don't force lazy types
                return infos.info;
            }
            while (infos.limit() != phase) {
                Phase limit = infos.limit();
                Type info = transformInfo(limit, infos.info);
                assert info != null: Debug.show(this) + " -- " + limit;
                if (info != infos.info) {
                    infos = new TypeIntervalList(infos, info, limit.next);
                } else {
                    infos.setLimit(limit.next);
                }
            }
            return infos.info;
        } else {
            TypeIntervalList infos = this.infos;
            while (phase.id < infos.start.id && infos.prev != null)
                infos = infos.prev;
            return infos.info;
        }
    }
    // where
        private Type transformInfo(Phase phase, Type info) {
            Global global = phase.global;
            Phase current = global.currentPhase;
            switch (info) {
            case ErrorType:
            case NoType:
                return info;
            case OverloadedType(Symbol[] alts, Type[] alttypes):
                global.currentPhase = phase.next;
		int n = 0;
		boolean altChanged = false;
                for (int i = 0; i < alts.length; i++) {
                    Type type = alts[i].info();
		    if (alts[i].owner() == owner()) n++;
		    if (alts[i].info() != alttypes[i]) altChanged = true;
		}
		Type result;
		if (n < alts.length) {
		    Symbol[] symbols = new Symbol[n];
		    Type[] types = new Type[n];
		    int j = 0;
		    for (int i = 0; i < alts.length; i++) {
			if (alts[i].owner() == owner()) {
			    symbols[j] = alts[i];
			    types[j] = alts[i].info();
			    j++;
			} else
			    if (Global.instance.debug) Global.instance.log("removing inherited alternatve " + alts[i] + ":" + alttypes[i]);//debug
		    }
		    result = Type.OverloadedType(symbols, types);
		} else if (altChanged) {
		    Type[] types = new Type[alttypes.length];
		    for (int i = 0; i < alts.length; i++) {
			types[i] = alts[i].info();
		    }
		    result = Type.OverloadedType(alts, types);
		} else {
		    result = info;
		}
		global.currentPhase = current;
		return result;
            default:
                global.currentPhase = phase;
                info = phase.transformInfo(this, info);
                global.currentPhase = current;
                return info;
            }
        }

    /** Get first defined info, without forcing lazy types.
     */
    public final Type rawFirstInfo() {
        TypeIntervalList infos = this.infos;
        assert infos != null : this;
        while (infos.prev != null) infos = infos.prev;
        return infos.info;
    }

    /** Get phase that first defined an info, without forcing lazy types.
     */
    public final Phase rawFirstInfoStartPhase() {
        TypeIntervalList infos = this.infos;
        assert infos != null : this;
        while (infos.prev != null) infos = infos.prev;
        return infos.start;
    }

    /** Get type at start of current phase. The type of a symbol is:
     *  for a type symbol, the type corresponding to the symbol itself
     *  for a term symbol, its usual type
     */
    public Type type() {
        return info();
    }
    public Type getType() {
        return info();
    }

    /** Get type at start of next phase
     */
    public final Type nextType() {
        Global.instance.nextPhase();
        Type type = type();
        Global.instance.prevPhase();
        return type;
    }

    /** The infos of these symbols as an array.
     */
    static public Type[] info(Symbol[] syms) {
        Type[] tps = new Type[syms.length];
        for (int i = 0; i < syms.length; i++)
            tps[i] = syms[i].info();
        return tps;
    }

    /** The types of these symbols as an array.
     */
    static public Type[] type(Symbol[] syms) {
        Type[] tps = new Type[syms.length];
        for (int i = 0; i < syms.length; i++)
            tps[i] = syms[i].type();
        return tps;
    }
    static public Type[] getType(Symbol[] syms) {
	return type(syms);
    }

    /** Get static type. */
    public final Type staticType() {
        return staticType(Type.EMPTY_ARRAY);
    }
    /** Get static type with given type argument. */
    public final Type staticType(Type arg0) {
        return staticType(new Type[]{arg0});
    }
    /** Get static type with given type arguments. */
    public final Type staticType(Type arg0, Type arg1) {
        return staticType(new Type[]{arg0, arg1});
    }
    /** Get static type with given type arguments. */
    public final Type staticType(Type[] args) {
        Type prefix = owner.staticPrefix();
        if (isType()) return Type.typeRef(prefix, this, args);
        assert args.length == 0: Debug.show(this, args);
        return prefix.memberType(this);
    }

    /** Get static prefix. */
    public final Type staticPrefix() {
        assert isStaticOwner(): Debug.show(this) + " - " + isTerm() + " - " + isModuleClass() + " - " + owner().isStaticOwner() + " - " + isJava();
        Global global = Global.instance;
        if (global.PHASE.EXPLICITOUTER.id() < global.currentPhase.id)
            return Type.NoPrefix;
        if (isRoot()) return thisType();
        assert sourceModule().owner() == owner(): Debug.show(this);
        assert sourceModule().type().isObjectType(): Debug.show(this);
        return Type.singleType(owner.staticPrefix(), sourceModule());
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
        return Global.instance.definitions.ALL_TYPE();
    }

    /** The view bound of this type variable
     */
    public Type vuBound() {
        return Global.instance.definitions.ANY_TYPE();
    }

    /** Get this.type corresponding to this symbol
     */
    public Type thisType() {
        return Type.NoPrefix;
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
        return this.id < that.id;
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
            c.isError() ||
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
        return NameTransformer.decode(simpleName());
    }

    /** String representation, including symbol's kind
     *  e.g., "class Foo", "function Bar".
     */
    public String toString() {
        return new SymbolTablePrinter().printSymbolKindAndName(this).toString();
    }

    /** String representation of location.
     */
    public String locationString() {
        if (owner.kind == CLASS &&
            !owner.isAnonymousClass() && !owner.isCompoundSym() ||
            Global.instance.debug)
            return " in " +
                (owner.isModuleClass() ? owner.sourceModule() : owner);
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
        //assert this.owner == that.owner : Debug.show(this) + " != " + Debug.show(that);
        assert this.isConstructor() == that.isConstructor();
        int overflags;
	//if (this.owner == that.owner)
	    overflags = (this.flags & that.flags &
			 (JAVA | ACCESSFLAGS | DEFERRED | PARAM | SYNTHETIC)) |
		((this.flags | that.flags) & ACCESSOR);
	// else // it's an inherited overloaded alternative
	//    overflags = this.flags & SOURCEFLAGS;
        Symbol overloaded = (this.isConstructor())
            ? this.constructorClass().newConstructor(this.constructorClass().pos, overflags)
            : owner.newTerm(pos, overflags, name, 0);
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

    /**
     * Returns the symbol in type "base" which is overridden by this
     * symbol in class "this.owner()". Returns "NONE" if no such
     * symbol exists. The type "base" must be a supertype of class
     * "this.owner()". If "exact" is true, overriding is restricted to
     * symbols that have the same type. The method may return this
     * symbol only if "base.symbol()" is equal to "this.owner()".
     */
    public final Symbol overriddenSymbol(Type base, boolean exact) {
        return overriddenSymbol(base, owner(), exact);
    }
    public final Symbol overriddenSymbol(Type base) {
        return overriddenSymbol(base, false);
    }

    /**
     * Returns the symbol in type "base" which is overridden by this
     * symbol in "clasz". Returns "NONE" if no such symbol exists. The
     * type "base" must be a supertype of "clasz" and "this.owner()"
     * must be a superclass of "clasz". If "exact" is true, overriding
     * is restricted to symbols that have the same type.  The method
     * may return this symbol if "base.symbol()" is a subclass of
     * "this.owner()".
     */
    public final Symbol overriddenSymbol(Type base, Symbol clasz, boolean exact) {
        Type.Relation relation = exact
            ? Type.Relation.SameType
            : Type.Relation.SuperType;
        return base.lookup(this, clasz.thisType(), relation);
    }
    public final Symbol overriddenSymbol(Type base, Symbol clasz) {
        return overriddenSymbol(base, clasz, false);
    }

    /**
     * Returns the symbol in type "sub" which overrides this symbol in
     * class "sub.symbol()". Returns this symbol if no such symbol
     * exists. The class "sub.symbol()" must be a subclass of
     * "this.owner()". If "exact" is true, overriding is restricted to
     * symbols that have the same type.
     */
    public final Symbol overridingSymbol(Type sub, boolean exact) {
        Type.Relation relation = exact
            ? Type.Relation.SameType
            : Type.Relation.SubType;
        return sub.lookup(this, sub, relation);
    }
    public final Symbol overridingSymbol(Type sub) {
        return overridingSymbol(sub, false);
    }

    /** Does this symbol override that symbol?
     */
    public boolean overrides(Symbol that) {
        return
            ((this.flags | that.flags) & PRIVATE) == 0 &&
            this.name == that.name &&
            owner.thisType().memberType(this).derefDef().isSubType(
                owner.thisType().memberType(that).derefDef());
    }

    /** Reset symbol to initial state
     */
    public void reset(Type completer) {
        this.flags &= SOURCEFLAGS;
        this.pos = 0;
        this.infos = null;
        this.setInfo(completer);
    }

    /**
     * Returns the symbol to use in case of a rebinding due to a more
     * precise type prefix.
     */
    public Symbol rebindSym() {
        return this;
    }

    /** return a tag which (in the ideal case) uniquely identifies
     *  class symbols
     */
    public int tag() {
        return name.toString().hashCode();
    }

    public void addInheritedOverloaded(Type owntype) {
	if (owner().kind == CLASS && !isConstructor() && owner().lookup(name) == this) {
	    // it's a class member which is not an overloaded alternative
	    Symbol sym = Type.lookupNonPrivate(owner().parents(), name);
	    if (sym.kind == VAL) {
		Type symtype = owner.thisType().memberType(sym);
		switch (symtype) {
		case OverloadedType(Symbol[] alts, Type[] alttypes):
		    for (int i = 0; i < alts.length; i++)
			addInheritedOverloaded(owntype, alts[i], alttypes[i]);
		    break;
		default:
		    addInheritedOverloaded(owntype, sym, symtype);
		}
	    }
	}
    }

    private void addInheritedOverloaded(Type owntype, Symbol sym, Type symtype) {
	if (!owntype.overrides(symtype)) {
	    if (Global.instance.debug) Global.instance.log(owner() + " inherits overloaded: " + sym + ":" + symtype + sym.locationString());//debug
	    owner().members().lookupEntry(name).setSymbol(overloadWith(sym));
	    //System.out.println("type is now: " + owner().members().lookup(name).type());
	}
    }

    public Type removeInheritedOverloaded(Type owntype) {
	switch (owntype) {
	case OverloadedType(Symbol[] alts, Type[] alttypes):
	    int n = 0;
	    for (int i = 0; i < alts.length; i++)
		if (alts[i].owner() == owner()) n++;
	    if (n < alts.length) {
		Symbol[] alts1 = new Symbol[n];
		Type[] alttypes1 = new Type[n];
		int j = 0;
		for (int i = 0; i < alts.length; i++) {
		    if (alts[i].owner() == owner()) {
			alts1[j] = alts[i];
			alttypes1[j] = alttypes[i];
			j++;
		    } else
			if (Global.instance.debug) Global.instance.log("removing inherited alternatve " + alts[i] + ":" + alttypes[i]);//debug
		}
		return Type.OverloadedType(alts1, alttypes1);
	    } else {
		return owntype;
	    }
	default:
	    return owntype;
	}
    }
}

/** A class for term symbols
 */
public // !!! for java -Xfuture
class TermSymbol extends Symbol {

    /** Constructor */
    TermSymbol(Symbol owner, int pos, int flags, Name name, int attrs) {
        super(VAL, owner, pos, flags, name, attrs);
        assert name.isTermName(): Debug.show(this);
    }

    public boolean isInitializer() {
        return name == Names.INITIALIZER;
    }

    public Symbol[] typeParams() {
        return type().typeParams();
    }

    public Symbol[] valueParams() {
        return type().valueParams();
    }

    protected Symbol cloneSymbolImpl(Symbol owner, int attrs) {
        return new TermSymbol(owner, pos, flags, name, attrs);
    }

}

/** A class for constructor symbols */
final class ConstructorSymbol extends TermSymbol {

    /** The constructed class */
    private final Symbol clasz;

    /** Initializes this instance. */
    ConstructorSymbol(Symbol clasz, int pos, int flags) {
        super(clasz.owner(), pos, flags, Names.CONSTRUCTOR, IS_CONSTRUCTOR);
        this.clasz = clasz;
    }

    public boolean isInitializer() {
        return false;
    }

    public Symbol constructorClass() {
        return clasz;
    }

    protected final Symbol cloneSymbolImpl(Symbol owner, int attrs) {
        throw Debug.abort("illegal clone of constructor", this);
    }

}

/** A class for module symbols */
public class ModuleSymbol extends TermSymbol {

    /** The module class */
    private final ModuleClassSymbol clasz;

    /** Initializes this instance. */
    private ModuleSymbol(Symbol owner, int pos, int flags, Name name,
        int attrs, ModuleClassSymbol clasz)
    {
        super(owner, pos, flags | MODUL | FINAL | STABLE, name, attrs);
        this.clasz = clasz != null ? clasz : new ModuleClassSymbol(this);
        setType(Type.typeRef(owner().thisType(), this.clasz,Type.EMPTY_ARRAY));
    }

    /** Initializes this instance. */
    ModuleSymbol(Symbol owner, int pos, int flags, Name name) {
        this(owner, pos, flags, name, 0, null);
    }

    public ModuleClassSymbol moduleClass() {
        // test may fail because loaded modules may be downgraded to
        // case class factory methods (see Symbol#linkedModule())

        assert isModule(): Debug.show(this);
        return clasz;
    }

    protected final Symbol cloneSymbolImpl(Symbol owner, int attrs) {
        return new ModuleSymbol(owner, pos, flags, name, attrs, clasz);
    }

}

/**
 * A class for linked module symbols
 *
 * @see Symbol#linkedModule()
 */
public // !!! for java -Xfuture
final class LinkedModuleSymbol extends ModuleSymbol {

    /** The linked class */
    private final LinkedClassSymbol clasz;

    /** Initializes this instance. */
    LinkedModuleSymbol(LinkedClassSymbol clasz) {
        super(clasz.owner(), clasz.pos, clasz.flags & JAVA,
            clasz.name.toTermName());
        this.clasz = clasz;
    }

    public ClassSymbol linkedClass() {
        return clasz;
    }

}

/** A base class for all type symbols.
 *  It has AliasTypeSymbol, AbsTypeSymbol, ClassSymbol as subclasses.
 */
public // !!! for java -Xfuture
abstract class TypeSymbol extends Symbol {

     /** The history of closures of this symbol */
    private final History/*<Type[]>*/ closures;

    /** A cache for type constructors
     */
    private Type tycon = null;

    /** The primary constructor of this type */
    private Symbol constructor;

    /** Constructor */
    public TypeSymbol(int kind, Symbol owner, int pos, int flags, Name name, int attrs) {
        super(kind, owner, pos, flags, name, attrs);
        this.closures = new ClosureHistory();
        assert name.isTypeName() : this;
        this.constructor = newConstructor(pos, flags & CONSTRFLAGS);
    }

    protected final void copyConstructorInfo(TypeSymbol other) {
        {
            Type info = primaryConstructor().info().cloneType(
                primaryConstructor(), other.primaryConstructor());
            if (!isTypeAlias()) info = fixConstrType(info, other);
            other.primaryConstructor().setInfo(info);
        }
        Symbol[] alts = allConstructors().alternativeSymbols();
        for (int i = 1; i < alts.length; i++) {
            Symbol constr = other.newConstructor(alts[i].pos, alts[i].flags);
            other.addConstructor(constr);
            Type info = alts[i].info().cloneType(alts[i], constr);
            if (!isTypeAlias()) info = fixConstrType(info, other);
            constr.setInfo(info);
        }
    }

    private final Type fixConstrType(Type type, Symbol clone) {
        switch (type) {
        case MethodType(Symbol[] vparams, Type result):
            result = fixConstrType(result, clone);
            return new Type.MethodType(vparams, result);
        case PolyType(Symbol[] tparams, Type result):
            result = fixConstrType(result, clone);
            return new Type.PolyType(tparams, result);
        case TypeRef(Type pre, Symbol sym, Type[] args):
            if (sym != this && isTypeAlias() && owner().isCompoundSym())
                return type;
            assert sym == this: Debug.show(sym) + " != " + Debug.show(this);
            return Type.typeRef(pre, clone, args);
        case LazyType():
            return type;
        default:
            throw Debug.abort("unexpected constructor type:" + clone + ":" + type);
        }
    }

    /** add a constructor
     */
    public final void addConstructor(Symbol constr) {
        assert constr.isConstructor(): Debug.show(constr);
        constructor = constructor.overloadWith(constr);
    }

    /** Get primary constructor */
    public final Symbol primaryConstructor() {
        return constructor.firstAlternative();
    }

    /** Get all constructors */
    public final Symbol allConstructors() {
        return constructor;
    }

    /** Get type parameters */
    public final Symbol[] typeParams() {
        return primaryConstructor().info().typeParams();
    }

    /** Get value parameters */
    public final Symbol[] valueParams() {
        return (kind == CLASS) ? primaryConstructor().info().valueParams()
            : Symbol.EMPTY_ARRAY;
    }

    /** Get type constructor */
    public final Type typeConstructor() {
        if (tycon == null)
            tycon = Type.typeRef(owner().thisType(), this, Type.EMPTY_ARRAY);
        return tycon;
    }

    public Symbol setOwner(Symbol owner) {
        tycon = null;
        constructor.setOwner0(owner);
        switch (constructor.type()) {
        case OverloadedType(Symbol[] alts, _):
            for (int i = 0; i < alts.length; i++) alts[i].setOwner0(owner);
        }
        return super.setOwner(owner);
    }

    /** Get type */
    public final Type type() {
        return primaryConstructor().type().resultType();
    }
    public final Type getType() {
        return primaryConstructor().type().resultType();
    }

    /**
     * Get closure at start of current phase. The closure of a symbol
     * is a list of types which contains the type of the symbol
     * followed by all its direct and indirect base types, sorted by
     * isLess().
     */
    public final Type[] closure() {
        if (kind == ALIAS) return info().symbol().closure();
        return (Type[])closures.getValue(this);
    }

    public void reset(Type completer) {
        super.reset(completer);
        closures.reset();
        tycon = null;
    }


    protected final Symbol cloneSymbolImpl(Symbol owner, int attrs) {
        TypeSymbol clone = cloneTypeSymbolImpl(owner, attrs);
        copyConstructorInfo(clone);
        return clone;
    }

    protected abstract TypeSymbol cloneTypeSymbolImpl(Symbol owner, int attrs);
}

public // !!! for java -Xfuture
final class AliasTypeSymbol extends TypeSymbol {

    /** Initializes this instance. */
    AliasTypeSymbol(Symbol owner, int pos, int flags, Name name, int attrs) {
        super(ALIAS, owner, pos, flags, name, attrs);
    }

    protected TypeSymbol cloneTypeSymbolImpl(Symbol owner, int attrs) {
        return new AliasTypeSymbol(owner, pos, flags, name, attrs);
    }

}

final class AbsTypeSymbol extends TypeSymbol {

    private Type lobound = null;
    private Type vubound = null;

    /** Initializes this instance. */
    AbsTypeSymbol(Symbol owner, int pos, int flags, Name name, int attrs) {
        super(TYPE, owner, pos, flags, name, attrs);
        allConstructors().setInfo(Type.MethodType(EMPTY_ARRAY, Type.typeRef(owner.thisType(), this, Type.EMPTY_ARRAY)));
    }

    public Type loBound() {
        initialize();
        return lobound == null ? Global.instance.definitions.ALL_TYPE() : lobound;
    }

    public Type vuBound() {
        initialize();
        return !isViewBounded() || vubound == null
	    ? Global.instance.definitions.ANY_TYPE() : vubound;
    }

    public Symbol setLoBound(Type lobound) {
        this.lobound = lobound;
        return this;
    }

    public Symbol setVuBound(Type vubound) {
	this.vubound = vubound;
        return this;
    }

    protected TypeSymbol cloneTypeSymbolImpl(Symbol owner, int attrs) {
        TypeSymbol clone = new AbsTypeSymbol(owner, pos, flags, name, attrs);
        clone.setLoBound(loBound());
        clone.setVuBound(vuBound());
        return clone;
    }

}

/** A class for class symbols. */
public class ClassSymbol extends TypeSymbol {

    /** The given type of self, or NoType, if no explicit type was given.
     */
    private Symbol thisSym = this;

    public Symbol thisSym() { return thisSym; }

    /** A cache for this.thisType()
     */
    final private Type thistp = Type.ThisType(this);

    private final Symbol rebindSym;

    /** Initializes this instance. */
    ClassSymbol(Symbol owner, int pos, int flags, Name name, int attrs) {
        super(CLASS, owner, pos, flags, name, attrs);
        this.rebindSym = owner.newTypeAlias(pos, 0, Names.ALIAS(this));
        Type rebindType = new ClassAliasLazyType();
        this.rebindSym.setInfo(rebindType);
        this.rebindSym.primaryConstructor().setInfo(rebindType);
    }

    private class ClassAliasLazyType extends Type.LazyType {
        public void complete(Symbol ignored) {
            Symbol clasz = ClassSymbol.this;
            Symbol alias = rebindSym;
            Type prefix = clasz.owner().thisType();
            Type constrtype = clasz.type();
            constrtype = Type.MethodType(Symbol.EMPTY_ARRAY, constrtype);
            constrtype = Type.PolyType(clasz.typeParams(), constrtype);
            constrtype = constrtype.cloneType(
                clasz.primaryConstructor(), alias.primaryConstructor());
            alias.primaryConstructor().setInfo(constrtype);
            alias.setInfo(constrtype.resultType());
        }
    }

    /** Creates the root class. */
    public static Symbol newRootClass(Global global) {
        int pos = Position.NOPOS;
        Name name = Names.ROOT.toTypeName();
        Symbol owner = Symbol.NONE;
        int flags = JAVA | PACKAGE | FINAL;
        int attrs = IS_ROOT;
        Symbol clasz = new ClassSymbol(owner, pos, flags, name, attrs);
        clasz.setInfo(global.getRootLoader());
        clasz.primaryConstructor().setInfo(
            Type.MethodType(Symbol.EMPTY_ARRAY, clasz.typeConstructor()));
        // !!! Type.MethodType(Symbol.EMPTY_ARRAY, clasz.thisType()));
        return clasz;
    }

    /** Creates the this-type symbol associated to this class. */
    private final Symbol newThisType() {
        return newTerm(pos, SYNTHETIC, Names.this_, IS_THISTYPE);
    }

    public Type thisType() {
        Global global = Global.instance;
        if (global.currentPhase.id > global.PHASE.ERASURE.id()) return type();
        return thistp;
    }

    public Type typeOfThis() {
        return thisSym.type();
    }

    public Symbol setTypeOfThis(Type tp) {
        thisSym = newThisType();
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
        if ((flags & JAVA) == 0) {
			for (int i = 0; i <= index; i++) {
				do {
					sym = it.next();
				} while (sym.kind != VAL || (sym.flags & CASEACCESSOR) == 0 || !sym.isMethod());
			}
			//System.out.println(this + ", case field[" + index + "] = " + sym);//DEBUG
		} else {
			sym = it.next();
			while ((sym.flags & SYNTHETIC) == 0) {
			    //System.out.println("skipping " + sym);
			    sym = it.next();
			}
			for (int i = 0; i < index; i++)
				sym = it.next();
			//System.out.println("field accessor = " + sym);//DEBUG
		}
		assert sym != null : this;
		return sym;
    }

    public final Symbol rebindSym() {
        return rebindSym;
    }

    public void reset(Type completer) {
        super.reset(completer);
        thisSym = this;
    }

    protected final TypeSymbol cloneTypeSymbolImpl(Symbol owner, int attrs) {
        assert !isModuleClass(): Debug.show(this);
        ClassSymbol clone = new ClassSymbol(owner, pos, flags, name, attrs);
        if (thisSym != this) clone.setTypeOfThis(typeOfThis());
        return clone;
    }

}

/**
 * A class for module class symbols
 *
 * @see Symbol#sourceModule()
 */
public final class ModuleClassSymbol extends ClassSymbol {

    /** The source module */
    private final ModuleSymbol module;

    /** Initializes this instance. */
    ModuleClassSymbol(ModuleSymbol module) {
        super(module.owner(), module.pos,
            (module.flags & MODULE2CLASSFLAGS) | MODUL | FINAL,
            module.name.toTypeName(), 0);
        primaryConstructor().flags |= PRIVATE;
        primaryConstructor().setInfo(
            Type.MethodType(Symbol.EMPTY_ARRAY, typeConstructor()));
        this.module = module;
    }

    public ModuleSymbol sourceModule() {
        return module;
    }

}

/**
 * A class for linked class symbols
 *
 * @see Symbol#linkedModule()
 */
public // !!! for java -Xfuture
final class LinkedClassSymbol extends ClassSymbol {

    /** The linked module */
    private final LinkedModuleSymbol module;

    /** Initializes this instance. */
    LinkedClassSymbol(Symbol owner, int flags, Name name) {
        super(owner, Position.NOPOS, flags, name, 0);
        this.module = new LinkedModuleSymbol(this);
    }

    public ModuleSymbol linkedModule() {
        return module;
    }

}

/** The class of Symbol.NONE
 */
public // !!! for java -Xfuture
final class NoSymbol extends Symbol {

    /** Constructor */
    public NoSymbol() {
        super(Kinds.NONE, null, Position.NOPOS, 0, Names.NOSYMBOL, 0);
        super.setInfo(Type.NoType);
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

    public Type thisType() {
        return Type.NoPrefix;
    }

    public void reset(Type completer) {
    }

    protected Symbol cloneSymbolImpl(Symbol owner, int attrs) {
        throw Debug.abort("illegal clone", this);
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

/** A base class for values indexed by phases. */
public // !!! for java -Xfuture
abstract class IntervalList {

    /** Interval starts at start of phase "start" (inclusive) */
    public final Phase start;
    /** Interval ends at start of phase "limit" (inclusive) */
    private Phase limit;

    public IntervalList(IntervalList prev, Phase start) {
        this.start = start;
        this.limit = start;
        assert start != null && (prev == null || prev.limit.next == start) :
            Global.instance.currentPhase + " - " + prev + " - " + start;
    }

    public Phase limit() {
        return limit;
    }

    public void setLimit(Phase phase) {
        assert phase != null && !phase.precedes(start) : start + " - " + phase;
        limit = phase;
    }

    public String toString() {
        return "[" + start + "->" + limit + "]";
    }

}

/** A class for types indexed by phases. */
public // !!! for java -Xfuture
class TypeIntervalList extends IntervalList {

    /** Previous interval */
    public final TypeIntervalList prev;
    /** Info valid during this interval */
    public final Type info;

    public TypeIntervalList(TypeIntervalList prev, Type info, Phase start) {
        super(prev, start);
        this.prev = prev;
        this.info = info;
        assert info != null;
    }

}
