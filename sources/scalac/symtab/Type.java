/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**
** $Id$
\*                                                                      */
//todo: implement rule single

package scalac.symtab;

import scalac.ApplicationError;
import scalac.util.*;
import scalac.Global;

public class Type implements Modifiers, Kinds, TypeTags {

    public static boolean debugSwitch = false;
    private static int indent = 0;

    //todo: convert C with {} to C.

    public case ErrorType;  // not used after analysis
    public case AnyType;    // not used after analysis
    public case NoType;

    public case ThisType(Symbol sym);

    public case TypeRef(Type pre, Symbol sym, Type[] args) {
	//assert sym != Global.instance.definitions.ALL_CLASS ||
	//    Global.instance.definitions.ALL_TYPE == null;
	assert pre.isLegalPrefix() || pre == ErrorType : pre + "#" + sym;
    }

    public case SingleType(Type pre, Symbol sym) {
	//	assert sym != Global.instance.definitions.SCALA ||
	//    Global.instance.definitions.SCALA_TYPE == null;
	assert this instanceof ExtSingleType;
    }

    public case CompoundType(Type[] parts, Scope members) {
	assert this instanceof ExtCompoundType;
    }
    public case MethodType(Symbol[] vparams, Type result);
    public case PolyType(Symbol[] tparams, Type result);
    public case OverloadedType(Symbol[] alts, Type[] alttypes);

    /** Hidden case to implement delayed evaluation of types.
     *  No need to pattern match on this type; it will never come up.
     */
    public case LazyType();

    /** Hidden case to implement local type inference.
     *  Later phases do not need to match on this type.
     */
    public case TypeVar(Type origin, Constraint constr);

    /** Hidden cases to implement type erasure.
     *  Earlier phases do not need to match on these types.
     */
    public case UnboxedType(int tag);
    public case UnboxedArrayType(Type elemtp);

    /** Force evaluation of a lazy type. No cycle
     *  check is needed; since this is done in Symbol.
     *  @see  Symbol.info().
     */
    public void complete(Symbol p) {}

// Creators ---------------------------------------------------------------------

    /** An owner-less ThisType
     */
    public static Type localThisType = ThisType(Symbol.NONE);

    /** An empty Type array */
    public static final Type[] EMPTY_ARRAY  = new Type[0];

    /** A non-existing Type array; used to express type erasure */
    public static final Type[] NO_ARRAY  = new Type[0];

    public static SingleType singleType(Type pre, Symbol sym) {
	if (pre.isStable() || pre == ErrorType) {
	    return new ExtSingleType(pre, sym);
	} else {
	    throw new Type.Error(
		"malformed type: " + pre + "#" + sym.nameString() + ".type");
	}
    }

    public static TypeRef appliedType(Type tycon, Type[] args) {
	switch (tycon) {
	case TypeRef(Type pre, Symbol sym, Type[] args1):
	    if (args == args1) return (TypeRef)tycon;
	    else return TypeRef(pre, sym, args);
	default:
	    throw new ApplicationError();
	}
    }

    public static CompoundType compoundType(Type[] parts, Scope members,
					    Symbol clazz) {
	ExtCompoundType res = new ExtCompoundType(parts, members);
	res.tsym = clazz;
	return res;
    }

    public static CompoundType compoundType(Type[] parts, Scope members) {
	ExtCompoundType res = new ExtCompoundType(parts, members);
	res.tsym = new ClassSymbol(
	    Position.NOPOS, Names.COMPOUND_NAME.toTypeName(), Symbol.NONE,
	    SYNTHETIC | ABSTRACTCLASS);
	res.tsym.setInfo(res);
	res.tsym.constructor().setInfo(
	    Type.MethodType(Symbol.EMPTY_ARRAY, Type.NoType));
	return res;
    }

    public static Type typeRef(Type pre, Symbol sym, Type[] args) {
	if (pre.isLegalPrefix() || pre == ErrorType)
	    return TypeRef(pre, sym, args);
	else if (sym.kind == ALIAS)
	    return pre.memberInfo(sym);
	else // todo: handle Java-style inner classes
	    throw new Type.Error(
		"malformed type: " + pre + "#" + sym.nameString());
    }

    static class ExtSingleType extends SingleType {
	Type tp = null;
	int definedId = -1;
	ExtSingleType(Type pre, Symbol sym) {
	    super(pre, sym);
	}
	private Type type() {
	    if (definedId != Global.instance.currentPhase.id) {
		definedId = Global.instance.currentPhase.id;
		tp = pre.memberType(sym).resultType();
	    }
	    return tp;
	}
	/** If this type is a thistype or singleton type, its underlying object type,
	 *  otherwise the type itself.
	 */
	public Type widen() {
	    return type().widen();
	}

	/** If this type is a singleton type whose type is another, the end of the chain,
	 *  otherwise the type itself.
	 */
	public Type aliasedType() {
	    Type tp = type();
	    if (tp.isStable()) return tp.aliasedType();
	    else return this;
	}
    }

    static class ExtCompoundType extends CompoundType {
	Symbol tsym;
	ExtCompoundType(Type[] parts, Scope members) {
	    super(parts, members);
	}
	public Symbol symbol() {
	    return tsym;
	}
	void validate() {//debug
	    for (Scope.Entry e = members.elems; e != Scope.Entry.NONE; e = e.next)
		assert e.sym.owner() == tsym;
	}
    }

    void validate() {//debug
    }

// Access methods ---------------------------------------------------------------

    /** If this is a thistype, named type, applied type, singleton type, or compound type,
     *  its symbol, otherwise Symbol.NONE.
     */
    public Symbol symbol() {
        switch (this) {
        case ErrorType:
	    return Symbol.ERROR;
	case ThisType(Symbol sym):
	    return sym;
        case TypeRef(_, Symbol sym, _):
	    return sym;
	case SingleType(_, Symbol sym):
	    return sym;
	case TypeVar(Type origin, _):
	    return origin.symbol();
	case CompoundType(_, _):
	    // overridden in ExtCompoundType
	    throw new ApplicationError();
        default:
	    return Symbol.NONE;
	}
    }

    public static Symbol[] symbol(Type[] tps) {
	Symbol[] syms = new Symbol[tps.length];
	for (int i = 0; i < syms.length; i++)
	    syms[i] = tps[i].symbol();
	return syms;
    }

    /** If this type is a thistype or singleton type, its underlying object type,
     *  otherwise the type itself.
     */
    public Type widen() {
	switch (this) {
	case ThisType(Symbol sym):
	    return sym.typeOfThis();
	case SingleType(Type pre, Symbol sym):
	    // overridden in ExtSingleType
	    throw new ApplicationError();
	case TypeVar(Type origin, Constraint constr):
	    if (constr.inst != NoType) return constr.inst.widen();
	    else return this;
	default:
	    return this;
	}
    }

    public static Type[] widen(Type[] tps) {
	if (tps.length == 0) return Type.EMPTY_ARRAY;
	Type[] tps1 = new Type[tps.length];
	for (int i = 0; i < tps1.length; i++) {
	    tps1[i] = tps[i].widen();
	    assert !(tps1[i] instanceof SingleType) : tps[i];//debug
	}
	return tps1;
    }

    /** If this type is a singleton type whose type is another, the end of the chain,
     *  otherwise the type itself.
     */
    public Type aliasedType() {
	return this;
    }

    /** The lower approximation of this type (which must be a typeref)
     */
    public Type loBound() {
	switch (unalias()) {
	case TypeRef(Type pre, Symbol sym, Type[] args):
	    Type lb = Global.instance.definitions.ANY_TYPE;
	    if (sym.kind == TYPE) {
		lb = sym.loBound().asSeenFrom(pre, sym.owner());
	    }
	    if (lb.isSameAs(Global.instance.definitions.ANY_TYPE) &&
		this.isSubType(Global.instance.definitions.ANYREF_TYPE)) {
		lb = Global.instance.definitions.ANYREF_TYPE;
	    }
	    return lb;
	default:
	    throw new ApplicationError();
	}
    }

    /** The thistype or singleton type corresponding to values of this type.
      */
    public Type narrow() {
	switch (this) {
	case TypeRef(Type pre, Symbol sym, Type[] args):
	    if (sym.kind == ALIAS) return pre.memberInfo(sym).narrow();
	    else if (sym.kind == CLASS) return sym.thisType();
	    else return ThisType(sym);
	case CompoundType(_, _):
	    return symbol().thisType();
	default:
	    return this;
	}
    }

    /** The upper bound of this type.
      */
    public Type bound() {
	switch (this) {
	case TypeRef(Type pre, Symbol sym, Type[] args):
	    if (sym.kind == ALIAS || sym.kind == TYPE)
                return pre.memberInfo(sym).bound();
	}
        return this;
    }

    /** The this is a this-type, named-type, applied type or single-type, its prefix,
     *  otherwise NoType.
     */
    public Type prefix() {
	switch (this) {
	case ThisType(Symbol sym): return sym.owner().thisType();
        case TypeRef(Type pre, _, _): return pre;
        case SingleType(Type pre, _): return pre;
	case TypeVar(Type origin, Constraint constr):
	    if (constr.inst != NoType) return constr.inst.prefix();
	    else return NoType;
	default: return NoType;
	}
    }

   /** Get all type arguments of this type.
    */
    public Type[] typeArgs() {
	switch (unalias()) {
	case TypeRef(_, _, Type[] args):
	    return args;
	default:
	    return Type.EMPTY_ARRAY;
	}
    }

    /** Get type of `this' symbol corresponding to this type, extend
     *  homomorphically to function types and method types.
     */
    public Type instanceType() {
	switch (unalias()) {
	case TypeRef(Type pre, Symbol sym, Type[] args):
	    Type tp1 = sym.typeOfThis();
	    if (tp1 != sym.type())
		return tp1.asSeenFrom(pre, sym.owner()).subst(sym.typeParams(), args);
	    break;
	case MethodType(Symbol[] params, Type restp):
	    Type restp1 = restp.instanceType();
	    if (restp1 != restp)
		return MethodType(params, restp1);
	    break;
	case PolyType(Symbol[] tparams, Type restp):
	    Type restp1 = restp.instanceType();
	    if (restp1 != restp)
		return PolyType(tparams, restp1);
	    break;
	}
	return this;
    }

    /** Remove all aliases
     */
    public Type unalias() {
	Type result = unalias(0);//debug
	//if (this != result) System.out.println(this + " ==> " + result);//DEBUG
	return result;
    }

    private Type unalias(int n) {
	if (n == 20) throw new Type.Error("recursive type alias: " + this);
	switch (this) {
        case TypeRef(Type pre, Symbol sym, Type[] args):
	    if (sym.kind == ALIAS) return pre.memberInfo(sym).unalias(n + 1);
	    break;
	case TypeVar(Type origin, Constraint constr):
	    if (constr.inst != NoType) return constr.inst.unalias(n + 1);
	    else return this;
	}
	return this;
    }

    /** The (prefix-adapted) parents of this type.
     */
    public Type[] parents() {
	switch (unalias()) {
	case ThisType(_):
	case SingleType(_, _):
	    return widen().parents();
	case TypeRef(Type pre, Symbol sym, Type[] args):
	    if (sym.kind == ALIAS)
		return unalias().parents();
	    else if (sym.kind == CLASS) {
		assert sym.typeParams().length == args.length : sym + " " + ArrayApply.toString(args);//debug
		return subst(asSeenFrom(sym.info().parents(), pre, sym.owner()),
			     sym.typeParams(), args);
	    } else
		return new Type[]{sym.info().asSeenFrom(pre, sym.owner())};
        case CompoundType(Type[] parts, _):
	    return parts;
        default:
            return Type.EMPTY_ARRAY;
        }
    }

    /** Get type parameters of polymorphic method
     *  or EMPTY_ARRAY if not applicable.
     */
    public Symbol[] typeParams() {
        switch (this) {
	case PolyType(Symbol[] tparams, _):
	    return tparams;
	case TypeRef(_, Symbol sym, _):
	    if (sym.kind == CLASS) return sym.typeParams();
	    else return sym.info().typeParams();
	}
	return Symbol.EMPTY_ARRAY;
    }

    /** Get value parameters of method or EMPTY_ARRAY if not
     * applicable.
     */
    public Symbol[] valueParams() {
        switch (this) {
	case PolyType(_, Type result):
	    return result.valueParams();
        case MethodType(Symbol[] vparams, _):
            return vparams;
	case TypeRef(_, Symbol sym, _):
	    if (sym.kind == CLASS) return sym.valueParams();
	    else return sym.info().valueParams();
        default:
            return Symbol.EMPTY_ARRAY;
	}
    }

    /** If this type is a (possibly polymorphic) method type, its result type
     *  after applying all method argument sections,
     *  otherwise the type itself.
     */
    public Type resultType() {
	switch (this) {
	case PolyType(_, Type tpe):
	    return tpe.resultType();
	case MethodType(_, Type tpe):
	    return tpe.resultType();
	default:
	    return this;
	}
    }

    /** The number of value parameter sections of this type.
     */
    public int paramSectionCount() {
	switch (this) {
	case PolyType(_, Type restpe):
	    return restpe.paramSectionCount();
	case MethodType(_, Type restpe):
	    return restpe.paramSectionCount() + 1;
	default: return 0;
	}
    }

    /** The first parameter section of this type.
     */
    public Symbol[] firstParams() {
	switch (this) {
	case PolyType(_, Type restpe):
	    return restpe.firstParams();
	case MethodType(Symbol[] params, _):
	    return params;
	default: return Symbol.EMPTY_ARRAY;
	}
    }

    /** If this type is overloaded, its alternative types,
     *  otherwise an array consisting of this type itself.
     */
    public Type[] alternatives() {
        switch (this) {
        case OverloadedType(_, Type[] alttypes):
	    return alttypes;
	default:
	    return new Type[]{this};
	}
    }

    /** If type is a this type of a module class, transform to singletype of
     *  module.
     */
    public Type expandModuleThis() {
	switch (this) {
	case ThisType(Symbol sym):
	    if ((sym.flags & MODUL) != 0 && sym.module() != Symbol.NONE) {
		return singleType(
		    sym.owner().thisType().expandModuleThis(), sym.module());
	    }
	}
	return this;
    }

// Tests --------------------------------------------------------------------

    /** Is this type a this type or singleton type?
     */
    public boolean isStable() {
	switch (unalias()) {
	case ThisType(_):
	case SingleType(_, _):
	    return true;
	default:
	    return false;
	}
    }

    /** Is this type a legal prefix?
     */
    public boolean isLegalPrefix() {
	switch (unalias()) {
	case ThisType(_):
	case SingleType(_, _):
	    return true;
	case TypeRef(_, Symbol sym, _):
	    return sym.kind == CLASS &&
		((sym.flags & JAVA) != 0 ||
		 (sym.flags & (TRAIT | ABSTRACTCLASS)) == 0);
	default:
	    return false;
	}
    }

    /** Is this type a reference to an object type?
     *  todo: replace by this.isSubType(global.definitions.ANY_TYPE)?
     */
    public boolean isObjectType() {
	switch (unalias()) {
	case ThisType(_):
	case SingleType(_, _):
	case CompoundType(_, _):
	    return true;
	case TypeRef(Type pre, Symbol sym, _):
	    return sym.kind != ALIAS || unalias().isObjectType();
	default:
	    return false;
	}
    }

    /** Is this type of the form scala.FunctionN[T_1, ..., T_n, +T] or
     *  scala.Object with scala.FunctionN[T_1, ..., T_n, +T]?
     */
    public boolean isFunctionType() {
	switch (this) {
	case TypeRef(Type pre, Symbol sym, Type[] args):
	    if (sym.fullName().startsWith(Names.scala_Function)) {
		return args.length > 0;
	    }
	    break;
	case CompoundType(Type[] parents, Scope members):
	    return members.elems == Scope.Entry.NONE &&
		parents.length == 2 &&
		parents[0].symbol().fullName() == Names.scala_Object &&
		parents[1].isFunctionType();
	}
	return false;
    }

// Members and Lookup -------------------------------------------------------

    /** Get the scope containing the local members of this type.
     *  Symbols in this scope are not prefix-adapted!
     */
    public Scope members() {
        switch (this) {
	case ErrorType:
	    return new Scope();
	case TypeRef(_, Symbol sym, _):
	    return sym.info().members();
	case SingleType(_, Symbol sym):
	    return widen().members();
	case CompoundType(Type[] basetypes, Scope members):
	    return members;
        default:
	    return Scope.EMPTY;
        }
    }

    /** Lookup symbol with given name among all local and inherited members
     *  of this type; return Symbol.NONE if not found.
     */
    public Symbol lookup(Name name) {
        switch (this) {
	case ErrorType:
	    return Symbol.ERROR;
	case ThisType(_):
	case SingleType(_, _):
	    return widen().lookup(name);
	case TypeRef(_, Symbol sym, _):
	    return sym.info().lookup(name);
	case CompoundType(Type[] parts, Scope members):
	    Symbol sym = members.lookup(name);
	    if (sym.kind != NONE) return sym;
	    else return lookupNonPrivate(name);
        default:
	    return Symbol.NONE;
	}
    }

    /** Lookup non-private symbol with given name among all local and
     *  inherited members of this type; return Symbol.NONE if not found.
     */
    public Symbol lookupNonPrivate(Name name) {
	return lookupNonPrivate(name, 0);
    }

    /** Same as before, but with additional parameter `start'.
     *  If start == 0, lookup in all basetypes of a compound type.
     *  If start == 1, lookup only in mixin classes.
     */
    private Symbol lookupNonPrivate(Name name, int start) {
        switch (this) {
	case ErrorType:
	    return Symbol.ERROR;
	case ThisType(_):
	case SingleType(_, _):
	    return widen().lookupNonPrivate(name);
	case TypeRef(_, Symbol sym, _):
	    return sym.info().lookupNonPrivate(name, start);
	case CompoundType(Type[] parts, Scope members):
	    Symbol sym = members.lookup(name);
	    if (sym.kind != NONE && (sym.flags & PRIVATE) == 0)
		return sym;

	    // search base types in reverse; non-abstract members
	    // take precedence over abstract ones.
	    int i = parts.length;
	    sym = Symbol.NONE;
	    while (i > start && (sym.kind == NONE || (sym.flags & DEFERRED) != 0)) {
		i--;
		Symbol sym1 = parts[i].lookupNonPrivate(name, i == 0 ? 0 : 1);
		if (sym1.kind != NONE &&
		    (sym1.flags & PRIVATE) == 0 &&
		    (sym.kind == NONE || (sym1.flags & DEFERRED) == 0))
		    sym = sym1;
	    }
	    return sym;
        default:
	    return Symbol.NONE;
	}
    }

// Maps --------------------------------------------------------------------------

    /** The type of type-to-type functions.
     */
    public abstract static class Map {

	public abstract Type apply(Type t);

	/** Apply map to all top-level components of this type.
	 */
	public Type map(Type tp) {
	    switch (tp) {
	    case ErrorType:
	    case AnyType:
	    case NoType:
	    case UnboxedType(_):
	    case TypeVar(_, _):
	    case ThisType(_):
		return tp;
	    case TypeRef(Type pre, Symbol sym, Type[] args):
		Type pre1 = apply(pre);
		Type[] args1 = map(args);
		if (pre1 == pre && args1 == args) return tp;
		else return typeRef(pre1, sym, args1);
	    case SingleType(Type pre, Symbol sym):
		Type pre1 = apply(pre);
		if (pre1 == pre) return tp;
		else return singleType(pre1, sym);
	    case CompoundType(Type[] parts, Scope members):
		Type[] parts1 = map(parts);
		Scope members1 = map(members);
		if (parts1 == parts && members1 == members) {
		    return tp;
		} else if (members1 == members && !tp.symbol().isCompoundSym()) {
		    return compoundType(parts1, members, tp.symbol());
		} else {
		    Scope members2 = new Scope();
		    //Type tp1 = compoundType(parts1, members2);
		    Type tp1 = (tp.symbol().isCompoundSym()) ? compoundType(parts1, members2)
		    	: compoundType(parts1, members2, tp.symbol());
		    Symbol[] syms1 = members1.elements();
		    Symbol[] syms2 = new Symbol[syms1.length];
		    for (int i = 0; i < syms2.length; i++) {
			syms2[i] = syms1[i].cloneSymbol().setOwner(tp1.symbol());
		    }
		    for (int i = 0; i < syms2.length; i++) {
			syms2[i].setInfo(syms1[i].info().subst(syms1, syms2));
			if (syms2[i].kind == TYPE)
			    syms2[i].setLoBound(syms1[i].loBound().subst(syms1, syms2));
		    }
		    for (int i = 0; i < syms2.length; i++) {
			members2.enter(syms2[i]);
		    }
		    return tp1;
		}

	    case MethodType(Symbol[] vparams, Type result):
		Symbol[] vparams1 = map(vparams);
		Type result1 = apply(result);
		if (vparams1 == vparams && result1 == result) return tp;
		else return MethodType(vparams1, result1);
	    case PolyType(Symbol[] tparams, Type result):
		Symbol[] tparams1 = map(tparams);
		Type result1 = apply(result);
		if (tparams1 != tparams) result1 = result1.subst(tparams, tparams1);
		if (tparams1 == tparams && result1 == result) return tp;
		else return PolyType(tparams1, result1);
	    case OverloadedType(Symbol[] alts, Type[] alttypes):
		Type[] alttypes1 = map(alttypes);
		if (alttypes1 == alttypes) return tp;
		else return OverloadedType(alts, alttypes1);
	    case UnboxedArrayType(Type elemtp):
		Type elemtp1 = apply(elemtp);
		if (elemtp1 == elemtp) return tp;
		else return UnboxedArrayType(elemtp1);
	    default:
		throw new ApplicationError(tp + " " + tp.symbol());
	    }
	}

	public Symbol map(Symbol sym) {
	    Type tp = sym.info();
	    Type tp1 = apply(tp);
	    Symbol sym1 = (tp == tp1) ? sym : sym.cloneSymbol().setInfo(tp1);
	    if (sym1.kind == TYPE) {
		Type lb = sym.loBound();
		Type lb1 = apply(lb);
		if (lb != lb1)
		    sym1 = sym1.cloneSymbol().setLoBound(lb1);
	    }
	    return sym1;
	}

	public Type[] map(Type[] tps) {
	    Type[] tps1 = tps;
	    for (int i = 0; i < tps.length; i++) {
		Type tp = tps[i];
		Type tp1 = apply(tp);
		if (tp1 != tp && tps1 == tps) {
		    tps1 = new Type[tps.length];
		    System.arraycopy(tps, 0, tps1, 0, i);
		}
		tps1[i] = tp1;
	    }
	    return tps1;
	}

	/** Apply map to all elements of this array of symbols,
	 *  preserving recursive references to symbols in the array.
	 */
	public Symbol[] map(Symbol[] syms) {
	    Symbol[] syms1 = syms;
	    for (int i = 0; i < syms.length; i++) {
		Symbol sym = syms[i];
		Symbol sym1 = map(sym);
		if (sym != sym1 && syms1 == syms) {
		    syms1 = new Symbol[syms.length];
		    System.arraycopy(syms, 0, syms1, 0, i);
		}
		syms1[i] = sym1;
	    }
	    if (syms1 != syms) {
		for (int i = 0; i < syms1.length; i++) {
		    if (syms1[i] == syms[i])
			syms1[i] = syms[i].cloneSymbol();
		}
		for (int i = 0; i < syms1.length; i++) {
		    syms1[i].setInfo(syms1[i].info().subst(syms, syms1));
		    if (syms1[i].kind == TYPE)
			syms1[i].setLoBound(syms1[i].loBound().subst(syms, syms1));
		}
	    }
	    return syms1;
	}

	/** Apply map to all elements of this array of this scope.
	 */
	public Scope map(Scope s) {
	    Symbol[] members = s.elements();
	    Symbol[] members1 = map(members);
	    if (members == members1) return s;
	    else return new Scope(members1);
	}
    }

    public abstract static class MapOnlyTypes extends Map {
        public Symbol map(Symbol sym) { return sym; }
        public Symbol[] map(Symbol[] syms) { return syms; }
        public Scope map(Scope s) { return s; }
    }

// baseType and asSeenFrom --------------------------------------------------------

    /** Return the base type of this type whose symbol is `clazz', or NoType, if
     *  such a type does not exist.
     */
    public Type baseType(Symbol clazz) {
	//System.out.println(this + ".baseType(" + clazz + ")");//DEBUG
	switch (this) {
	case ErrorType:
	    return ErrorType;

	case ThisType(_):
	case SingleType(_, _):
	    return widen().baseType(clazz);

	case TypeRef(Type pre, Symbol sym, Type[] args):
	    if (sym == clazz)
		return this;
	    else if (sym.kind == TYPE || sym.kind == ALIAS)
		return pre.memberInfo(sym).baseType(clazz);
	    else if (clazz.isCompoundSym())
		return NoType;
	    else {
		return sym.baseType(clazz)
		    .asSeenFrom(pre, clazz.owner())
		    .subst(sym.typeParams(), args);
	    }

	case CompoundType(Type[] parts, _):
	    for (int i = parts.length - 1; i >= 0; i--) {
		Type result = parts[i].baseType(clazz);
		if (result != NoType) return result;
	    }
	    break;

	case UnboxedArrayType(_):
	    if (clazz == Global.instance.definitions.ANY_CLASS)
		return clazz.type();
	}
	return NoType;
    }

    /** Return overriding instance of `sym' in this type,
     *  or `sym' itself if none exists.
     */
    public Symbol rebind(Symbol sym) {
	Symbol sym1 = lookupNonPrivate(sym.name);
	if (sym1.kind != NONE) {
	    //System.out.println("rebinding " + sym + " to " + sym1);//DEBUG
	    return sym1;
	}
	else return sym;
    }

    /** A map to implement `asSeenFrom'.
     */
    static class AsSeenFromMap extends Map {

	private Type pre;
	private Symbol clazz;

	AsSeenFromMap(Type pre, Symbol clazz) {
	    this.pre = pre; this.clazz = clazz;
	}

	public Type apply0(Type t) {
	    Type t1 = apply0(t);
	    System.out.println(t + " as seen from (" + pre + "," + clazz + ") = " + t1);//debug
	    return t1;
	}

	public Type apply(Type t) {
	    if (pre == NoType || clazz.kind != CLASS)
		return t;
	    switch (t) {
	    case ThisType(Symbol sym):
		return t.toPrefix(pre, clazz);

	    case TypeRef(Type prefix, Symbol sym, Type[] args):
		if (sym.kind == ALIAS) {
		    return apply(t.unalias());
		} else if (sym.owner().isPrimaryConstructor()) {
		    assert sym.kind == TYPE;
		    Type t1 = t.toInstance(pre, clazz);
		    //System.out.println(t + ".toInstance(" + pre + "," + clazz + ") = " + t1);//DEBUG
		    return t1;
		} else {
		    //Type prefix1 = prefix.toPrefix(pre, clazz);
		    Type prefix1 = apply(prefix);
		    Symbol sym1 = (prefix1 == prefix || (sym.flags & MODUL) != 0)
			? sym : prefix1.rebind(sym);
		    Type[] args1 = map(args);
		    if (prefix1 == prefix && args1 == args) return t;
		    else return typeRef(prefix1, sym1, args1);
		}

	    case SingleType(Type prefix, Symbol sym):
		try {
		    //Type prefix1 = prefix.toPrefix(pre, clazz);
		    Type prefix1 = apply(prefix);
		    if (prefix1 == prefix) return t;
		    else return singleType(prefix1, prefix1.rebind(sym));
		} catch (Type.Error ex) {}
                return apply(t.widen());

	    default:
		return map(t);
	    }
	}
    }
    //where
        Type toInstance(Type pre, Symbol clazz) {
	    if (pre == NoType || clazz.kind != CLASS)
		return this;
	    Symbol sym = symbol();
	    Symbol ownclass = sym.owner().primaryConstructorClass();
	    if (ownclass.isSubClass(clazz) &&
		pre.widen().symbol().isSubClass(ownclass)) {
		switch (pre.baseType(ownclass)) {
		case TypeRef(_, Symbol basesym, Type[] baseargs):
		    Symbol[] baseparams = basesym.typeParams();
		    for (int i = 0; i < baseparams.length; i++) {
			if (sym == baseparams[i]) return baseargs[i];
		    }
		    break;
		case ErrorType:
		    return ErrorType;
		}
		throw new ApplicationError(
		    this + " in " + ownclass + " cannot be instantiated from " + pre.widen());
	    } else {
		return toInstance(
		    pre.baseType(clazz).prefix(), clazz.owner());
	    }
	}

	Type toPrefix(Type pre, Symbol clazz) {
	    if (pre == NoType || clazz.kind != CLASS)
		return this;
	    else if (symbol().isSubClass(clazz) &&
		     pre.widen().symbol().isSubClass(symbol()))
		return pre;
	    else
		return toPrefix(pre.baseType(clazz).prefix(), clazz.owner());
	}

    /** This type Types as seen from prefix `pre' and class `clazz'. This means:
     *  Replace all this thistypes of `clazz' or one of its superclasses by `pre'.
     *  Proceed analogously for thistypes referring to outer classes.
     */
    public Type asSeenFrom(Type pre, Symbol clazz) {
	//System.out.println("computing asseenfrom of " + this + " with " + pre + "," + clazz);//DEBUG
	return new AsSeenFromMap(pre, clazz).apply(this);
    }

    /** Types `these' as seen from prefix `pre' and class `clazz'.
     */
    public static Type[] asSeenFrom(Type[] these, Type pre, Symbol clazz) {
	return new AsSeenFromMap(pre, clazz).map(these);
    }

    /** The info of `sym', seen as a member of this type.
     */
    public Type memberInfo(Symbol sym) {
	return memberTransform(sym, sym.info());
    }

    /** The type of `sym', seen as a member of this type.
     */
    public Type memberType(Symbol sym) {
	return memberTransform(sym, sym.type());
    }

    private Type memberTransform(Symbol sym, Type tp) {
	Type tp1 = tp.asSeenFrom(this, sym.owner());
	//if (Global.instance.debug) System.out.println(this + ".memberType(" + sym + ":" + tp + ") = " + tp1);//DEBUG
	return tp1;
    }

    public Type memberLoBound(Symbol sym) {
	return sym.loBound().asSeenFrom(this, sym.owner());
    }

// Substitutions ---------------------------------------------------------------

    /** A common map superclass for symbol/symbol and type/symbol substitutions.
     */
    public static abstract class SubstMap extends Map {
	private Symbol[] from;

	SubstMap(Symbol[] from) {
	    this.from = from;
	}

	public boolean matches(Symbol sym1, Symbol sym2) {
	    return sym1 == sym2;
	}

	/** Produce replacement type
	 *  @param i          The index in `from' of the symbol to be replaced.
	 *  @param fromtp     The type referring to this symbol.
	 */
	protected abstract Type replacement(int i, Type fromtp);

	/** Produce new substitution where some symbols are excluded.
	 *  @param newfrom    The new array of from symbols (without excluded syms)
	 *  @param excluded   The array of excluded sysmbols
	 */
	protected abstract SubstMap exclude(Symbol[] newfrom, Symbol[] excluded);

	public Type apply(Type t) {
	    switch (t) {
	    case TypeRef(ThisType(_), Symbol sym, Type[] args):
		for (int i = 0; i < from.length; i++) {
		    if (matches(sym, from[i])) return replacement(i, t);
		}
		break;
	    case SingleType(ThisType(_), Symbol sym):
		for (int i = 0; i < from.length; i++) {
		    if (matches(sym, from[i])) return replacement(i, t);
		}
		break;
	    case PolyType(Symbol[] tparams, Type result):
		Symbol[] from1 = excludeSyms(from, tparams, from);
		if (from1 != from) {
		    SubstMap f = exclude(from1, tparams);
		    Symbol[] tparams1 = f.map(tparams);
		    Type result1 = f.apply(result);
		    if (tparams1 != tparams)
			result1 = result1.subst(tparams, tparams1);
		    if (tparams1 == tparams && result1 == result) return t;
		    else return PolyType(tparams1, result1);
		}
	    }
	    return map(t);
	}
	//where
	private boolean contains1(Symbol[] syms, Symbol sym) {
	    int i = 0;
	    while (i < syms.length && syms[i] != sym) i++;
	    return i < syms.length;
	}

	private int nCommon(Symbol[] from, Symbol[] tparams) {
	    int cnt = 0;
	    for (int i = 0; i < from.length; i++) {
		if (contains1(tparams, from[i])) cnt++;
	    }
	    return cnt;
	}

	private Symbol[] excludeSyms(Symbol[] from, Symbol[] tparams, Symbol[] syms) {
	    int n = nCommon(from, tparams);
	    if (n == 0) {
		return syms;
	    } else {
		Symbol[] syms1 = new Symbol[syms.length - n];
		int j = 0;
		for (int i = 0; i < from.length; i++) {
		    if (!contains1(tparams, from[i])) syms1[j++] = syms[i];
		}
		return syms1;
	    }
	}

	private Type[] excludeTypes(Symbol[] from, Symbol[] tparams, Type[] types) {
	    int n = nCommon(from, tparams);
	    if (n == 0) {
		return types;
	    } else {
		Type[] types1 = new Type[types.length - n];
		int j = 0;
		for (int i = 0; i < from.length; i++) {
		    if (!contains1(tparams, from[i])) types1[j++] = types[i];
		}
		return types1;
	    }
	}
    }

    /** A map for symbol/symbol substitutions
     */
    public static class SubstSymMap extends SubstMap {
	Symbol[] to;
	protected SubstSymMap(Symbol[] from, Symbol[] to) {
	    super(from);
	    this.to = to;
	}
	protected Type replacement(int i, Type fromtp) {
	    switch (fromtp) {
	    case TypeRef(Type pre, Symbol sym, Type[] args):
		return typeRef(pre, to[i], args);
	    case SingleType(Type pre, Symbol sym):
		return singleType(pre, to[i]);
	    default:
		throw new ApplicationError();
	    }
	}
	protected SubstMap exclude(Symbol[] newfrom, Symbol[] excluded) {
	    return new SubstSymMap(newfrom, excludeSyms(from, excluded, to));
	}
    }

    /** A map for type/symbol substitutions
     */
    public static class SubstTypeMap extends SubstMap {
	Type[] to;
	protected SubstTypeMap(Symbol[] from, Type[] to) {
	    super(from);
	    this.to = to;
	}
	protected Type replacement(int i, Type fromtp) {
	    return to[i];
	}
	protected SubstMap exclude(Symbol[] newfrom, Symbol[] excluded) {
	    return new SubstTypeMap(newfrom, excludeTypes(from, excluded, to));
	}
    }

    /** Substitute symbols `to' for occurrences of symbols `from' in this type.
     */
    public Type subst(Symbol[] from, Symbol[] to) {
	assert from.length == to.length
	    : this + ": " + from.length + " != " + to.length;
	if (from.length != 0 && from != to)
	    return new SubstSymMap(from, to).apply(this);
	else return this;
    }

    /** Substitute symbols `to' for occurrences of symbols `from' in these types.
     */
    public static Type[] subst(Type[] these, Symbol[] from, Symbol[] to) {
	assert from.length == to.length;
	if (these.length != 0 && from.length != 0 && from != to)
	    return new SubstSymMap(from, to).map(these);
	else return these;
    }

    /** Substitute types `to' for occurrences of symbols `from' in this type.
     */
    public Type subst(Symbol[] from, Type[] to) {
	assert from.length == to.length
	    : this + ": " + from.length + " != " + to.length;
	if (from.length != 0)
	    return new SubstTypeMap(from, to).apply(this);
	else return this;
    }

    /** Substitute types `to' for occurrences of symbols `from' in these types.
     */
    public static Type[] subst(Type[] these, Symbol[] from, Type[] to) {
	assert from.length == to.length;
	if (these.length != 0 && from.length != 0)
	    return new SubstTypeMap(from, to).map(these);
	else return these;
    }

    /** A map for substitutions of thistypes.
     */
    public static class SubstThisMap extends Map {
	Symbol from;
	Type to;
	protected SubstThisMap(Symbol from, Type to) {
	    this.from = from;
	    this.to = to;
	}
	public Type apply(Type t) {
	    switch (t) {
	    case ThisType(Symbol sym):
		if (sym == from) return to;
		else return t;
	    default:
		return map(t);
	    }
	}
    }

    public Type substThis(Symbol from, Type to) {
	return new SubstThisMap(from, to).apply(this);
    }

    public static Type[] substThis(Type[] these, Symbol from, Type to) {
	return new SubstThisMap(from, to).map(these);
    }

    static class ContainsMap extends Map {
	boolean result = false;
	Symbol sym;
	ContainsMap(Symbol sym) {
	    this.sym = sym;
	}
	public Type apply(Type t) {
	    switch (t) {
	    case TypeRef(Type pre, Symbol sym1, Type[] args):
		if (sym == sym1) result = true;
		else { map(pre); map(args); }
		break;
	    case SingleType(Type pre, Symbol sym1):
		map(pre);
		if (sym == sym1) result = true;
		break;
	    default:
		map(t);
	    }
	    return t;
	}
    }

    /** Does this type contain symbol `sym'?
     */
    public boolean contains(Symbol sym) {
	ContainsMap f = new ContainsMap(sym);
	f.apply(this);
	return f.result;
    }

    /** Does this type contain any of the symbols `syms'?
     */
    public boolean containsSome(Symbol[] syms) {
	for (int i = 0; i < syms.length; i++)
	    if (contains(syms[i])) return true;
	return false;
    }

// Comparisons ------------------------------------------------------------------

    /** Is this type a subtype of that type?
     */
    public boolean isSubType(Type that) {
	if (debugSwitch) {
	    for (int i = 0; i < indent; i++) System.out.print("  ");
	    System.out.println(this + " < " + that + "?");
	    indent++;
	}
	boolean result = isSubType0(that);
	if (debugSwitch) {
	    indent--;
	    for (int i = 0; i < indent; i++) System.out.print("  ");
	    System.out.println(result);
	}
	return result;
    }

    public boolean isSubType0(Type that) {
	if (this == that) return true;

	switch (this) {
	case ErrorType:
	case AnyType:
	    return true;
	}

	switch (that) {
	case ErrorType:
	case AnyType:
	    return true;

	case NoType:
	    return false;

	case ThisType(_):
	case SingleType(_, _):
	    switch (this) {
	    case ThisType(_):
	    case SingleType(_, _):
		return this.isSameAs(that);
	    }
	    break;

       	case TypeRef(Type pre1, Symbol sym1, Type[] args1):
	    switch (this) {
	    case TypeRef(Type pre, Symbol sym, Type[] args):
		if (sym == sym1 && pre.isSameAs(pre1) &&
		    isSubArgs(args, args1, sym.typeParams())
		    ||
		    sym.kind == TYPE && pre.memberInfo(sym).isSubType(that))
		    return true;
		break;
	    }
	    if (sym1.kind == CLASS) {
		Type base = this.baseType(sym1);
		if (this != base && base.isSubType(that))
		    return true;
	    }
	    break;

	case CompoundType(Type[] parts1, Scope members1):
	    int i = 0;
	    while (i < parts1.length && isSubType(parts1[i])) i++;
	    if (i == parts1.length && specializes(members1))
		return true;
	    break;

	case MethodType(Symbol[] ps1, Type res1):
	    switch (this) {
	    case MethodType(Symbol[] ps, Type res):
		if (ps.length != ps1.length) return false;
		for (int i = 0; i < ps.length; i++) {
		    Symbol p1 = ps1[i];
		    Symbol p = ps[i];
		    if (!p1.type().isSubType(p.type()) ||
			(p1.flags & Modifiers.DEF) != (p.flags & Modifiers.DEF))
			return false;
		}
		return res.isSubType(res1);
	    }
	    break;

	case PolyType(Symbol[] ps1, Type res1):
	    switch (this) {
	    case PolyType(Symbol[] ps, Type res):
		if (ps.length != ps1.length) return false;
		for (int i = 0; i < ps.length; i++)
		    if (!ps1[i].info().subst(ps1, ps).isSubType(ps[i].info()))
			return false;
		return res.isSubType(res1.subst(ps1, ps));
	    }
	    break;

	case OverloadedType(Symbol[] alts1, Type[] alttypes1):
	    if (isSubSet(alttypes1, alternatives()))
		return true;
	    break;

	case UnboxedType(int tag1):
	    switch (this) {
	    case UnboxedType(int tag):
		return tag <= tag1 && tag1 <= DOUBLE && tag1 != CHAR;
	    }
	    break;

	case UnboxedArrayType(Type elemtp1):
	    switch (this) {
	    case UnboxedArrayType(Type elemtp):
		return !(elemtp1 instanceof UnboxedType) && elemtp.isSubType(elemtp1);
	    }
	    break;

	case TypeVar(Type origin, Constraint constr):
	    //todo: should we test for equality with origin?
	    if (constr.inst != NoType) {
		return this.isSubType(constr.inst);
	    } else {
		constr.lobounds = new List(this, constr.lobounds);
		return true;
	    }

	default:
	    throw new ApplicationError(this + " <: " + that);
	}

	switch (this) {
	case NoType:
	    return false;
	case ThisType(_):
	case SingleType(_, _):
	    if (this.widen().isSubType(that)) return true;
	    break;
	case TypeVar(Type origin, Constraint constr):
	    if (constr.inst != NoType) {
		return constr.inst.isSubType(that);
	    } else {
		constr.hibounds = new List(that, constr.hibounds);
		return true;
	    }

	case TypeRef(_, Symbol sym, _):
	    switch (that) {
	    case TypeRef(Type pre1, Symbol sym1, _):
		if (sym1.kind == TYPE && this.isSubType(that.loBound()))
		    return true;
	    }
	    if (sym.kind == ALIAS)
		return this.unalias().isSubType(that);
	    else if (sym == Global.instance.definitions.ALL_CLASS)
		return that.isSubType(Global.instance.definitions.ANY_TYPE);
	    else if (sym == Global.instance.definitions.ALLREF_CLASS)
		return
		    that.isSameAs(Global.instance.definitions.ANY_TYPE) ||
		    that.isSubType(Global.instance.definitions.ANYREF_TYPE);
	    break;

	case OverloadedType(Symbol[] alts, Type[] alttypes):
	    for (int i = 0; i < alttypes.length; i++) {
		if (alttypes[i].isSameAs0(that)) return true;
	    }
	    break;
	}

	switch (that) {
	case TypeRef(_, Symbol sym1, _):
	    if (sym1.kind == ALIAS) return this.isSubType(that.unalias());
	    break;
	}

	return false;
    }

    /** Are types `these' subtypes of corresponding types `those'?
     */
    public static boolean isSubType(Type[] these, Type[] those) {
	if (these.length != those.length) return false;
	for (int i = 0; i < these.length; i++) {
	    if (!these[i].isSubType(those[i])) return false;
	}
	return true;
    }

    /** Are types `these' arguments types conforming to corresponding types `those'?
     */
    static boolean isSubArgs(Type[] these, Type[] those, Symbol[] tparams) {
	if (these.length != those.length) return false;
	for (int i = 0; i < these.length; i++) {
	    if ((tparams[i].flags & COVARIANT) != 0) {
		if (!these[i].isSubType(those[i])) return false;
		//} else if ((tparams[i].flags & CONTRAVARIANT) != 0) {
		//if (!those[i].isSubType(these[i])) return false;
	    } else {
		if (!these[i].isSameAs(those[i])) return false;
	    }
	}
	return true;
    }

    public static boolean isSubSet(Type[] alts, Type[] alts1) {
	for (int i = 0; i < alts.length; i++) {
	    int j = 0;
	    while (j < alts1.length && !alts1[j].isSameAs(alts[i])) j++;
	    if (j == alts1.length) return false;
	}
	return true;
    }

    /** Does this type implement all symbols in scope `s' with same or stronger types?
     */
    public boolean specializes(Scope s) {
	for (Scope.Entry e = s.elems; e != Scope.Entry.NONE; e = e.next)
	    if (!specializes(e.sym)) return false;
	return true;
    }

    /** Does this type implement symbol `sym1' with same or stronger type?
     */
    public boolean specializes(Symbol sym1) {
	if (debugSwitch) {
	    for (int i = 0; i < indent; i++) System.out.print("  ");
	    System.out.println(this + " specializes " + sym1 + "?");
	    indent++;
	}
	boolean result = specializes0(sym1);
	if (debugSwitch) {
	    indent--;
	    for (int i = 0; i < indent; i++) System.out.print("  ");
	    System.out.println(result);
	}
	return result;
    }

    public boolean specializes0(Symbol sym1) {
	Symbol sym = lookup(sym1.name);
	Type self = narrow();
	return
	    sym == sym1 ||
	    ((sym.kind == sym1.kind || sym1.kind == TYPE) &&
	     self.memberInfo(sym).subst(symbol().typeParams(), typeArgs())
	         .isSubType(sym1.info().substThis(sym.owner(), self))) ||
	    (sym.kind == TYPE && sym1.kind == ALIAS &&
	     sym1.info().unalias().isSameAs(sym.type()));
    }

    /** Is this type the same as that type?
     */
    public boolean isSameAs(Type that) {
	if (debugSwitch) {
	    for (int i = 0; i < indent; i++) System.out.print("  ");
	    System.out.println(this + " = " + that + "?");
	    indent++;
	}
	boolean result = isSameAs0(that);
	if (debugSwitch) {
	    indent--;
	    for (int i = 0; i < indent; i++) System.out.print("  ");
	    System.out.println(result);
	}
	return result;
    }

    public boolean isSameAs0(Type that) {
	if (this == that) return true;

 	switch (this) {
	case ErrorType:
	case AnyType:
	    return true;

	case ThisType(Symbol sym):
	    switch (that) {
	    case ThisType(Symbol sym1):
		return sym == sym1;
	    case SingleType(Type pre1, Symbol sym1):
		return sym1.isModule()
		    && sym == sym1.moduleClass()
		    && sym.owner().thisType().isSameAs(pre1)
		    ||
		    that != that.aliasedType() &&
		    this.isSameAs(that.aliasedType());
	    }
	    break;

	case SingleType(Type pre, Symbol sym):
	    switch (that) {
	    case SingleType(Type pre1, Symbol sym1):
		return sym == sym1 && pre.isSameAs(pre1)
		    ||
		    (this != this.aliasedType() || that != that.aliasedType()) &&
		    this.aliasedType().isSameAs(that.aliasedType());
	    case ThisType(Symbol sym1):
		return sym.isModule()
		    && sym.moduleClass() == sym1
		    && pre.isSameAs(sym1.owner().thisType())
		    ||
		    this != this.aliasedType() &&
		    this.aliasedType().isSameAs(that);
	    }
	    break;

	case TypeRef(Type pre, Symbol sym, Type[] args):
	    switch (that) {
	    case TypeRef(Type pre1, Symbol sym1, Type[] args1):
		if (sym == sym1 && pre.isSameAs(pre1) && isSameAs(args, args1))
		    return true;
	    }
	    break;

	case CompoundType(Type[] parts, Scope members):
	    switch (that) {
	    case CompoundType(Type[] parts1, Scope members1):
		if (parts.length != parts1.length) return false;
		for (int i = 0; i < parts.length; i++)
		    if (!parts[i].isSameAs(parts1[i])) return false;
		return isSameAs(members, members1);
	    }
	    break;

	case MethodType(Symbol[] ps, Type res):
	    switch (that) {
	    case MethodType(Symbol[] ps1, Type res1):
		if (ps.length != ps1.length) return false;
		for (int i = 0; i < ps.length; i++) {
		    Symbol p1 = ps1[i];
		    Symbol p = ps[i];
		    if (!p1.type().isSameAs(p.type()) ||
			(p1.flags & Modifiers.DEF) != (p.flags & Modifiers.DEF))
			return false;
		}
		return res.isSameAs(res1);
	    }
	    break;

	case PolyType(Symbol[] ps, Type res):
	    switch (that) {
	    case PolyType(Symbol[] ps1, Type res1):
		if (ps.length != ps1.length) return false;
		for (int i = 0; i < ps.length; i++)
		    if (!ps1[i].info().subst(ps1, ps).isSameAs(ps[i].info()))
			return false;
		return res.isSameAs(res1.subst(ps1, ps));
	    }
	    break;

	case OverloadedType(Symbol[] alts, Type[] alttypes):
	    switch (that) {
	    case OverloadedType(Symbol[] alts1, Type[] alttypes1):
		return isSubSet(alttypes1, alttypes)
		    && isSubSet(alttypes, alttypes1);
	    }
	    break;

	case UnboxedType(int kind):
	    switch (that) {
	    case UnboxedType(int kind1):
		return kind == kind1;
	    }
	    break;

	case UnboxedArrayType(Type elemtp):
	    switch (that) {
	    case UnboxedArrayType(Type elemtp1):
		return elemtp.isSameAs(elemtp1);
	    }
	    break;
	}

	switch (that) {
	case ErrorType:
	case AnyType:
	    return true;
	case NoType:
	    return false;
	case TypeVar(Type origin, Constraint constr):
	    if (constr.inst != NoType) return constr.inst.isSameAs(this);
	    else return constr.instantiate(this.any2typevar());
	}

	switch (this) {
	case NoType:
	    return false;
	case TypeRef(_, Symbol sym, _):
	    if (sym.kind == ALIAS) return this.unalias().isSameAs(that);
	    break;
	case TypeVar(Type origin, Constraint constr):
	    if (constr.inst != NoType) return constr.inst.isSameAs(that);
	    else return constr.instantiate(that.any2typevar());
	}

	switch (that) {
	case TypeRef(_, Symbol sym, _):
	    if (sym.kind == ALIAS) return this.isSameAs(that.unalias());
	}

	return false;
    }

    /** Are types `these' the same as corresponding types `those'?
     */
    public static boolean isSameAs(Type[] these, Type[] those) {
	if (these.length != those.length) return false;
	for (int i = 0; i < these.length; i++) {
	    if (!these[i].isSameAs(those[i])) return false;
	}
	return true;
    }

    /** Do scopes `s1' and `s2' define he same symbols with the same kinds and infos?
     */
    public boolean isSameAs(Scope s1, Scope s2) {
	return isSubScope(s1, s2) && isSubScope(s2, s1);
    }

    /** Does scope `s1' define all symbols of scope `s2' with the same kinds and infos?
     */
    private boolean isSubScope(Scope s1, Scope s2) {
	for (Scope.Entry e = s2.elems; e != Scope.Entry.NONE; e = e.next) {
	    Symbol sym2 = e.sym;
	    Symbol sym1 = s1.lookup(sym2.name);
	    if (sym1.kind != sym2.kind ||
		!sym1.info().isSameAs(
		    sym2.info().substThis(sym2.owner(), sym1.owner().thisType())))
		return false;
	}
	return true;
    }

    boolean isSameAsAll(Type[] tps) {
	int i = 1;
	while (i < tps.length && isSameAs(tps[i])) i++;
	return i == tps.length;
    }

    /** Map every occurrence of AnyType to a fresh type variable.
     */
    public static Map any2typevarMap = new Map() {
	public Type apply(Type t) { return t.any2typevar(); }
    };

    public Type any2typevar() {
	switch (this) {
	case AnyType:
	    return TypeVar(this, new Constraint());
	default:
	    return any2typevarMap.map(this);
	}
    }

// Closures and Least Upper Bounds ---------------------------------------------------

    /** The closure of this type, i.e. the widened type itself followed by all
     *  its direct and indirect (pre-) base types, sorted by Symbol.isLess().
     *  Note that (pre-) base types do _not_ carry type parameters; these
     *  are added by baseType().
     */
    public Type[] closure() {
	switch (this.widen().unalias()) {
        case TypeRef(Type pre, Symbol sym, Type[] args):
	    return subst(
		asSeenFrom(sym.closure(), pre, sym.owner()),
		sym.typeParams(), args);

	case CompoundType(Type[] parts, Scope members):
	    Type[][] closures = new Type[parts.length][];
	    for (int i = 0; i < parts.length; i++)
		closures[i] = parts[i].closure();
	    return union(closures);

        default:
            return new Type[]{this};
        }
    }

    /** return union of array of closures
     */
    static private Type[] union(Type[][] closures) {
	if (closures.length == 1) return closures[0]; // fast special case
	int[] index = new int[closures.length];
	int totalsize = 0;
	for (int i = 0; i < index.length; i++) {
	    index[i] = 0;
	    totalsize = totalsize + closures[i].length;
	}
	Type[] res = new Type[totalsize];
	int j = 0;

	while (true) {
	    // find minimal element
	    Type min = null;
	    for (int i = 0; i < index.length; i++) {
		if (index[i] < closures[i].length) {
		    Type cltype = closures[i][index[i]];
		    if (min == null ||
			cltype.symbol().isLess(min.symbol()) ||
			cltype.symbol() == min.symbol() && cltype.isSubType(min)) {
			min = cltype;
		    }
		}
	    }
	    if (min == null) break;

	    res[j] = min;
	    j = j + 1;

	    // bump all indices that start with minimal element
	    for (int i = 0; i < index.length; i++) {
		if (index[i] < closures[i].length &&
		    closures[i][index[i]].symbol() == min.symbol())
		    index[i] = index[i] + 1;
	    }
	}
	Type[] result = new Type[j];
	System.arraycopy(res, 0, result, 0, j);
	return result;
    }

    /** return intersection of array of closures
     */
    static private Type[] intersection(Type[][] closures) {
	if (closures.length == 1) return closures[0]; // fast special case
	int[] index = new int[closures.length];
	Type[] mintypes = new Type[closures.length];
	int minsize = Integer.MAX_VALUE;
	for (int i = 0; i < index.length; i++) {
	    index[i] = 0;
	    if (closures[i].length < minsize) minsize = closures[i].length;
	}
	Type[] res = new Type[minsize];
	int j = 0;

	L:
	while (true) {
	    // find minimal element
	    Symbol min = null;
	    for (int i = 0; i < index.length; i++) {
		if (index[i] == closures[i].length) break L;
		Symbol clsym = closures[i][index[i]].symbol();
		if (min == null || clsym.isLess(min)) min = clsym;
	    }

	    boolean agree = true;
	    // bump all indices that start with minimal element
	    for (int i = 0; i < index.length; i++) {
		Type cltype = closures[i][index[i]];
		if (cltype.symbol() == min) {
		    mintypes[i] = cltype;
		    index[i] = index[i] + 1;
		} else {
		    agree = false;
		}
	    }
	    if (agree) {
		Type mintype;
	        mintype = commonType(mintypes);
		if (mintype == NoType)
		    mintype = arglub(mintypes);
		if (mintype.symbol().kind == CLASS) {
		    res[j] = mintype;
		    j = j + 1;
		}
	    }
	}
	Type[] result = new Type[j];
	System.arraycopy(res, 0, result, 0, j);
	return result;
    }

    /** same as lub, but all types are instances of the same class,
     *  possibly with different prefixes and arguments.
     */
    //todo: catch lubs not within bounds.
    static Type arglub(Type[] types) {
	Type pre = types[0].prefix();
	Symbol sym = types[0].symbol();
	Symbol[] tparams = sym.typeParams();
	Type[] args = new Type[tparams.length];
	Type[][] argss = new Type[args.length][types.length];
	for (int i = 0; i < types.length; i++) {
	    switch (types[i]) {
	    case TypeRef(Type pre1, Symbol sym1, Type[] args1):
		assert sym == sym1;
		assert args1.length == args.length;
		if (!pre.isSameAs(pre1)) return NoType;
		for (int j = 0; j < args1.length; j++)
		    argss[j][i] = args1[j];
		break;
	    case ErrorType:
		return ErrorType;
	    default:
		assert false : types[i];
	    }
	}
	for (int j = 0; j < args.length; j++) {
	    args[j] = commonType(argss[j]);
	    if (args[j] == NoType) {
		if ((tparams[j].flags & COVARIANT) != 0) args[j] = lub(argss[j]);
		else return NoType;
	    }
	}
	return typeRef(pre, sym, args);
    }

    /** The frontier of a closure C is the minimal set of types such that
     *  the union of the closures of these types equals C.
     */
    static private Type[] frontier(Type[] closure) {
	Type[] front = new Type[closure.length];
	int j = 0;
	for (int i = 0; i < closure.length; i++) {
	    int k = 0;
	    Type tp = closure[i];
	    while (k < j && !front[k].symbol().isSubClass(tp.symbol()))
		 k++;
	    if (k == j) {
		front[j] = tp;
		j++;
	    }
	}
	Type[] result = new Type[j];
	System.arraycopy(front, 0, result, 0, j);
	return result;
    }

    /** remove types that are subtypes of some other type.
     */
    static private Type[] elimRedundant(Type[] tps, boolean elimLower) {
	Type.List tl = Type.List.EMPTY;
	int nredundant = 0;
	boolean[] redundant = new boolean[tps.length];
	for (int i = 0; i < tps.length; i++) {
	    if (tps[i] == ErrorType) {
		return new Type[]{ErrorType};
	    } else {
		assert tps[i].isObjectType() : tps[i];
		for (int j = 0; j < i && !redundant[i]; j++) {
		    if (!redundant[j]) {
			if (tps[i].isSubType(tps[j])) {
			    redundant[elimLower ? i : j] = true;
			    nredundant++;
			} else if (tps[j].isSubType(tps[i])) {
			    redundant[elimLower ? j : i] = true;
			    nredundant++;
			}
		    }
		}
	    }
	}

	if (nredundant != 0) {
	    Type[] tps1 = new Type[tps.length - nredundant];
	    int n = 0;
	    for (int i = 0; i < tps.length; i++) {
		if (!redundant[i]) tps1[n++] = tps[i];
	    }
	    return tps1;
	} else {
	    return tps;
	}
    }

    /** Return the least upper bound of non-empty array of types `tps'.
     */
    public static Type lub(Type[] tps) {
	//System.out.println("lub" + ArrayApply.toString(tps));//DEBUG

	// remove types that are subtypes of some other type.

	tps = elimRedundant(tps, true);
	if (tps.length == 1) return tps[0];

	// intersect closures and build frontier.
	Type[][] closures = new Type[tps.length][];
	for (int i = 0; i < tps.length; i++) {
	    closures[i] = tps[i].closure();
	}
	Type[] allBaseTypes = intersection(closures);
	Type[] leastBaseTypes = frontier(allBaseTypes);
	if (leastBaseTypes.length == 0) {
	    //System.out.println("empty intersection");//DEBUG
	    return Type.NoType;
	}

	// add refinements where necessary
	Scope members = new Scope();
	Type lubType = compoundType(leastBaseTypes, members);
	Type lubThisType = lubType.narrow();
	//System.out.println("lubtype = " + lubType);//DEBUG

	Symbol[] rsyms = new Symbol[tps.length];
	Type[] rtps = new Type[tps.length];
	Type[] rlbs = new Type[tps.length];
	for (int i = 0; i < allBaseTypes.length; i++) {
	    for (Scope.Entry e = allBaseTypes[i].members().elems;
		 e != Scope.Entry.NONE;
		 e = e.next) {
		Name name = e.sym.name;
		if ((e.sym.flags & PRIVATE) == 0 && lubType.lookup(name) == e.sym) {
		    //todo: not memberType?
		    Type symType = lubThisType.memberInfo(e.sym);
		    Type symLoBound = lubThisType.memberLoBound(e.sym);
		    int j = 0;
		    while (j < tps.length) {
			rsyms[j] = tps[j].lookupNonPrivate(name);
			if (rsyms[j] == e.sym) break;
			rtps[j] = tps[j].memberType(rsyms[j])
			    .substThis(tps[j].symbol(), lubThisType);
			rlbs[j] = tps[j].memberLoBound(rsyms[j])
			    .substThis(tps[j].symbol(), lubThisType);
			if (rtps[j].isSameAs(symType) &&
			    rlbs[j].isSameAs(symLoBound)) break;
			j++;
		    }
		    if (j == tps.length) {
			Symbol lubSym = lub(rsyms, rtps, rlbs, lubType.symbol());
			if (lubSym.kind != NONE && !lubSym.info().isSameAs(symType))
			    members.enter(lubSym);
		    }
		}
	    }
	}
	//System.out.print("lub "); System.out.print(ArrayApply.toString(tps)); System.out.println(" = " + lubType);//DEBUG
	if (leastBaseTypes.length == 1 && members.elems == Scope.Entry.NONE)
	    return leastBaseTypes[0];
	else return lubType;
    }

    private static Type commonType(Type[] tps) {
	Type tp = tps[0];
	if (tp.isSameAsAll(tps)) return tp;
	else return NoType;
    }

    private static Symbol lub(Symbol[] syms, Type[] tps, Type[] lbs, Symbol owner) {
	//System.out.println("lub" + ArrayApply.toString(syms));//DEBUG
	int lubKind = syms[0].kind;
	for (int i = 1; i < syms.length; i++) {
	    Symbol sym = syms[i];
	    if (sym.kind == ERROR) return Symbol.NONE;
	    if (sym.isType() && sym.kind != lubKind) lubKind = TYPE;
	}
	if (lubKind == syms[0].kind && tps[0].isSameAsAll(tps)) {
	    return syms[0].cloneSymbol();
	}
	Type lubType = lub(tps);
	if (lubType == Type.NoType) return Symbol.NONE;
	Symbol lubSym;
	switch (lubKind) {
	case VAL:
	    lubSym = new TermSymbol(syms[0].pos, syms[0].name, owner, 0);
	    break;
	case TYPE: case ALIAS: case CLASS:
	    lubSym = new AbsTypeSymbol(syms[0].pos, syms[0].name, owner, 0);
	    lubSym.setLoBound(glb(lbs));
	    break;
	default:
	    throw new ApplicationError();
	}
	lubSym.setInfo(lubType);
	return lubSym;
    }

    private static Type glb(Type[] tps) {
	// step one: eliminate redunandant types; return if one one is left
	tps = elimRedundant(tps, false);
	if (tps.length == 1) return tps[0];

	// step two: build arrays of all typerefs and all refinements
	Type.List treftl = Type.List.EMPTY;
	Type.List comptl = Type.List.EMPTY;
	for (int i = 0; i < tps.length; i++) {
	    switch (tps[i]) {
	    case TypeRef(_, _, _):
		treftl = new Type.List(tps[i], treftl);
		break;
	    case CompoundType(Type[] parents, Scope members):
		if (members.elems != Scope.Entry.NONE)
		    comptl = new Type.List(tps[i], comptl);
		for (int j = 0; j < parents.length; j++)
		    treftl = new Type.List(parents[i], treftl);
		break;
	    case ThisType(_):
	    case SingleType(_, _):
		return Global.instance.definitions.ALL_TYPE;
	    }
	}

	CompoundType glbType = compoundType(Type.EMPTY_ARRAY, new Scope());
	Type glbThisType = glbType.narrow();

	// step 3: compute glb of all refinements.
	Scope members = Scope.EMPTY;
	if (comptl != List.EMPTY) {
	    Type[] comptypes = comptl.toArrayReverse();
	    Scope[] refinements = new Scope[comptypes.length];
	    for (int i = 0; i < comptypes.length; i++)
		refinements[i] = comptypes[i].members();
	    if (!setGlb(glbType.members, refinements, glbThisType)) {
		// refinements don't have lower bound, so approximate
		// by AllRef
		glbType.members = Scope.EMPTY;
		treftl = new Type.List(
		    Global.instance.definitions.ALLREF_TYPE, treftl);
	    }
	}

	// eliminate redudant typerefs
	Type[] treftypes = elimRedundant(treftl.toArrayReverse(), false);
	if (treftypes.length != 1 || glbType.members.elems != Scope.Entry.NONE) {
	    // step 4: replace all abstract types by their lower bounds.
	    boolean hasAbstract = false;
	    for (int i = 0; i < treftypes.length; i++) {
		if (treftypes[i].unalias().symbol().kind == TYPE)
		    hasAbstract = true;
	    }
	    if (hasAbstract) {
		treftl = Type.List.EMPTY;
		for (int i = 0; i < treftypes.length; i++) {
		    if (treftypes[i].unalias().symbol().kind == TYPE)
			treftl = new Type.List(treftypes[i].loBound(), treftl);
		    else
			treftl = new Type.List(treftypes[i], treftl);
		}
		treftypes = elimRedundant(treftl.toArrayReverse(), false);
	    }
	}

	if (treftypes.length != 1) {
	    // step 5: if there are conflicting instantiations of same
	    // class, replace them by lower bound.
	    Type lb = NoType;
	    for (int i = 0;
		 i < treftypes.length &&
		     lb.symbol() != Global.instance.definitions.ALL_CLASS;
		 i++) {
		for (int j = 0; j < i; j++) {
		    if (treftypes[j].symbol() == treftypes[i].symbol())
			lb = treftypes[i].loBound();
		}
	    }
	    if (lb != NoType) return lb;
	}

	if (treftypes.length == 1 && glbType.members.elems == Scope.Entry.NONE) {
	    return treftypes[0];
	} else {
	    glbType.parts = treftypes;
	    return glbType;
	}
    }

    private static boolean setGlb(Scope result, Scope[] ss, Type glbThisType) {
	for (int i = 0; i < ss.length; i++)
	    for (Scope.Entry e = ss[i].elems; e != Scope.Entry.NONE; e = e.next)
		if (!addMember(result, e.sym, glbThisType)) return false;
	return true;
    }

    private static boolean addMember(Scope s, Symbol sym, Type glbThisType) {
	Type syminfo = sym.info().substThis(sym.owner(), glbThisType);
	Type symlb = sym.loBound().substThis(sym.owner(), glbThisType);
	Scope.Entry e = s.lookupEntry(sym.name);
	if (e == Scope.Entry.NONE) {
	    Symbol sym1 = sym.cloneSymbol();
	    sym1.setOwner(glbThisType.symbol());
	    sym1.setInfo(syminfo);
	    if (sym1.kind == TYPE) sym1.setLoBound(symlb);
	    s.enter(sym1);
	} else {
	    Type einfo = e.sym.info();
	    if (einfo.isSameAs(syminfo)) {
	    } else if (einfo.isSubType(syminfo) && sym.kind != ALIAS) {
	    } else if (syminfo.isSubType(einfo) && e.sym.kind != ALIAS) {
		e.sym.setInfo(syminfo);
	    } else if (sym.kind == VAL && e.sym.kind == VAL ||
		       sym.kind == TYPE && e.sym.kind == TYPE) {
		e.sym.setInfo(glb(new Type[]{einfo, syminfo}));
	    } else {
		return false;
	    }
	    if (e.sym.kind == TYPE && sym.kind == TYPE) {
		Type elb = e.sym.loBound();
		if (elb.isSameAs(symlb)) {
		} else if (symlb.isSubType(elb)) {
		} else if (elb.isSubType(symlb)) {
		    e.sym.setLoBound(symlb);
		} else {
		    e.sym.setLoBound(lub(new Type[]{elb, symlb}));
		}
	    }
	}
	return true;
    }

// Erasure --------------------------------------------------------------------------

    public static Map erasureMap = new MapOnlyTypes() {
	public Type apply(Type t) { return t.erasure(); }
    };

    private static final Type[] unboxedType =
	new Type[LastUnboxedTag + 1 - FirstUnboxedTag];
    private static final Type[] unboxedArrayType =
	new Type[LastUnboxedTag + 1 - FirstUnboxedTag];
    private static final Name[] unboxedName =
	new Name[LastUnboxedTag + 1 - FirstUnboxedTag];
    private static final Name[] boxedName =
	new Name[LastUnboxedTag + 1 - FirstUnboxedTag];
    private static final Name[] boxedFullName =
	new Name[LastUnboxedTag + 1 - FirstUnboxedTag];

    private static void mkStdClassType(int kind, String unboxedstr, String boxedstr) {
	unboxedType[kind - FirstUnboxedTag] = UnboxedType(kind);
	unboxedArrayType[kind - FirstUnboxedTag] = UnboxedArrayType(unboxedType(kind));
	unboxedName[kind - FirstUnboxedTag] = Name.fromString(unboxedstr);
	boxedName[kind - FirstUnboxedTag] = Name.fromString(boxedstr);
	boxedFullName[kind - FirstUnboxedTag] = Name.fromString("scala." + boxedstr);
    }

    static {
	mkStdClassType(BYTE, "byte", "Byte");
	mkStdClassType(SHORT, "short", "Short");
	mkStdClassType(CHAR, "char", "Char");
	mkStdClassType(INT, "int", "Int");
	mkStdClassType(LONG, "long", "Long");
	mkStdClassType(FLOAT, "float", "Float");
	mkStdClassType(DOUBLE, "double", "Double");
	mkStdClassType(BOOLEAN, "boolean", "Boolean");
	mkStdClassType(UNIT, "void", "Unit");
    }

    /** Return unboxed type of given kind.
     */
    public static Type unboxedType(int kind) {
	return unboxedType[kind - FirstUnboxedTag];
    }

    /** Return unboxed array type of given element kind.
     */
    public static Type unboxedArrayType(int kind) {
	return unboxedArrayType[kind - FirstUnboxedTag];
    }

    /** Return the name of unboxed type of given kind.
    */
    public static Name unboxedName(int kind) {
	return unboxedName[kind - FirstUnboxedTag];
    }

    /** Return the name of boxed type of given kind.
    */
    public static Name boxedName(int kind) {
	return boxedName[kind - FirstUnboxedTag];
    }

    /** Return the full name of boxed type of given kind.
    */
    public static Name boxedFullName(int kind) {
	return boxedFullName[kind - FirstUnboxedTag];
    }

    /** If type is boxed, return its unboxed equivalent; otherwise return the type
     *  itself.
     */
    public Type unbox() {
	switch (this) {
	case TypeRef(Type pre, Symbol sym, Type[] args):
	    if ((sym.flags & MODUL) == 0) {
		Name fullname = sym.fullName();
		if (fullname == Names.scala_Array && args.length == 1
		    /*&& args[0].unalias().symbol().kind != TYPE Q: why needed?*/) {
                    Type bound = args[0].bound();
                    if (bound.symbol() != Global.instance.definitions.ANY_CLASS &&
                        bound.symbol() != Global.instance.definitions.ANYVAL_CLASS)
                    {
                        return UnboxedArrayType(args[0].erasure());
                    }
                }
                for (int i = 0; i < boxedFullName.length; i++) {
                    if (boxedFullName[i] == fullname) return unboxedType[i];
                }
	    }
	}
	return this;
    }

    /** Return the erasure of this type.
     */
    public Type erasure() {
	switch (this) {
	case ThisType(_):
	case SingleType(_, _):
	    return widen().erasure();
	case TypeRef(Type pre, Symbol sym, Type[] args):
	    switch (sym.kind) {
	    case ALIAS: case TYPE:
		return pre.memberInfo(sym).erasure();

	    case CLASS:
                if (sym == Global.instance.definitions.UNIT_CLASS) return this;
		Name fullname = sym.fullName();
		if (fullname == Names.java_lang_Object ||
		    fullname == Names.scala_AnyRef ||
		    fullname == Names.scala_AnyVal ||
		    fullname == Names.scala_All ||
		    fullname == Names.scala_AllRef)
		    return Global.instance.definitions.ANY_TYPE;
		else {
		    Type this1 = unbox();
		    if (this1 != this) return this1;
		    else if (args.length == 0) return this;
		    else return typeRef(pre, sym, Type.EMPTY_ARRAY);
		}

	    default: throw new ApplicationError();
	    }
	case CompoundType(Type[] parents, _):
	    if (parents.length > 0) return parents[0].erasure();
	    else return this;
	case MethodType(Symbol[] params, Type tp):
	    Symbol[] params1 = erasureMap.map(params);
	    Type tp1 = tp.fullErasure();
	    switch (tp1) {
	    case MethodType(Symbol[] params2, Type tp2):
		Symbol[] newparams = new Symbol[params1.length + params2.length];
		System.arraycopy(params1, 0, newparams, 0, params1.length);
		System.arraycopy(params2, 0, newparams, params1.length, params2.length);
		return MethodType(newparams, tp2);
	    default:
		if (params1 == params && tp1 == tp) return this;
		else return MethodType(params1, tp1);
	    }
	case PolyType(_, Type result):
	    return result.erasure();
	default:
	    return erasureMap.map(this);
	}
    }

    /** Return the full erasure of the type. Full erasure is the same
     * as "normal" erasure, except that the "Unit" type is erased to
     * the "void" type.
     */
    public Type fullErasure() {
        if (Global.instance.definitions.UNIT_CLASS == symbol())
            return unbox();
        else
            return erasure();
    }

// Object Interface -----------------------------------------------------------------

    public String toString() {
        return new SymbolTablePrinter().printType(this).toString();
    }

    public String toLongString() {
	String str = toString();
	if (str.endsWith(".type")) return str + " (with underlying type " + widen() + ")";
	else return str;
    }

    public int hashCode() {
	switch (this) {
	case ErrorType:
	    return ERROR;
	case NoType:
	    return NOtpe;
	case ThisType(Symbol sym):
	    return THIStpe
		^ (sym.hashCode() * 41);
	case TypeRef(Type pre, Symbol sym, Type[] args):
	    return NAMEDtpe
		^ (pre.hashCode() * 41)
		^ (sym.hashCode() * (41*41))
                ^ (hashCode(args) * (41*41*41));
	case SingleType(Type pre, Symbol sym):
	    return SINGLEtpe
		^ (pre.hashCode() * 41)
		^ (sym.hashCode() * (41*41));
	case CompoundType(Type[] parts, Scope members):
	    return COMPOUNDtpe
		^ (hashCode(parts) * 41)
		^ (members.hashCode() * (41 * 41));
	case MethodType(Symbol[] vparams, Type result):
	    return METHODtpe
		^ (hashCode(Symbol.type(vparams)) * 41)
		^ (result.hashCode() * (41 * 41));
	case PolyType(Symbol[] tparams, Type result):
	    return POLYtpe
		^ (hashCode(tparams) * 41)
		^ (result.hashCode() * (41 * 41));
	case OverloadedType(Symbol[] alts, Type[] alttypes):
	    return OVERLOADEDtpe
		^ (hashCode(alts) * 41)
		^ (hashCode(alttypes) * (41 * 41));
	case UnboxedType(int kind):
	    return UNBOXEDtpe
		^ (kind * 41);
	case UnboxedArrayType(Type elemtp):
	    return UNBOXEDARRAYtpe
		^ (elemtp.hashCode() * 41);
	default:
	    throw new ApplicationError();
	}
    }

    public static int hashCode(Object[] elems) {
	int h = 0;
	for (int i = 0; i < elems.length; i++)
	    h = h * 41 + elems[i].hashCode();
	return h;
    }

    public static int hashCode(Scope.Entry elems) {
	int h = 0;
	for (Scope.Entry e = elems; e != Scope.Entry.NONE; e = e.next)
	    h = h * 41
		+ e.sym.kind
		+ e.sym.name.hashCode()
		+ e.sym.info().hashCode();
	return h;
    }

    // todo: change in relation to needs.

    public boolean equals(Object other) {
	if (this == other) {
	    return true;
	} else if (other instanceof Type) {
	    Type that = (Type) other;
	    switch (this) {
	    case ErrorType:
		return that == ErrorType;
	    case NoType:
		return that == NoType;
	    case ThisType(Symbol sym):
		switch (that) {
		case ThisType(Symbol sym1):
		    return sym == sym1;
		default: return false;
		}
	    case TypeRef(Type pre, Symbol sym, Type[] args):
		switch (that) {
		case TypeRef(Type pre1, Symbol sym1, Type[] args1):
		    return pre.equals(pre1) && sym == sym1 && equals(args, args1);
		default: return false;
		}
	    case SingleType(Type pre, Symbol sym):
		switch (that) {
		case SingleType(Type pre1, Symbol sym1):
		    return pre.equals(pre1) && sym == sym1;
		default: return false;
		}
	    case CompoundType(Type[] parts, Scope members):
		switch (that) {
		case CompoundType(Type[] parts1, Scope members1):
		    return parts.equals(parts1) && members.equals(members1);
		default: return false;
		}
	    case MethodType(Symbol[] vparams, Type result):
		switch (that) {
		case MethodType(Symbol[] vparams1, Type result1):
		    return equals(Symbol.type(vparams), Symbol.type(vparams1)) &&
			result.equals(result1);
		default: return false;
		}
	    case PolyType(Symbol[] tparams, Type result):
		switch (that) {
		case PolyType(Symbol[] tparams1, Type result1):
		    return equals(tparams, tparams1) && result.equals(result1);
		default: return false;
		}
	    case OverloadedType(Symbol[] alts, Type[] alttypes):
		switch (that) {
		case OverloadedType(Symbol[] alts1, Type[] alttypes1):
		    return equals(alts, alts1) && equals(alttypes, alttypes1);
		default: return false;
		}
	    case UnboxedType(int kind):
		switch (that) {
		case UnboxedType(int kind1):
		    return kind == kind1;
		default: return false;
		}
	    case UnboxedArrayType(Type elemtp):
		switch (that) {
		case UnboxedArrayType(Type elemtp1):
		    return elemtp.equals(elemtp1);
		default: return false;
		}
	    default:
	    }
	}
	return false;
    }

    public static boolean equals(Object[] elems1, Object[] elems2) {
	if (elems1.length != elems2.length) return false;
	for (int i = 0; i < elems1.length; i++) {
	    if (!elems1[i].equals(elems2[i])) return false;
	}
	return true;
    }

// Type.List class -----------------------------------------------------------------

    /** A class for lists of types.
     */
    public static class List {
	public Type head;
	public List tail;
	public List(Type head, List tail) {
	    this.head = head; this.tail = tail;
	}
	public int length() {
	    return (this == EMPTY) ? 0 : 1 + tail.length();
	}
	public Type[] toArray() {
	    Type[] ts = new Type[length()];
	    copyToArray(ts, 0, 1);
	    return ts;
	}
	public void copyToArray(Type[] ts, int start, int delta) {
	    if (this != EMPTY) {
		ts[start] = head;
		tail.copyToArray(ts, start+delta, delta);
	    }
	}
	public Type[] toArrayReverse() {
	    Type[] ts = new Type[length()];
	    copyToArray(ts, ts.length - 1, -1);
	    return ts;
	}

	public String toString() {
	    if (this == EMPTY) return "List()";
	    else return head + "::" + tail;
	}

	public static List EMPTY = new List(null, null);

	public static List append(List l, Type tp) {
	    return (l == EMPTY) ? new List(tp, EMPTY)
		: new List(l.head, append(l.tail, tp));
	}
    }

// Type.Constraint class -------------------------------------------------------

    /** A class for keeping sub/supertype constraints and instantiations
     *  of type variables.
     */
    public static class Constraint {
	public List lobounds = List.EMPTY;
	public List hibounds = List.EMPTY;
	public Type inst = NoType;

	public boolean instantiate(Type tp) {
	    for (List l = lobounds; l != List.EMPTY; l = l.tail) {
		if (!l.head.isSubType(tp)) return false;
	    }
	    for (List l = hibounds; l != List.EMPTY; l = l.tail) {
		if (!tp.isSubType(l.head)) return false;
	    }
	    inst = tp;
	    return true;
	}
    }

// Type.Error class --------------------------------------------------------------

    /** A class for throwing type errors
     */
    public static class Error extends java.lang.Error {
	public String msg;
	public Error(String msg) {
	    super(msg);
	    this.msg = msg;
	}
    }

    /** A class for throwing type errors
     */
    public static class VarianceError extends Error {
	public VarianceError(String msg) {
	    super(msg);
	}
    }
}

/* A standard pattern match:

    case ErrorType:
    case AnyType:
    case NoType:
    case ThisType(Symbol sym):
    case TypeRef(Type pre, Symbol sym, Type[] args):
    case SingleType(Type pre, Symbol sym):
    case CompoundType(Type[] parts, Scope members):
    case MethodType(Symbol[] vparams, Type result):
    case PolyType(Symbol[] tparams, Type result):
    case OverloadedType(Symbol[] alts, Type[] alttypes):
*/

