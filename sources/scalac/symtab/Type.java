/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**
** $Id$
\*                                                                      */
//todo: T {} == T

package scalac.symtab;

import java.util.HashMap;

import scala.tools.util.Position;
import scalac.ApplicationError;
import scalac.atree.AConstant;
import scalac.util.*;
import scalac.Global;

public class Type implements Modifiers, Kinds, TypeTags, EntryTags {

    public static boolean explainSwitch = false;
    private static int indent = 0;

    public case ErrorType;  // not used after analysis
    public case AnyType;    // not used after analysis
    public case NoType;
    public case NoPrefix;

    /** C.this.type
     */
    public case ThisType(Symbol sym) {
        assert sym.isClassType(): Debug.show(sym);
    }

    /** pre.sym.type
     *  sym represents a valueS
     */
    public case SingleType(Type pre, Symbol sym) {
        assert this instanceof ExtSingleType;
    }

    /** Type for a numeric or string constant.
     */
    public case ConstantType(Type base, AConstant value);

    /** pre.sym[args]
     *  sym represents a type
     *  for example: scala.List[java.lang.String] is coded as
     *
     *  TypeRef(
     *      SingleType(ThisType(definitions.ROOT_CLASS), definitions.SCALA),
     *      <List>,
     *      new Type[]{
     *          TypeRef(
     *              SingleType(
     *                  SingleType(ThisType(definitions.ROOT_CLASS), definitions.JAVA),
     *                  definitions.LANG),
     *              definitions.STRING,
     *              new Type[]{})}).
     *
     */
    public case TypeRef(Type pre, Symbol sym, Type[] args) {
        assert this instanceof ExtTypeRef: this;
    }

    /** parts_1 with ... with parts_n { members }
     */
    public case CompoundType(Type[] parts, Scope members) {
        assert this instanceof ExtCompoundType;
    }

    /** synthetic type of a method  def ...(vparams): result = ...
     */
    public case MethodType(Symbol[] vparams, Type result) {
        for (int i = 0; i < vparams.length; i++)
            assert vparams[i].isParameter() && vparams[i].isTerm(): this;
    }

    /** synthetic type of a method  def ...[tparams]result
     *  For instance, given  def f[a](x: a): a
     *  f has type   PolyType(new Symbol[]{<a>},
     *                 MethodType(new Symbol[]{<x>}, <a>.type()))
     *
     *  if tparams is empty, this is the type of a parameterless method
     *  def ... =
     *  For instance, given    def f = 1
     *  f has type   PolyType(new Symbol[]{}, <scala.Int>.type())
     */
    public case PolyType(Symbol[] tparams, Type result) {
        for (int i = 0; i < tparams.length; i++)
            assert tparams[i].isParameter()&&tparams[i].isAbstractType(): this;
    }

    /** synthetic type of an overloaded value whose alternatives are
     *  alts_1, ..., alts_n, with respective types alttypes_1, ..., alttypes_n
     *
     *  For instance, if there are two definitions of `f'
     *    def f: int
     *    def f: String
     *  then there are three symbols:
     *    ``f1'' corresponding to def f: int
     *    ``f2'' corresponding to def f: String
     *    ``f3'' corresponding to both
     *  f3 has type
     *    OverloadedType(
     *      new Symbol[]{<f1>, <f2>},
     *      new Type[]{PolyType(new Symbol[]{}, <int>),
     *                 PolyType(new Symbol[]{}, <String>),
     *
     */
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

    /** An empty Type array */
    public static final Type[] EMPTY_ARRAY  = new Type[0];

    public static SingleType singleType(Type pre, Symbol sym) {
        assert sym.isTerm() && !sym.isNone(): pre + " -- " + Debug.show(sym);
        rebind:
        {
            Symbol owner = sym.owner();
            if (!owner.isClass()) break rebind;
            if (owner == pre.symbol()) break rebind;
            // !!! add if (owner is sealed/final) break rebind ?
            // !!! add if (owner is module class) break rebind ?
            if (sym.isFinal() || sym.isPrivate()) break rebind;
            Symbol rebind = pre.lookupNonPrivate(sym.name);
            if (rebind.isNone()) break rebind;
            if (rebind.isLocked()) throw new Type.Error(
                "illegal cyclic reference involving " + rebind);
            sym = rebind.rebindSym();
        }
        if (pre.isStable() || pre.isError()) {
            return new ExtSingleType(pre, sym);
        } else {
            throw new Type.Malformed(pre, sym.nameString() + ".type");
        }
    }

    public static Type constantType(AConstant value) {
        return Global.instance.definitions.atyper.type(value);
    }

    public static Type singleTypeMethod(Type pre, Symbol sym) {
        Global global = Global.instance;
        if (global.currentPhase.id <= global.PHASE.UNCURRY.id())
            return singleType(pre, sym);
        else if (global.currentPhase.id <= global.PHASE.ERASURE.id())
            return sym.type().singleTypeMethod0(pre, sym);
        else
            return pre.memberType(sym);
    }

    private Type singleTypeMethod0(Type pre, Symbol sym) {
        switch (this) {
        case PolyType(Symbol[] args, Type result):
            return PolyType(args, result.singleTypeMethod0(pre, sym));
        case MethodType(Symbol[] args, Type result):
            return MethodType(args, result.singleTypeMethod0(pre, sym));
        default:
            return singleType(pre, sym);
        }
    }

    public static Type appliedType(Type tycon, Type[] args) {
        switch (tycon) {
        case TypeRef(Type pre, Symbol sym, Type[] args1):
            if (args == args1) return tycon;
            else return Type.typeRef(pre, sym, args);
        default:
            throw Debug.abort("illegal case", tycon);
        }
    }

    public static Type typeRef(Type pre, Symbol sym, Type[] args) {
        if (sym.kind == TYPE && !pre.isLegalPrefix() && !pre.isError())
            throw new Type.Malformed(pre, sym.nameString());
        rebind:
        if (sym.isAbstractType()) {
            Symbol owner = sym.owner();
            if (!owner.isClass()) break rebind;
            if (owner == pre.symbol()) break rebind;
            // !!! add if (owner is sealed/final) break rebind ?
            // !!! add if (owner is module class) break rebind ?
            if (sym.isFinal() || sym.isPrivate()) break rebind;
            Symbol rebind = pre.lookupNonPrivate(sym.name);
            if (rebind.isNone()) break rebind;
            if (rebind.isLocked()) throw new Type.Error(
                "illegal cyclic reference involving " + rebind);
            sym = rebind.rebindSym();
        }
        if (sym.isTypeAlias()) {
            Symbol[] params = sym.typeParams();
            if (args.length == params.length)
                return pre.memberInfo(sym).subst(params, args);
            assert args.length == 0 || args.length == params.length:
                Debug.show(pre, sym, args, params);
        }
        assert isLegalTypeRef(pre, sym, args):
            Debug.show(pre, sym, args, sym.typeParams());
        return new ExtTypeRef(pre, sym, args);
    }
    private static boolean isLegalTypeRef(Type pre, Symbol sym, Type[] args) {
        if (sym.kind == TYPE && !pre.isLegalPrefix() && !pre.isError()) return false;
        if (!sym.isType() && !sym.isError()) return false;
        // !!! return args.length == 0 || args.length == sym.typeParams().length;
        return true;
    }

    public static Type newTypeRefUnsafe(Type pre, Symbol sym, Type[] args) {
        return new ExtTypeRef(pre, sym, args);
    }

    public static CompoundType compoundType(Type[] parts, Scope members,
                                            Symbol clazz) {
        return new ExtCompoundType(parts, members, clazz);
    }

    public static CompoundType compoundTypeWithOwner(Symbol owner, Type[] parts, Scope members) {
        return new ExtCompoundType(owner, parts, members);
    }

    static class ExtSingleType extends SingleType {
        Type tp = null;
        int definedId = -1;
        ExtSingleType(Type pre, Symbol sym) {
            super(pre, sym);
        }
        public Type singleDeref() {
            if (definedId != Global.instance.currentPhase.id) {
                definedId = Global.instance.currentPhase.id;
                tp = pre.memberType(sym).resultType();
            }
            return tp;
        }
    }

    static class ExtTypeRef extends TypeRef {
        ExtTypeRef(Type pre, Symbol sym, Type[] args) {
            super(pre, sym, args);
        }
    }

    private static final class ExtCompoundType extends CompoundType {
        private final Symbol clasz;
        public ExtCompoundType(Symbol owner, Type[] parts, Scope members) {
            super(parts, members);
            this.clasz = owner.newCompoundClass(this);
	    assert !owner.isPackageClass() : ArrayApply.toString(parts);
        }
        public ExtCompoundType(Type[] parts, Scope members, Symbol clasz) {
            super(parts, members);
            this.clasz = clasz;
        }
        public Symbol symbol() {
            return clasz;
        }
    }

// Access methods ---------------------------------------------------------------

    /** If this is a thistype, named type, applied type, singleton type, or compound type,
     *  its symbol, otherwise Symbol.NONE.
     */
    public Symbol symbol() {
        switch (this) {
        case ThisType(Symbol sym):
            return sym;
        case TypeRef(_, Symbol sym, _):
            return sym;
        case SingleType(_, Symbol sym):
            return sym;
        case ConstantType(Type base, _):
            return base.symbol();
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

    /** If this is a reference to a type constructor, add its
     *  type parameters as arguments
     */
    public Type withDefaultArgs() {
        switch (this) {
        case TypeRef(Type pre, Symbol sym, Type[] args):
            if (args.length == 0 && sym.typeParams().length != 0)
                return Type.typeRef(pre, sym, Symbol.type(sym.typeParams()));
        }
        return this;
    }

    /** The upper bound of this type. Returns always a TypeRef whose
     * symbol is a class.
     */
    public Type bound() {
        switch (unalias()) {
        case TypeRef(Type pre, Symbol sym, _):
            if (sym.kind == TYPE) return pre.memberInfo(sym).bound();
            assert sym.isClass() : Debug.show(sym) + " -- " + this;
            return this;
        case ThisType(_):
        case SingleType(_, _):
        case ConstantType(_, _):
            return singleDeref().bound();
        case TypeVar(Type origin, Constraint constr):
            if (constr.inst != NoType) return constr.inst.bound();
            else return this;
        default:
            throw Debug.abort("illegal case", this);
        }
    }

    /** If this type is a thistype or singleton type, its type,
     *  otherwise the type itself.
     */
    public Type singleDeref() {
        switch (this) {
        case ThisType(Symbol sym):
            return sym.typeOfThis();
        case SingleType(Type pre, Symbol sym):
            // overridden in ExtSingleType
            throw new ApplicationError();
        case ConstantType(Type base, _):
            return base;
        case TypeVar(Type origin, Constraint constr):
            if (constr.inst != NoType) return constr.inst.singleDeref();
            else return this;
        default:
            return this;
        }
    }

    /** If this type is a thistype or singleton type, its underlying object type,
     *  otherwise the type itself.
     */
    public Type widen() {
        Type tp = singleDeref();
        switch (tp) {
        case ThisType(_):
        case SingleType(_, _):
        case ConstantType(_, _):
            return tp.widen();
        default:
            return tp;
        }
    }

    private static Map widenMap = new Map() {
            public Type apply(Type t) {
                return t.widen();
            }
        };

    public static Type[] widen(Type[] tps) {
        return widenMap.map(tps);
    }

    /** The thistype or singleton type corresponding to values of this type.
      */
    public Type narrow() {
        switch (unalias()) {
        case TypeRef(Type pre, Symbol sym, Type[] args):
            if (sym.kind == CLASS) return sym.thisType();
            else return ThisType(sym);
        case CompoundType(_, _):
            return symbol().thisType();
        default:
            return this;
        }
    }

    /** If this type is a constant type, its underlying basetype;
     *  otherwise the type itself
     */
    public Type deconst() {
        switch (this) {
        case ConstantType(Type base, _):
            return base;
        default:
            return this;
        }
    }

    /** If this type is a parameterless method, its underlying resulttype;
     *  otherwise the type itself
     */
    public Type derefDef() {
        switch (this) {
        case PolyType(Symbol[] tparams, Type restp):
            if (tparams.length == 0) return restp;
        }
        return this;
    }

    /** The lower approximation of this type (which must be a typeref)
     */
    public Type loBound() {
        switch (unalias()) {
        case TypeRef(Type pre, Symbol sym, Type[] args):
            Type lb = Global.instance.definitions.ALL_TYPE();
            if (sym.kind == TYPE) {
                lb = pre.memberLoBound(sym);
            }
            if (lb.symbol() == Global.instance.definitions.ALL_CLASS &&
                this.symbol() != Global.instance.definitions.ALL_CLASS &&
                this.isSubType(Global.instance.definitions.ANYREF_TYPE())) {
                lb = Global.instance.definitions.ALLREF_TYPE();
            }
            return lb;
        default:
            throw new ApplicationError();
        }
    }

    /** If this is a this-type, named-type, applied type or single-type, its prefix,
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
     *  homomorphically to function types and poly types.
     */
    public Type instanceType() {
        switch (unalias()) {
        case TypeRef(Type pre, Symbol sym, Type[] args):
            if (sym != sym.thisSym())
                return sym.typeOfThis()
                    .asSeenFrom(pre, sym.owner())
                    .subst(sym.typeParams(), args);
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
        if (n == 100)
            throw new Type.Error("alias chain too long (recursive type alias?): " + this);
        switch (this) {
        case TypeVar(Type origin, Constraint constr):
            if (constr.inst != NoType) return constr.inst.unalias(n + 1);
            else return this;
        }
        return this;
    }

    /** The (prefix/argument-adapted) parents of this type.
     */
    public Type[] parents() {
        switch (unalias()) {
        case ThisType(_):
        case SingleType(_, _):
        case ConstantType(_, _):
            return singleDeref().parents();
        case TypeRef(Type pre, Symbol sym, Type[] args):
            if (sym.kind == CLASS) {
                assert sym.typeParams().length == args.length : sym + " " + ArrayApply.toString(args) + " " + sym.primaryConstructor().info();//debug
                return subst(asSeenFrom(sym.info().parents(), pre, sym.owner()),
                             sym.typeParams(), args);
            } else {
                return new Type[]{sym.info().asSeenFrom(pre, sym.owner())};
            }
        case CompoundType(Type[] parts, _):
            return parts;
        default:
            return Type.EMPTY_ARRAY;
        }
    }

    /** Get type parameters of method type (a PolyType or MethodType)
     * or EMPTY_ARRAY if method type is not polymorphic.
     */
    public Symbol[] typeParams() {
        switch (this) {
        case PolyType(Symbol[] tparams, _):
            return tparams;
        case MethodType(Symbol[] vparams, _):
            return Symbol.EMPTY_ARRAY;
        case TypeRef(_, Symbol sym, Type[] args):
            if (args.length == 0) return sym.typeParams();
            else return Symbol.EMPTY_ARRAY;
        default:
            return Symbol.EMPTY_ARRAY;
        }
    }

    /** Get value parameters of method type (a PolyType or MethodType)
     * or EMPTY_ARRAY if method type has no value parameter section.
     */
    public Symbol[] valueParams() {
        return valueParams(false);
    }
    private Symbol[] valueParams(boolean ok) {
        switch (this) {
        case PolyType(_, Type result):
            return result.valueParams(true);
        case MethodType(Symbol[] vparams, _):
            return vparams;
        default:
            if (ok) return Symbol.EMPTY_ARRAY;
            throw Debug.abort("illegal case", this);
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
    public Type[] alternativeTypes() {
        switch (this) {
        case OverloadedType(_, Type[] alttypes):
            return alttypes;
        default:
            return new Type[]{this};
        }
    }

    /** If this type is overloaded, its alternative symbols,
     *  otherwise an empty array.
     */
    public Symbol[] alternativeSymbols() {
        switch (this) {
        case OverloadedType(Symbol[] alts, _):
            return alts;
        default:
            return Symbol.EMPTY_ARRAY;
        }
    }

// Tests --------------------------------------------------------------------

    /** Is this type a an error type?
     */
    public boolean isError() {
        switch (this) {
        case ErrorType:
            return true;
        case ThisType(Symbol clasz):
            return clasz.isError();
        case SingleType(_, Symbol symbol):
            return symbol.isError();
        case TypeRef(_, Symbol symbol, _):
            return symbol.isError();
        case CompoundType(Type[] parts, Scope members):
            return symbol().isError();
        default:
            return false;
        }
    }

    /** Is this type a this type or singleton type?
     */
    public boolean isStable() {
        switch (unalias()) {
        case NoPrefix:
        case ThisType(_):
        case SingleType(_, _):
        case ConstantType(_, _):
            return true;
        case TypeRef(_, Symbol sym, _):
            if (sym.isParameter() && sym.isSynthetic() && sym.hasStableFlag()) return true;
            return false;
        default:
            return false;
        }
    }

    /** Is this type a legal prefix?
     */
    public boolean isLegalPrefix() {
        switch (unalias()) {
        case NoPrefix:
        case ThisType(_):
        case SingleType(_, _):
            return true;
        case TypeRef(_, Symbol sym, _):
            if (sym.isParameter() && sym.isSynthetic()) return true;
	    return false;
	    /*
            return sym.kind == CLASS &&
                ((sym.flags & JAVA) != 0 ||
                 (sym.flags & (TRAIT | ABSTRACT)) == 0);
	    */
        default:
            return false;
        }
    }

    /** Is this type a s thistype or singletype?
     */
    public boolean isSingletonType() {
        switch (this) {
        case ThisType(_): case SingleType(_, _): return true;
        default: return false;
        }
    }

    /** Is this type a reference to an object type?
     *  todo: replace by this.isSubType(global.definitions.ANY_TYPE())?
     */
    public boolean isObjectType() {
        switch (unalias()) {
        case ThisType(_):
        case SingleType(_, _):
        case ConstantType(_, _):
        case CompoundType(_, _):
        case TypeRef(_, _, _):
            return true;
        default:
            return false;
        }
    }

    /** Is this type of the form scala.FunctionN[T_1, ..., T_n, +T] or
     *  scala.AnyRef with scala.FunctionN[T_1, ..., T_n, +T] or
     *  java.lang.Object with scala.FunctionN[T_1, ..., T_n, +T]?
     */
    public boolean isFunctionType() {
        switch (this) {
        case TypeRef(Type pre, Symbol sym, Type[] args):
            Definitions definitions = Global.instance.definitions;
            return args.length > 0
                && args.length <= definitions.FUNCTION_COUNT
                && sym == definitions.FUNCTION_CLASS[args.length - 1];
        case CompoundType(Type[] parents, Scope members):
            Definitions definitions = Global.instance.definitions;
            return members.isEmpty() &&
                parents.length == 2 &&
                (parents[0].symbol() == definitions.OBJECT_CLASS ||
		 parents[0].symbol() == definitions.ANYREF_CLASS) &&
                parents[1].isFunctionType();
        }
        return false;
    }

    /** Is this a polymorphic method type?
     */
    public boolean isPolymorphic() {
        return typeParams().length > 0;
    }

    /** Is this a parameterized or polymorphic method type?
     */
    public boolean isParameterized() {
        switch (this) {
        case MethodType(_, _): return true;
        default: return isPolymorphic();
        }
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
        case SingleType(_, _):
        case ConstantType(_, _):
            return singleDeref().members();
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
            return new ErrorScope(Symbol.NONE).lookup(name);
        case ThisType(_):
        case SingleType(_, _):
        case ConstantType(_, _):
            return singleDeref().lookup(name);
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
        switch (this) {
        case ErrorType:
            return new ErrorScope(Symbol.NONE).lookup(name);
        case ThisType(_):
        case SingleType(_, _):
        case ConstantType(_, _):
            return singleDeref().lookupNonPrivate(name);
        case TypeRef(_, Symbol sym, _):
            return sym.info().lookupNonPrivate(name);
        case CompoundType(Type[] parts, Scope members):
            Symbol sym = members.lookup(name);
            if (sym.kind != NONE && (sym.flags & PRIVATE) == 0) return sym;
	    else return lookupNonPrivate(parts, name);
        default:
            return Symbol.NONE;
        }
    }

    public static Symbol lookupNonPrivate(Type[] parts, Name name) {
	// search base types in reverse; non-abstract members
	// take precedence over abstract ones.
	int i = parts.length;
	Symbol sym = Symbol.NONE;
	while (i > 0) {
	    i--;
	    Symbol sym1 = parts[i].lookupNonPrivate(name);
	    if (sym1.kind != NONE &&
		(sym.kind == NONE
		 ||
		 (sym.flags & DEFERRED) != 0 &&
		 (sym1.flags & DEFERRED) == 0
		 ||
		 (sym.flags & DEFERRED) == (sym1.flags & DEFERRED) &&
		 sym1.owner().isSubClass(sym.owner())))
		sym = sym1;
	}
	return sym;
    }

    /**
     * Looks up in the current type a symbol with the same name as the
     * given symbol and whose type (as seen from the given prefix) is
     * in the given relation to the type (as seen from the given
     * prefix) of the given symbol. If no such symbol is found,
     * returns NONE. Note that in some cases, the returned symbol may
     * be equal to the given one. The main purpose of this method is
     * look up overridden and overriding symbols.
     */
    public Symbol lookup(Symbol sym, Type pre, Relation relation) {
        assert !sym.isOverloaded(): Debug.show(sym);
        if (sym.isPrivate() || sym.isInitializer())
            return symbol().isSubClass(sym.owner()) ? sym : Symbol.NONE;
        Type symtype = pre.memberType(sym).derefDef();
        Symbol[] classes = classes();
        Symbol deferred = null;
        for (int i = 0; i < classes.length; i++) {
            if (deferred != null && deferred.isSubClass(classes[i])) continue;
            Symbol sym1 = classes[i].members().lookup(sym.name);
            switch (sym1.type()) {
            case NoType:
            case ErrorType:
                continue;
            case OverloadedType(Symbol[] alts, _):
                for (int j = 0; j < alts.length; j++)
                    if (areRelated(sym, symtype, relation, pre,alts[j],false)){
                        if (!alts[j].isDeferred()) return alts[j];
                        if (deferred == null) deferred = alts[j];
                    }
                continue;
            default:
                if (areRelated(sym, symtype, relation, pre, sym1, true)) {
                    if (!sym1.isDeferred()) return sym1;
                    if (deferred == null) deferred = sym1;
                }
                continue;
            }
        }
        return deferred == null ? Symbol.NONE : deferred;
    }
    //where
    private static boolean areRelated(
        Symbol sym, Type symtype, Relation relation, Type pre, Symbol sym1,
        boolean warn)
    {
        if (sym == sym1) return true;
        if (sym1.isPrivate() || sym1.isInitializer()) return false;
//         System.out.println("Is 'sym1' " + relation + " 'sym' in 'pre' ?"
//             + "\n  sym      : " + Debug.show(sym)
//             + "\n  sym1     : " + Debug.show(sym1)
//             + "\n  sym .type: " + sym.type()
//             + "\n  sym1.type: " + sym1.type()
//             + "\n  pre      : " + pre
//         );//DEBUG
        Type sym1type = pre.memberType(sym1).derefDef();
        if (sym1.isJava()) symtype = symtype.objParamToAny();
        if (sym1type.compareTo(symtype, relation)) return true;
        if (warn && Global.instance.debug) System.out.println(
            "'sym1' is not " + relation + " 'sym' in 'pre'"
            + "\n  sym      : " + Debug.show(sym)
            + "\n  sym1     : " + Debug.show(sym1)
            + "\n  sym .type: " + sym.type()
            + "\n  sym1.type: " + sym1.type()
            + "\n  pre      : " + pre
            + "\nsince 'sym1type' " + relation.toString(true) + " 'symtype'"
            + "\n  symtype  : " + symtype
            + "\n  sym1type : " + sym1type
        );//DEBUG
        return false;
    }
    private Symbol[] classes() {
        switch (this) {
        case ThisType(_):
        case SingleType(_, _):
        case ConstantType(_, _):
            return singleDeref().classes();
        case TypeRef(_, Symbol sym, _):
            return sym.info().classes();
        case CompoundType(Type[] parts, Scope members):
            return symbol(symbol().closure());
        default:
            return Symbol.EMPTY_ARRAY;
        }
    }
    static private Map objToAnyMap = new Map() {
	public Type apply(Type t) {
	    if (t.symbol() == Global.instance.definitions.OBJECT_CLASS)
		return Global.instance.definitions.ANY_TYPE();
	    else return t;
	}
    };

    private Type objParamToAny() {
	switch (this) {
	case MethodType(Symbol[] params, Type restp):
	    Symbol[] params1 = objToAnyMap.map(params);
	    if (params1 == params) return this;
	    else return MethodType(params1, restp);
	default:
	    return this;
	}
    }

// Set Owner ------------------------------------------------------------------

    public Type setOwner(Symbol owner) {
        switch (this) {
        case PolyType(Symbol[] tparams, Type restpe):
            Type restpe1 = restpe.setOwner(owner);
            if (restpe1 == restpe) return this;
            else return Type.PolyType(tparams, restpe1);
        case MethodType(Symbol[] params, Type restpe):
            Symbol[] params1 = params;
            if (params.length > 0 &&
                params[0].owner() != owner && params[0].owner() != Symbol.NONE) {
                params1 = new Symbol[params.length];
                for (int i = 0; i < params.length; i++)
                    params1[i] = params[i].cloneSymbol();
            }
            for (int i = 0; i < params.length; i++)
                params1[i].setOwner(owner);
            Type restpe1 = restpe.setOwner(owner);
            if (params1 == params && restpe1 == restpe) return this;
            else return Type.MethodType(params1, restpe1);
        default:
            return this;
        }
    }

// Maps --------------------------------------------------------------------------

    /** The type of type-to-type functions.
     */
    public abstract static class Map {

        public abstract Type apply(Type t);

        /**
         * This method assumes that all symbols in MethodTypes and
         * PolyTypes have already been cloned.
         */
        public Type applyParams(Type type) {
            switch (type) {

            case MethodType(Symbol[] vparams, Type result):
                map(vparams, true);
                Type result1 = applyParams(result);
                return result == result1 ? type : MethodType(vparams, result1);

            case PolyType(Symbol[] tparams, Type result):
                map(tparams, true);
                Type result1 = applyParams(result);
                return result == result1 ? type : PolyType(tparams, result1);

            default:
                return apply(type);
            }
        }

        /** Apply map to all top-level components of this type.
         */
        public Type map(Type tp) {
            switch (tp) {
            case ErrorType:
            case AnyType:
            case NoType:
            case NoPrefix:
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
            case ConstantType(Type base, AConstant value):
                Type base1 = apply(base);
                if (base1 == base) return tp;
                else return new ConstantType(base1, value);
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
                    Type tp1 = (tp.symbol().isCompoundSym()) ? compoundTypeWithOwner(tp.symbol().owner(), parts1, members2)
                        : compoundType(parts1, members2, tp.symbol());
                    Symbol[] syms1 = members1.elements();
                    Symbol[] syms2 = new Symbol[syms1.length];
                    for (int i = 0; i < syms2.length; i++) {
                        syms2[i] = syms1[i].cloneSymbol(tp1.symbol());
                    }
                    for (int i = 0; i < syms2.length; i++) {
                        syms2[i].setInfo(syms1[i].info().subst(syms1, syms2));
                        if (syms2[i].kind == TYPE) {
                            syms2[i].setLoBound(syms1[i].loBound().subst(syms1, syms2));
                            syms2[i].setVuBound(syms1[i].vuBound().subst(syms1, syms2));
			}
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

        public final Symbol map(Symbol sym) {
            return map(sym, false);
        }
        public Symbol map(Symbol sym, boolean dontClone) {
            Type tp = sym.info();
            Type tp1 = apply(tp);
            if (tp != tp1) {
                if (!dontClone) sym = sym.cloneSymbol();
                sym.setInfo(tp1);
                dontClone = true;
            }
            if (sym.kind == TYPE) {
                Type lb = sym.loBound();
                Type lb1 = apply(lb);
                if (lb != lb1) {
                    if (!dontClone) sym = sym.cloneSymbol();
                    sym.setLoBound(lb1);
                }
                Type vb = sym.vuBound();
                Type vb1 = apply(vb);
                if (vb != vb1) {
                    if (!dontClone) sym = sym.cloneSymbol();
                    sym.setVuBound(vb1);
                }
            }
            return sym;
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
        public final Symbol[] map(Symbol[] syms) {
            return map(syms, false);
        }
        public Symbol[] map(Symbol[] syms, boolean dontClone) {
            Symbol[] syms1 = syms;
            for (int i = 0; i < syms.length; i++) {
                Symbol sym = syms[i];
                Symbol sym1 = map(sym, dontClone);
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
                new SubstSymMap(syms, syms1).map(syms1, true);
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
        public Symbol map(Symbol sym, boolean dontClone) { return sym; }
        public Symbol[] map(Symbol[] syms, boolean dontClone) { return syms; }
        public Scope map(Scope s) { return s; }
    }

    public static final Map IdMap = new Map() {
        public Type apply(Type tp) { return tp; }
        public Type applyParams(Type tp) { return tp; }
        public Type map(Type tp) { return tp; }
        public Symbol map(Symbol sym, boolean dontClone) { return sym; }
        public Type[] map(Type[] tps) { return tps; }
        public Symbol[] map(Symbol[] syms, boolean dontClone) { return syms; }
        public Scope map(Scope scope) { return scope; }
    };

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
        case ConstantType(_, _):
            return singleDeref().baseType(clazz);

        case TypeRef(Type pre, Symbol sym, Type[] args):
            if (sym == clazz)
                return this;
            else if (sym.kind == TYPE)
                return sym.info()
                    .asSeenFrom(pre, sym.owner()).baseType(clazz);
            else if (sym.kind == ALIAS)
                return Type.NoType;
            else if (clazz.isCompoundSym())
                return NoType;
            else {
                return sym.baseType(clazz)
                    .asSeenFrom(pre, sym.owner())
                    .subst(sym.typeParams(), args);
            }

        case CompoundType(Type[] parts, _):
            for (int i = parts.length - 1; i >= 0; i--) {
                Type result = parts[i].baseType(clazz);
                if (result != NoType) return result;
            }
            break;

        case UnboxedArrayType(_):
            if (clazz == Global.instance.definitions.ANY_CLASS ||
                clazz == Global.instance.definitions.ANYREF_CLASS)
                return clazz.type();
        }
        return NoType;
    }

    /** A map to implement `asSeenFrom'.
     */
    static class AsSeenFromMap extends Map {

        private final Type pre;
        private final Symbol clazz;

        AsSeenFromMap(Type pre, Symbol clazz) {
            this.pre = pre;
            this.clazz = clazz;
        }

        public Type apply(Type type) {
            //System.out.println(type + " as seen from " + pre + "," + clazz);//DEBUG
            if (pre == NoType || clazz.kind != CLASS) return type;
            switch (type) {
            case ThisType(Symbol sym):
                return type.toPrefix(sym, pre, clazz);
            case TypeRef(Type prefix, Symbol sym, Type[] args):
                if (sym.owner().isPrimaryConstructor()) {
                    assert sym.kind == TYPE;
                    return type.toInstance(sym, pre, clazz);
                }
                return map(type);

            case SingleType(Type prefix, Symbol sym):
                try {
                    return map(type);
                } catch (Type.Malformed ex) {}
                return apply(type.singleDeref());

            default:
                return map(type);
            }
        }
    }
    //where
        Type toInstance(Symbol sym, Type pre, Symbol clazz) {
            if (pre == NoType || clazz.kind != CLASS)
                return this;
            Symbol ownclass = sym.owner().constructorClass();
            if (ownclass == clazz &&
                pre.widen().symbol().isSubClass(ownclass)) {
                switch (pre.baseType(ownclass)) {
                case TypeRef(_, Symbol basesym, Type[] baseargs):
                    Symbol[] baseparams = basesym.typeParams();
                    for (int i = 0; i < baseparams.length; i++) {
                        if (sym == baseparams[i]) return baseargs[i];
                    }
                    //System.out.println(sym + " " + basesym + " " + ArrayApply.toString(baseparams));//DEBUG
                    break;
                case ErrorType:
                    return ErrorType;
                }
                throw new ApplicationError(
                    this + " in " + ownclass + " cannot be instantiated from " + pre.widen()
                    );
            } else {
                return toInstance(sym, pre.baseType(clazz).prefix(), clazz.owner());
            }
        }

        Type toPrefix(Symbol sym, Type pre, Symbol clazz) {
            //System.out.println(this + ".toPrefix(" + sym + "," + pre + "," + clazz + ")");//DEBUG
            if (pre == NoType || clazz.kind != CLASS)
                return this;
            else if (sym.isSubClass(clazz) &&
                     pre.widen().symbol().isSubClass(sym))
                return pre;
            else
                return toPrefix(sym, pre.baseType(clazz).prefix(), clazz.owner());
        }

    /** This type as seen from prefix `pre' and class `clazz'. This means:
     *  Replace all thistypes of `clazz' or one of its subclasses by `pre'
     *  and instantiate all parameters by arguments of `pre'.
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
        return sym.info().asSeenFrom(this, sym.owner());
    }

    /** The type of `sym', seen as a member of this type.
     */
    public Type memberType(Symbol sym) {
        return sym.type().asSeenFrom(this, sym.owner());
    }

    /** The stabilized type of `sym', seen as a member of this type.
     */
    public Type memberStabilizedType(Symbol sym) {
        return sym.isStable() && this.isStable()
            ? Type.singleTypeMethod(this, sym)
            : this.memberType(sym);
    }

    /** The low bound of `sym', seen as a member of this type.
     */
    public Type memberLoBound(Symbol sym) {
        return sym.loBound().asSeenFrom(this, sym.owner());
    }

    /** The view bound of `sym', seen as a member of this type.
     */
    public Type memberVuBound(Symbol sym) {
        return sym.vuBound().asSeenFrom(this, sym.owner());
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
            case TypeRef(NoPrefix, Symbol sym, Type[] args):
                for (int i = 0; i < from.length; i++) {
                    if (matches(sym, from[i])) return replacement(i, t);
                }
                break;
            case SingleType(NoPrefix, Symbol sym):
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
        public SubstTypeMap(Symbol[] from, Type[] to) {
            super(from);
            this.to = to;
        }
        public Type replacement(int i, Type fromtp) {
            return to[i];
        }
        public SubstMap exclude(Symbol[] newfrom, Symbol[] excluded) {
            return new SubstTypeMap(newfrom, excludeTypes(from, excluded, to));
        }
    }


    /** A map for symbol/symbol substitutions which, instead of
     * cloning parameters, updates their symbol's types.
     */
    public static class UpdateSubstSymMap extends SubstSymMap {
        protected UpdateSubstSymMap(Symbol[] from, Symbol[] to) {
            super(from, to);
        }
        public Type apply(Type t) {
            switch (t) {
            case PolyType(Symbol[] params, Type result):
                // !!! Also update loBounds? How? loBound can only be set!
                for (int i = 0; i < params.length; i++) {
                    Type tp = params[i].nextType();
                    Type tp1 = apply(tp);
                    if (tp != tp1) params[i].updateInfo(tp1);
                }
                Type result1 = apply(result);
                if (result1 == result) return t;
                else return Type.PolyType(params, result1);
            case MethodType(Symbol[] params, Type result):
                for (int i = 0; i < params.length; i++) {
                    Type tp = params[i].nextType();
                    Type tp1 = apply(tp);
                    if (tp != tp1) params[i].updateInfo(tp1);
                }
                Type result1 = apply(result);
                if (result1 == result) return t;
                else return Type.MethodType(params, result1);
            default:
                return super.apply(t);
            }
        }
        public Symbol map(Symbol sym, boolean dontClone) { return sym; }
        public Symbol[] map(Symbol[] syms, boolean dontClone) { return syms; }
        public Scope map(Scope s) { return s; }
    }

    /** Returns the given non-updating symbol/symbol substitution. */
    public static Map getSubst(Symbol[] from, Symbol[] to) {
        return getSubst(from, to, false);
    }

    /** Returns the given (updating?) symbol/symbol substitution. */
    public static Map getSubst(Symbol[] from, Symbol[] to, boolean update) {
        if (from.length == 0 && to.length == 0) return IdMap;
        if (update) return new UpdateSubstSymMap(from, to);
        return new SubstSymMap(from, to);
    }

    /** Returns the given non-updating symbol/type substitution. */
    public static Map getSubst(Symbol[] from, Type[] to) {
        if (from.length == 0 && to.length == 0) return IdMap;
        return new SubstTypeMap(from, to);
    }

    /** Substitute symbols `to' for occurrences of symbols `from' in this type.
     */
    public Type subst(Symbol[] from, Symbol[] to) {
        if (to.length != 0 && from != to) {//!!!
            assert from.length == to.length
                : this + ": " + from.length + " != " + to.length;
            return new SubstSymMap(from, to).apply(this);
        } else return this;
    }

    /** Substitute symbols `to' for occurrences of symbols `from' in these types.
     */
    public static Type[] subst(Type[] these, Symbol[] from, Symbol[] to) {
        if (these.length != 0 && to.length != 0 && from != to) {
            assert from.length == to.length;
            return new SubstSymMap(from, to).map(these);
        } else return these;
    }

    /** Substitute types `to' for occurrences of symbols `from' in this type.
     */
    public Type subst(Symbol[] from, Type[] to) {
        if (to.length != 0) {
            assert from.length == to.length
                : this + ": " + Debug.show(from) + " <> " + ArrayApply.toString(to);
            return new SubstTypeMap(from, to).apply(this);
        } else return this;
    }

    /** Substitute types `to' for occurrences of symbols `from' in these types.
     */
    public static Type[] subst(Type[] these, Symbol[] from, Type[] to) {
        if (these.length != 0 && to.length != 0) {
            assert from.length == to.length;
            return new SubstTypeMap(from, to).map(these);
        } else return these;
    }

    /**
     * A map that substitutes ThisTypes of a given class by a given
     * type. All occurrences of the type parameters of the given class
     * are replaced by the type arguments extracted from the given
     * type. Furthermore, the prefixes of the given type are used to
     * substitute, in the same way, the ThisTypes of the outer classes
     * of the given class.
     *
     * object Foo {
     *   class C[D] { class I[J]; }
     *   val c: C[Int] = new C[Int];
     *   class M[N] extends c.I[N];
     * }
     *
     * In the code above, a ThisTypeMap of class "I" and type
     * "ThisType(M)", would do the following substitutions:
     *
     *   - ThisType(I)       ->  ThisType(M)
     *   - TypeRef(_, J, _)  ->  TypeRef(_, N, -)
     *   - ThisType(C)       ->  SingleType(ThisType(Foo), c)
     *   - TypeRef(_, D, _)  ->  TypeRef(_, Int, _)
     */
    private static class ThisTypeMap extends Map {

        private static Map create(Symbol clasz, Type type) {
            HashMap subst = getSubst(clasz, type, 0);
            return subst == null ? IdMap : new ThisTypeMap(subst);
        }

        private static HashMap getSubst(Symbol clasz, Type type, int capacity){
            switch (type) {
            case NoPrefix:
                return getSubst(capacity);
            case ThisType(Symbol symbol):
                if (symbol == clasz) return getSubst(capacity);
            }
            Type base = type.baseType(clasz);
            switch (base) {
            case TypeRef(Type prefix, Symbol symbol, Type[] args):
                capacity += 1 + args.length;
                HashMap subst = getSubst(clasz.owner(), prefix, capacity);
                subst.put(clasz, type);
                Symbol[] params = clasz.typeParams();
                assert symbol == clasz && args.length == params.length:
                    type + " @ " + Debug.show(clasz) + " -> " + base;
                for (int i = 0; i < params.length; i++) {
                    assert params[i].isParameter(): Debug.show(params[i]);
                    subst.put(params[i], args[i]);
                }
                return subst;
            default:
                throw Debug.abort("illegal case",
                    type + " @ " + Debug.show(clasz) + " -> " + base);
            }
        }

        private static HashMap getSubst(int capacity) {
            return capacity == 0 ? null : new HashMap(capacity);
        }

        private final HashMap/*<Symbol,Type>*/ subst;

        private ThisTypeMap(HashMap subst) {
            this.subst = subst;
        }

        public Type apply(Type type) {
            switch (type) {
            case ThisType(Symbol symbol):
                Object lookup = subst.get(symbol);
                if (lookup == null) break;
                return (Type)lookup;
            case TypeRef(NoPrefix, Symbol symbol, Type[] args):
                if (!symbol.isParameter()) break;
                assert args.length == 0: type;
                Object lookup = subst.get(symbol);
                if (lookup == null) break;
                return (Type)lookup;
            }
            return map(type);
        }

        public String toString() {
            return subst.toString();
        }

    }

    /** Returns a ThisTypeMap of given class and type. */
    public static Map getThisTypeMap(Symbol clasz, Type type) {
        return ThisTypeMap.create(clasz, type);
    }

    /** A map for substitutions of thistypes.
     */
    public static class SubstThisMap extends Map {
        Symbol from;
        Type to;
        public SubstThisMap(Symbol from, Type to) {
            this.from = from;
            this.to = to;
        }
        public SubstThisMap(Symbol oldSym, Symbol newSym) {
            this(oldSym, newSym.thisType());
        }
        public Type apply(Type type) {
            switch (type) {
            case ThisType(Symbol sym):
                return sym == from ? to : type;
            default:
                return map(type);
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
            if (!result) {
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

// Cloning ---------------------------------------------------------------

    /** Returns a shallow copy of the given array. */
    public static Type[] cloneArray(Type[] array) {
        return cloneArray(0, array, 0);
    }

    /**
     * Returns a shallow copy of the given array prefixed by "prefix"
     * null items.
     */
    public static Type[] cloneArray(int prefix, Type[] array) {
        return cloneArray(prefix, array, 0);
    }

    /**
     * Returns a shallow copy of the given array suffixed by "suffix"
     * null items.
     */
    public static Type[] cloneArray(Type[] array, int suffix) {
        return cloneArray(0, array, suffix);
    }

    /**
     * Returns a shallow copy of the given array prefixed by "prefix"
     * null items and suffixed by "suffix" null items.
     */
    public static Type[] cloneArray(int prefix, Type[] array, int suffix) {
        assert prefix >= 0 && suffix >= 0: prefix + " - " + suffix;
        int size = prefix + array.length + suffix;
        if (size == 0) return EMPTY_ARRAY;
        Type[] clone = new Type[size];
        for (int i = 0; i < array.length; i++) clone[prefix + i] = array[i];
        return clone;
    }

    /** Returns the concatenation of the two arrays. */
    public static Type[] concat(Type[] array1, Type[] array2) {
        if (array1.length == 0) return array2;
        if (array2.length == 0) return array1;
        Type[] clone = cloneArray(array1.length, array2);
        for (int i = 0; i < array1.length; i++) clone[i] = array1[i];
        return clone;
    }

    /**
     * Clones a type i.e. returns a new type where all symbols in
     * MethodTypes and PolyTypes and CompoundTypes have been cloned.
     */
    public Type cloneType(Symbol oldOwner, Symbol newOwner) {
        SymbolCloner cloner = new SymbolCloner();
        cloner.owners.put(oldOwner, newOwner);
        return cloner.cloneType(this);
    }

    /**
     * Clones a type i.e. returns a new type where all symbols in
     * MethodTypes and PolyTypes have been cloned. This method
     * performs no substitution on the type of the cloned symbols.
     * Typically, the type of those symbols will be fixed later by
     * applying some Map.applyParams method to the returned type.
     */
    public Type cloneTypeNoSubst(SymbolCloner cloner) {
        switch (this) {

        case MethodType(Symbol[] vparams, Type result):
            Symbol[] clones = cloner.cloneSymbols(vparams);
            return Type.MethodType(clones, result.cloneTypeNoSubst(cloner));

        case PolyType(Symbol[] tparams, Type result):
            Symbol[] clones = cloner.cloneSymbols(tparams);
            return Type.PolyType(clones, result.cloneTypeNoSubst(cloner));

        default:
            return this;
        }
    }


// Comparisons ------------------------------------------------------------------

    /** Type relations */
    public static class Relation {
        public case SubType;   // this SubType   that <=> this.isSubType(that)
        public case SameType;  // this SameType  that <=> this.isSameAs(that)
        public case SuperType; // this SuperType that <=> that.isSubType(this)

        public String toString() {
            return toString(false);
        }
        public String toString(boolean negate) {
            switch (this) {
            case SubType  : return negate ? "!<=" : "<=";
            case SameType : return negate ? "!==" : "=";
            case SuperType: return negate ? "!>=" : ">=";
            default       : throw Debug.abort("unknown relation", this);
            }
        }
    }

    /** Is this type in given relation to that type?
     */
    public boolean compareTo(Type that, Relation relation) {
        switch (relation) {
        case SubType  : return this.isSubType(that);
        case SameType : return this.isSameAs(that);
        case SuperType: return that.isSubType(this);
        default       : throw Debug.abort("unknown relation", relation);
        }
    }

    /** Is this type a subtype of that type?
     */
    public boolean isSubType(Type that) {
        if (explainSwitch) {
            for (int i = 0; i < indent; i++) System.out.print("  ");
            System.out.println(this + " < " + that + "?");
            indent++;
        }
        boolean result = isSubType0(that);
        if (explainSwitch) {
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
        case NoPrefix:
            return false;

        case ThisType(_):
        case SingleType(_, _):
            switch (this) {
            case ThisType(_):
            case SingleType(_, _):
                return this.isSameAs(that);
            default:
                if (this.isSameAs(that)) return true;
            }
            break;

        case ConstantType(_, _):
            switch (this) {
            case ConstantType(Type base, _):
                return this.isSameAs(that) || base.isSubType(that);
            }
            break;

        case TypeRef(Type pre1, Symbol sym1, Type[] args1):
            switch (this) {
            case TypeRef(Type pre, Symbol sym, Type[] args):
                if (sym == sym1 && pre.isSubType(pre1) &&
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
                    if (!p1.type().isSameAs(p.type()) ||
                        (p1.flags & (DEF | REPEATED)) != (p.flags & (DEF | REPEATED)))
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
                    if (!ps1[i].info().subst(ps1, ps).isSubType(ps[i].info()) ||
                        !ps[i].loBound().isSubType(ps1[i].loBound().subst(ps1, ps)) ||
                        !ps1[i].vuBound().subst(ps1, ps).isSubType(ps[i].vuBound()))
                        return false;
                return res.isSubType(res1.subst(ps1, ps));
            }
            break;

        case OverloadedType(Symbol[] alts1, Type[] alttypes1):
            for (int i = 0; i < alttypes1.length; i++) {
                if (!isSubType(alttypes1[i]))
                    return false;
            }
            return true;

        case UnboxedType(int tag1):
            switch (this) {
            case UnboxedType(int tag):
                return tag == tag1;
            }
            break;

        case UnboxedArrayType(Type elemtp1):
            switch (this) {
            case UnboxedArrayType(Type elemtp):
                return elemtp.isSubType(elemtp1);
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
        case NoPrefix:
            return false;
        case ThisType(_):
        case SingleType(_, _):
            if (this.isSameAs(that)) return true;
            if (this.singleDeref().isSubType(that)) return true;
            break;
        case ConstantType(_, _):
            if (this.singleDeref().isSubType(that)) return true;
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
            case TypeRef(_, Symbol sym1, _):
                if (sym1.kind == TYPE && this.isSubType(that.loBound()))
                    return true;
            }
            if (sym == Global.instance.definitions.ALL_CLASS)
                return that.isSubType(Global.instance.definitions.ANY_TYPE());
            else if (sym == Global.instance.definitions.ALLREF_CLASS)
                return
                    that.symbol() == Global.instance.definitions.ANY_CLASS ||
                    (that.symbol() != Global.instance.definitions.ALL_CLASS &&
                     that.isSubType(Global.instance.definitions.ANYREF_TYPE()));
            break;

        case OverloadedType(Symbol[] alts, Type[] alttypes):
            for (int i = 0; i < alttypes.length; i++) {
                if (alttypes[i].isSubType(that)) return true;
            }
            break;

        case CompoundType(Type[] parts, Scope members):
            int i = 0;
            while (i < parts.length) {
                if (parts[i].isSubType(that)) return true;
                i++;
            }
            break;

        case UnboxedArrayType(_):
            if (Global.instance.definitions.OBJECT_TYPE().isSubType(that))
                return true;
            // !!! we should probably also test for Clonable, Serializable, ...
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
            } else if ((tparams[i].flags & CONTRAVARIANT) != 0) {
                //System.out.println("contra: " + these[i] + " " + those[i] + " " + those[i].isSubType(these[i]));//DEBUG
                if (!those[i].isSubType(these[i])) return false;
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
        for (Scope.SymbolIterator it = s.iterator();
	     it.hasNext();) {
            if (!specializes(it.next())) return false;
        }
        return true;
    }

    /** Does this type implement symbol `sym1' with same or stronger type?
     */
    public boolean specializes(Symbol sym1) {
        if (explainSwitch) {
            for (int i = 0; i < indent; i++) System.out.print("  ");
            System.out.println(this + " specializes " + sym1 + "?");
            indent++;
        }
        boolean result = specializes0(sym1);
        if (explainSwitch) {
            indent--;
            for (int i = 0; i < indent; i++) System.out.print("  ");
            System.out.println(result);
        }
        return result;
    }

    private boolean specializes0(Symbol sym1) {
        Type self = narrow();
        Symbol[] tparams = symbol().typeParams();
        Type[] targs = typeArgs();
        Symbol sym = lookup(sym1.name);
        return
            sym.kind != NONE &&
            (sym == sym1
             ||
             (sym.kind == sym1.kind || sym1.kind == TYPE) &&
             self.memberInfo(sym).subst(tparams, targs)
             .isSubType(sym1.info().substThis(sym1.owner(), self)) &&
             sym1.loBound().substThis(sym1.owner(), self)
             .isSubType(self.memberLoBound(sym).subst(tparams, targs)) &&
             self.memberVuBound(sym).subst(tparams, targs)
             .isSubType(sym1.vuBound().substThis(sym1.owner(), self))
             ||
             (sym.kind == TYPE && sym1.kind == ALIAS &&
              sym1.info().unalias().isSameAs(sym.type())));
    }

    /** Is this type the same as that type?
     */
    public boolean isSameAs(Type that) {
        if (explainSwitch) {
            for (int i = 0; i < indent; i++) System.out.print("  ");
            System.out.println(this + " = " + that + "?");
            indent++;
        }
        boolean result = isSameAs0(that);
        if (explainSwitch) {
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
                    this.singleDeref().isSingletonType() &&
                    this.singleDeref().isSameAs(that)
                    ||
                    that.singleDeref().isSingletonType() &&
                    this.isSameAs(that.singleDeref());
            default:
                if (deAlias(this) != this)
                    return deAlias(this).isSameAs(that);
            }
            break;

        case SingleType(Type pre, Symbol sym):
            switch (that) {
            case SingleType(Type pre1, Symbol sym1):
                return sym == sym1 && pre.isSameAs(pre1)
                    ||
                    this.singleDeref().isSingletonType() &&
                    this.singleDeref().isSameAs(that)
                    ||
                    that.singleDeref().isSingletonType() &&
                    this.isSameAs(that.singleDeref());
            case ThisType(Symbol sym1):
                return sym.isModule()
                    && sym.moduleClass() == sym1
                    && pre.isSameAs(sym1.owner().thisType())
                    ||
                    this.singleDeref().isSingletonType() &&
                    this.singleDeref().isSameAs(that)
                    ||
                    that.singleDeref().isSingletonType() &&
                    this.isSameAs(that.singleDeref());
            default:
                if (deAlias(this) != this)
                    return deAlias(this).isSameAs(that);
            }
            break;

        case ConstantType(Type base, AConstant value):
            switch (that) {
            case ConstantType(Type base1, AConstant value1):
                return base.isSameAs(base1) && value.equals(value1);
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
                        (p1.flags & (DEF | REPEATED)) != (p.flags & (DEF | REPEATED)))
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
                    if (!ps1[i].info().subst(ps1, ps).isSameAs(ps[i].info()) ||
                        !ps1[i].loBound().subst(ps1, ps).isSameAs(ps[i].loBound()) ||
                        !ps1[i].vuBound().subst(ps1, ps).isSameAs(ps[i].vuBound()))
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
        case NoPrefix:
            return false;
        case TypeVar(Type origin, Constraint constr):
            if (constr.inst != NoType) return constr.inst.isSameAs(this);
            else return constr.instantiate(this.any2typevar());
        case ThisType(_):
        case SingleType(_, _):
            if (deAlias(that) != that)
                return this.isSameAs(deAlias(that));
        }

        switch (this) {
        case NoType:
        case NoPrefix:
            return false;
        case TypeVar(Type origin, Constraint constr):
            if (constr.inst != NoType) return constr.inst.isSameAs(that);
            else return constr.instantiate(that.any2typevar());
        }

        return false;
    }
    //where

        static Type deAlias(Type tp) {
            switch (tp) {
            case ThisType(_):
            case SingleType(_, _):
                Type tp1 = tp.singleDeref();
                if (tp1.isStable()) return deAlias(tp1);
            }
            return tp;
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
        for (Scope.SymbolIterator it = s2.iterator(); it.hasNext(); ) {
            Symbol sym2 = it.next();
            // todo: handle overloaded
            Symbol sym1 = s1.lookup(sym2.name);
            if (sym1.kind != sym2.kind ||
                !sym1.info().isSameAs(
                    sym2.info().substThis(
                        sym2.owner(), sym1.owner().thisType())) ||
                !sym1.loBound().isSameAs(
                    sym2.loBound().substThis(
                        sym2.owner(), sym1.owner().thisType())) ||
                !sym1.vuBound().isSameAs(
                    sym2.vuBound().substThis(
                        sym2.owner(), sym1.owner().thisType())))
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

    /** Does this type match type `tp', so that corresponding symbols with
     *  the two types would be taken to override each other?
     */
    public boolean overrides(Type tp) {
	switch (this) {
	case OverloadedType(Symbol[] alts, Type[] alttypes):
	    for (int i = 0; i < alttypes.length; i++) {
		if (alttypes[i].overrides(tp)) return true;
	    }
	    return false;
	default:
	    switch (tp) {
	    case MethodType(Symbol[] ps1, Type res1):
		switch (this) {
		case MethodType(Symbol[] ps, Type res):
		    if (ps.length != ps1.length) return false;
		    for (int i = 0; i < ps.length; i++) {
			Symbol p1 = ps1[i];
			Symbol p = ps[i];
			if (!p1.type().isSameAs(p.type()) ||
			    (p1.flags & (DEF | REPEATED)) != (p.flags & (DEF | REPEATED)))
			    return false;
		    }
		    return res.overrides(res1);
		}
		return false;

	    case PolyType(Symbol[] ps1, Type res1):
		switch (this) {
		case PolyType(Symbol[] ps, Type res):
		    if (ps.length != ps1.length) return false;
		    for (int i = 0; i < ps.length; i++)
			if (!ps1[i].info().subst(ps1, ps).isSubType(ps[i].info()) ||
			    !ps[i].loBound().isSubType(ps1[i].loBound().subst(ps1, ps)) ||
			    !ps1[i].vuBound().subst(ps1, ps).isSubType(ps[i].vuBound()))
			    return false;
		    return res.overrides(res1.subst(ps1, ps));
		}
		return false;

	    case OverloadedType(_, _):
		throw new ApplicationError("overrides inapplicable for " + tp);

	    default:
		switch (this) {
		case MethodType(_, _): case PolyType(_, _): return false;
		default: return true;
		}
	    }
	}
    }

// Closures and Least Upper Bounds ---------------------------------------------------

    /** The closure of this type, i.e. the widened type itself followed by all
     *  its direct and indirect (pre-) base types, sorted by Symbol.isLess().
     */
    public Type[] closure() {
        switch (this.widen().unalias()) {
        case TypeRef(Type pre, Symbol sym, Type[] args):
            return subst(
                asSeenFrom(sym.closure(), pre, sym.owner()),
                sym.typeParams(), args);

        case CompoundType(Type[] parts, Scope members):
/*
	    if (symbol().isCompoundSym()) {
		Type[][] closures = new Type[parts.length][];
		for (int i = 0; i < parts.length; i++)
		    closures[i] = parts[i].closure();
		return union(closures);
	    } else {
*/
	    return symbol().closure();

        default:
            return new Type[]{this};
        }
    }

    /** return union of array of closures. It is assumed that
     *  for any two base types with the same class symbols the later one
     *  is a subtype of the former.
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
                        cltype.symbol() == min.symbol()) {
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

    /** return intersection of non-empty array of closures
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
            Symbol minsym = null;
            for (int i = 0; i < index.length; i++) {
                if (index[i] == closures[i].length) break L;
                Symbol clsym = closures[i][index[i]].symbol();
                if (minsym == null || clsym.isLess(minsym)) minsym = clsym;
            }

            boolean agree = true;
            // bump all indices that start with minimal element
            for (int i = 0; i < index.length; i++) {
                Type cltype = closures[i][index[i]];
                if (cltype.symbol() == minsym) {
                    mintypes[i] = cltype;
                    index[i] = index[i] + 1;
                } else {
                    agree = false;
                }
            }
            if (agree) {
                Type mintype = argLub(mintypes);
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
    static Type argLub(Type[] tps) {
        tps = elimRedundant(tps, true);
        if (tps.length == 1) return tps[0];

        Type pre = tps[0].prefix();
        Symbol sym = tps[0].symbol();
        Symbol[] tparams = sym.typeParams();
        Type[] args = new Type[tparams.length];
        Type[][] argss = new Type[args.length][tps.length];
        for (int i = 0; i < tps.length; i++) {
            switch (tps[i]) {
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
                assert false : tps[i];
            }
        }
        for (int j = 0; j < args.length; j++) {
            if ((tparams[j].flags & COVARIANT) != 0)
                args[j] = lub(argss[j]);
            else if ((tparams[j].flags & CONTRAVARIANT) != 0)
                args[j] = glb(argss[j]);
            else return NoType;
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
            switch (tps[i]) {
            case ErrorType:
                return new Type[]{ErrorType};
            case MethodType(_, _):
            case PolyType(_, _):
            case OverloadedType(_, _):
                return new Type[]{NoType};
            default:
                assert tps[i].isObjectType(): tps[i];
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

    static int recCount = 0;
    static boolean giveUp = false;
    static int recLimit = 10;

    public static Type lub(Type[] tps) {
	if (recCount == recLimit) {
	    giveUp = true;
	    return Global.instance.definitions.ANY_TYPE();
	} else {
	    recCount++;
	    Type result = lub0(tps);
	    recCount--;
	    if (recCount == 0) {
		if (giveUp) {
		    giveUp = false;
		    throw new Error("failure to compute least upper bound of types " +
				    ArrayApply.toString(tps, "", " and ", ";\n") +
				    "an approximation is: " + result + ";\n" +
				    "additional type annotations are needed");
		} else {
		    giveUp = false;
		}
	    }
	    return result;
	}
    }

    /** Return the least upper bound of non-empty array of types `tps'.
     */
    public static Type lub0(Type[] tps) {
        //System.out.println("lub" + ArrayApply.toString(tps));//DEBUG

        if (tps.length == 0) return Global.instance.definitions.ALL_TYPE();

        //If all types are method types with same parameters,
        //compute lub of their result types.
        switch (tps[0]) {
        case PolyType(Symbol[] tparams, _):
            return polyLub(tps, tparams);
        case MethodType(Symbol[] vparams, _):
            return methodLub(tps, vparams);
        }

        // remove types that are subtypes of some other type.
        tps = elimRedundant(tps, true);
        if (tps.length == 1) return tps[0];

        // singleDeref singletypes and try again
        Type[] tps1 = tps;
        for (int i = 0; i < tps.length; i++) {
            Type tp1 = tps[i].singleDeref();
            if (tp1 != tps[i] && tps1 == tps) {
                tps1 = new Type[tps.length];
                System.arraycopy(tps, 0, tps1, 0, i);
            }
            tps1[i] = tp1;
        }
        if (tps1 != tps) return lub0(tps1);

        // intersect closures and build frontier.
        Type[][] closures = new Type[tps.length][];
        for (int i = 0; i < tps.length; i++) {
            closures[i] = tps[i].closure();
        }
        Type[] allBaseTypes = intersection(closures);
        Type[] leastBaseTypes = frontier(allBaseTypes);
        assert leastBaseTypes.length > 0 : ArrayApply.toString(tps);

        // add refinements where necessary
        Scope members = new Scope();
        Type lubType = compoundTypeWithOwner(Symbol.NONE, leastBaseTypes, members); // !!! NONE
	/*
        Type lubThisType = lubType.narrow();
        //System.out.println("lubtype = " + lubType);//DEBUG

        Symbol[] rsyms = new Symbol[tps.length];
        Type[] rtps = new Type[tps.length];
        Type[] rlbs = new Type[tps.length];
        for (int i = 0; i < allBaseTypes.length; i++) {
            for (Scope.SymbolIterator it = allBaseTypes[i].members().iterator();
                 it.hasNext(); ) {
                Symbol sym = it.next();
                Name name = sym.name;
                if ((sym.flags & PRIVATE) == 0 && lubType.lookup(name) == sym) {
                    Type symType = memberTp(lubThisType, sym);
                    Type symLoBound = lubThisType.memberLoBound(sym);
                    int j = 0;
                    while (j < tps.length) {
                        rsyms[j] = tps[j].lookupNonPrivate(name);
                        if (rsyms[j] == sym) break;
                        rtps[j] = memberTp(tps[j], rsyms[j])
                            .substThis(tps[j].symbol(), lubThisType);
                        rlbs[j] = tps[j].memberLoBound(rsyms[j])
                            .substThis(tps[j].symbol(), lubThisType);
                        if (rtps[j].isSameAs(symType) &&
                            rlbs[j].isSameAs(symLoBound)) break;
                        j++;
                    }
                    if (j == tps.length) {
			if (Global.instance.debug)
			    System.out.println("refinement lub for " +
			    ArrayApply.toString(rsyms) + ":" + ArrayApply.toString(rtps));//debug
                        Symbol lubSym = lub(rsyms, rtps, rlbs, lubType.symbol());
                        if (lubSym.kind != NONE &&
                            !(lubSym.kind == sym.kind &&
                              lubSym.info().isSameAs(symType) &&
                              lubSym.loBound().isSameAs(symType)))
                            members.enter(lubSym);
                    }
                }
            }
        }
        //System.out.print("lub "); System.out.print(ArrayApply.toString(tps)); System.out.println(" = " + lubType);//DEBUG
	*/
        if (leastBaseTypes.length == 1 && members.isEmpty())
            return leastBaseTypes[0];
        else return lubType;
    }
    //where
        private static Type memberTp(Type base, Symbol sym) {
            return sym.kind == CLASS ? base.memberType(sym) : base.memberInfo(sym);
        }

    private static Type polyLub(Type[] tps, Symbol[] tparams0) {
        Type[][] hiboundss = new Type[tparams0.length][tps.length];
        Type[][] loboundss = new Type[tparams0.length][tps.length];
        Type[][] vuboundss = new Type[tparams0.length][tps.length];
        Type[] restps   = new Type[tps.length];
        for (int i = 0; i < tps.length; i++) {
            switch (tps[i]) {
            case PolyType(Symbol[] tparams, Type restp):
                if (tparams.length == tparams0.length) {
                    for (int j = 0; j < tparams0.length; j++) {
                        hiboundss[j][i] = tparams[j].info()
                            .subst(tparams, tparams0);
                        loboundss[j][i] = tparams[j].loBound()
                            .subst(tparams, tparams0);
                        vuboundss[j][i] = tparams[j].vuBound()
                            .subst(tparams, tparams0);
                    }
                    restps[i] = restp.subst(tparams, tparams0);
                } else {
                    return Type.NoType;
                }
                break;
            default:
                return Type.NoType;
            }
        }
        Type[] hibounds = new Type[tparams0.length];
        Type[] lobounds = new Type[tparams0.length];
        Type[] vubounds = new Type[tparams0.length];
        for (int j = 0; j < tparams0.length; j++) {
            hibounds[j] = glb(hiboundss[j]);
            lobounds[j] = lub(loboundss[j]);
            vubounds[j] = glb(vuboundss[j]);
        }
        Symbol[] tparams = new Symbol[tparams0.length];
        for (int j = 0; j < tparams.length; j++) {
            tparams[j] = tparams0[j].cloneSymbol(Symbol.NONE)
                .setInfo(hibounds[j].subst(tparams0, tparams))
                .setLoBound(lobounds[j].subst(tparams0, tparams))
                .setVuBound(vubounds[j].subst(tparams0, tparams));
        }
        return Type.PolyType(tparams, lub(restps).subst(tparams0, tparams));
    }

    private static Type methodLub(Type[] tps, Symbol[] vparams0) {
        Type[] restps = new Type[tps.length];
        for (int i = 0; i < tps.length; i++) {
            switch (tps[i]) {
            case MethodType(Symbol[] vparams, Type restp):
                if (vparams.length != vparams0.length)
                    return Type.NoType;
                for (int j = 0; j < vparams.length; j++)
                    if (!vparams[j].type().isSameAs(vparams0[j].type()) ||
                        (vparams[j].flags & (DEF | REPEATED)) !=
                        (vparams0[j].flags & (DEF | REPEATED)))
                        return Type.NoType;
                restps[i] = restp;
            }
        }
        Symbol[] vparams = new Symbol[vparams0.length];
        for (int j = 0; j < vparams.length; j++) {
            vparams[j] = vparams0[j].cloneSymbol(Symbol.NONE);
        }
        return Type.MethodType(vparams, lub(restps));
    }

    private static Symbol lub(Symbol[] syms, Type[] tps, Type[] lbs, Symbol owner) {
        //System.out.println("lub" + ArrayApply.toString(syms));//DEBUG
        int lubKind = syms[0].kind;
        for (int i = 1; i < syms.length; i++) {
            Symbol sym = syms[i];
            if (sym.isError()) return Symbol.NONE;
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
            lubSym = owner.newTerm(syms[0].pos, 0, syms[0].name);
            break;
        case TYPE: case ALIAS: case CLASS:
            lubSym = owner.newAbstractType(syms[0].pos, 0, syms[0].name);
            lubSym.setLoBound(glb(lbs));
            break;
        default:
            throw new ApplicationError();
        }
        lubSym.setInfo(lubType.setOwner(lubSym));
        return lubSym;
    }

    public static Type glb(Type[] tps) {
	if (recCount == recLimit) {
	    giveUp = true;
	    return Global.instance.definitions.ALL_TYPE();
	} else {
	    recCount++;
	    Type result = glb0(tps);
	    recCount--;
	    if (recCount == 0) {
		if (giveUp) {
		    giveUp = false;
		    throw new Error("failure to compute greatest lower bound of types " +
				    ArrayApply.toString(tps, "", " and ", ";\n") +
				    "an approximation is: " + result + ";\n" +
				    "additional type annotations are needed");
		} else {
		    giveUp = false;
		}
	    }
	    return result;
	}
    }

    public static Type glb0(Type[] tps) {
        if (tps.length == 0) return Global.instance.definitions.ANY_TYPE();

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
                if (!members.isEmpty())
                    comptl = new Type.List(tps[i], comptl);
                for (int j = 0; j < parents.length; j++)
                    treftl = new Type.List(parents[j], treftl);
                break;
            case ThisType(_):
            case SingleType(_, _):
            case ConstantType(_, _):
                return Global.instance.definitions.ALL_TYPE();
            }
        }

        CompoundType glbType = compoundTypeWithOwner(Symbol.NONE, Type.EMPTY_ARRAY, new Scope()); // !!! NONE
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
                    Global.instance.definitions.ALLREF_TYPE(), treftl);
            }
        }

        // eliminate redudant typerefs
        Type[] treftypes = elimRedundant(treftl.toArrayReverse(), false);
        if (treftypes.length != 1 || !glbType.members.isEmpty()) {
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
            // class, replace them by lub/glb of arguments or lower bound.
            Type lb = NoType;
            for (int i = 0;
                 i < treftypes.length &&
                     lb.symbol() != Global.instance.definitions.ALL_CLASS;
                 i++) {
                for (int j = 0; j < i; j++) {
                    if (treftypes[j].symbol() == treftypes[i].symbol())
                        lb = argGlb(treftypes[j], treftypes[i]);
                }
            }
            if (lb != NoType) return lb;
        }

        if (treftypes.length == 1 && glbType.members.isEmpty()) {
            return treftypes[0];
        } else {
            glbType.parts = treftypes;
            return glbType;
        }
    }

    private static Type argGlb(Type tp1, Type tp2) {
        switch (tp1) {
        case TypeRef(Type pre1, Symbol sym1, Type[] args1):
            switch (tp2) {
            case TypeRef(Type pre2, Symbol sym2, Type[] args2):
                assert sym1 == sym2;
                if (pre1.isSameAs(pre2)) {
                    Symbol[] tparams = sym1.typeParams();
                    Type[] args = new Type[tparams.length];
                    for (int i = 0; i < tparams.length; i++) {
                        if (args1[i].isSameAs(args2[i]))
                            args[i] = args1[i];
                        else if ((tparams[i].flags & COVARIANT) != 0)
                            args[i]= lub(new Type[]{args1[i], args2[i]});
                        else if ((tparams[i].flags & CONTRAVARIANT) != 0)
                            args[i]= glb(new Type[]{args1[i], args2[i]});
                        else
                            return glb(new Type[]{tp1.loBound(), tp2.loBound()});
                    }
                    return typeRef(pre1, sym1, args);
                }
            }
        }
        return glb(new Type[]{tp1.loBound(), tp2.loBound()});
    }

    /** Set scope `result' to glb of scopes `ss'. Return true iff succeeded.
     */
    private static boolean setGlb(Scope result, Scope[] ss, Type glbThisType) {
        for (int i = 0; i < ss.length; i++)
            for (Scope.SymbolIterator it = ss[i].iterator(); it.hasNext(); )
                if (!addMember(result, it.next(), glbThisType)) return false;
        return true;
    }

    /** Add member `sym' to scope `s'. If`s' has already a member with same name,
     *  overwrite its info/low bound to form glb of both symbols.
     */
    private static boolean addMember(Scope s, Symbol sym, Type glbThisType) {
        Type syminfo = sym.info().substThis(sym.owner(), glbThisType);
        Type symlb = sym.loBound().substThis(sym.owner(), glbThisType);
        Type symvb = sym.vuBound().substThis(sym.owner(), glbThisType);
        Scope.Entry e = s.lookupEntry(sym.name);
        if (e == Scope.Entry.NONE) {
            Symbol sym1 = sym.cloneSymbol(glbThisType.symbol());
            sym1.setInfo(syminfo);
            if (sym1.kind == TYPE) {
		sym1.setLoBound(symlb);
		sym1.setVuBound(symvb);
	    }
            s.enter(sym1);
        } else {
            Type einfo = e.sym.info();
            if (einfo.isSameAs(syminfo)) {
            } else if (einfo.isSubType(syminfo) && sym.kind != ALIAS) {
            } else if (syminfo.isSubType(einfo) && e.sym.kind != ALIAS) {
                e.sym.setInfo(syminfo);
            } else if (sym.kind == VAL && e.sym.kind == VAL ||
                       sym.kind == TYPE && e.sym.kind == TYPE) {
                e.sym.setInfo(glb(new Type[]{einfo, syminfo}).setOwner(e.sym));
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
                Type evb = e.sym.vuBound();
                if (evb.isSameAs(symvb)) {
                } else if (evb.isSubType(symvb)) {
                } else if (symvb.isSubType(evb)) {
                    e.sym.setVuBound(symvb);
                } else {
                    e.sym.setVuBound(glb(new Type[]{evb, symvb}));
                }
            }
        }
        return true;
    }

    private static Type polyGlb(Type[] tps, Symbol[] tparams0) {
        Type[][] hiboundss = new Type[tparams0.length][tps.length];
        Type[][] loboundss = new Type[tparams0.length][tps.length];
        Type[][] vuboundss = new Type[tparams0.length][tps.length];
        Type[] restps   = new Type[tps.length];
        for (int i = 0; i < tps.length; i++) {
            switch (tps[i]) {
            case PolyType(Symbol[] tparams, Type restp):
                if (tparams.length == tparams0.length) {
                    for (int j = 0; j < tparams0.length; j++) {
                        hiboundss[j][i] = tparams[j].info()
                            .subst(tparams, tparams0);
                        loboundss[j][i] = tparams[j].loBound()
                            .subst(tparams, tparams0);
                        vuboundss[j][i] = tparams[j].vuBound()
                            .subst(tparams, tparams0);
                    }
                    restps[i] = restp.subst(tparams, tparams0);
                } else {
                    return Type.NoType;
                }
                break;
            default:
                return Type.NoType;
            }
        }
        Type[] hibounds = new Type[tparams0.length];
        Type[] lobounds = new Type[tparams0.length];
        Type[] vubounds = new Type[tparams0.length];
        for (int j = 0; j < tparams0.length; j++) {
            hibounds[j] = lub(hiboundss[j]);
            lobounds[j] = glb(loboundss[j]);
            vubounds[j] = lub(vuboundss[j]);
        }
        Symbol[] tparams = new Symbol[tparams0.length];
        for (int j = 0; j < tparams.length; j++) {
            tparams[j] = tparams0[j].cloneSymbol(Symbol.NONE)
                .setInfo(hibounds[j].subst(tparams0, tparams))
                .setLoBound(lobounds[j].subst(tparams0, tparams))
                .setVuBound(vubounds[j].subst(tparams0, tparams));
        }
        return Type.PolyType(tparams, glb(restps).subst(tparams0, tparams));
    }

    private static Type methodGlb(Type[] tps, Symbol[] vparams0) {
        Type[] restps = new Type[tps.length];
        for (int i = 0; i < tps.length; i++) {
            switch (tps[i]) {
            case MethodType(Symbol[] vparams, Type restp):
                if (vparams.length != vparams0.length)
                    return Type.NoType;
                for (int j = 0; j < vparams.length; j++)
                    if (!vparams[i].type().isSameAs(vparams0[i].type()) ||
                        (vparams[i].flags & (DEF | REPEATED)) !=
                        (vparams0[i].flags & (DEF | REPEATED)))
                        return Type.NoType;
                restps[i] = restp;
            }
        }
        Symbol[] vparams = new Symbol[vparams0.length];
        for (int j = 0; j < vparams.length; j++) {
            vparams[j] = vparams0[j].cloneSymbol(Symbol.NONE);
        }
        return Type.MethodType(vparams, glb(restps));
    }

// Erasure --------------------------------------------------------------------------

    public static Map erasureMap = new MapOnlyTypes() {
        public Type apply(Type t) { return t.erasure(); }
    };

    private static final Type[] unboxedType =
        new Type[LastUnboxedTag + 1 - FirstUnboxedTag];
    private static final Name[] unboxedName =
        new Name[LastUnboxedTag + 1 - FirstUnboxedTag];
    private static final Symbol[] boxedSymbol =
        new Symbol[LastUnboxedTag + 1 - FirstUnboxedTag];

    private static void mkStdClassType(int kind, String unboxedstr, Symbol boxedsym) {
        unboxedType[kind - FirstUnboxedTag] = UnboxedType(kind);
        unboxedName[kind - FirstUnboxedTag] = Name.fromString(unboxedstr);
        boxedSymbol[kind - FirstUnboxedTag] = boxedsym;
    }

    static void initializeUnboxedTypes(Definitions definitions) {
        mkStdClassType(BYTE, "byte", definitions.BYTE_CLASS);
        mkStdClassType(SHORT, "short", definitions.SHORT_CLASS);
        mkStdClassType(CHAR, "char", definitions.CHAR_CLASS);
        mkStdClassType(INT, "int", definitions.INT_CLASS);
        mkStdClassType(LONG, "long", definitions.LONG_CLASS);
        mkStdClassType(FLOAT, "float", definitions.FLOAT_CLASS);
        mkStdClassType(DOUBLE, "double", definitions.DOUBLE_CLASS);
        mkStdClassType(BOOLEAN, "boolean", definitions.BOOLEAN_CLASS);
        mkStdClassType(UNIT, "void", definitions.UNIT_CLASS);
    }

    /** Return unboxed type of given kind.
     */
    public static Type unboxedType(int kind) {
        return unboxedType[kind - FirstUnboxedTag];
    }

    /** Return the name of unboxed type of given kind.
    */
    public static Name unboxedName(int kind) {
        return unboxedName[kind - FirstUnboxedTag];
    }

    /** If type is boxed, return its unboxed equivalent; otherwise return the type
     *  itself.
     */
    public Type unbox() {
        switch (this) {
        case TypeRef(_, Symbol clasz, Type[] args):
            if (args.length == 0) {
                for (int i = 0; i < boxedSymbol.length; i++)
                    if (boxedSymbol[i] == clasz) return unboxedType[i];
            } else if (args.length == 1) {
                Definitions definitions = Global.instance.definitions;
                if (clasz == definitions.ARRAY_CLASS) {
                    Type item = args[0];
                    Type bound = item.upperBound();
                    // todo: check with Philippe if this is what we want.
                    if (item.symbol().isClass() ||
                        (bound.symbol() != definitions.ANY_CLASS &&
                            bound.symbol() != definitions.ANYVAL_CLASS))
                    {
                        return UnboxedArrayType(args[0].erasure());
                    }
                }
            }
        }
        return this;
    }
    //where
        private Type upperBound() {
            switch (this) {
            case TypeRef(Type pre, Symbol sym, Type[] args):
                if (sym.kind == TYPE)
                    return pre.memberInfo(sym).upperBound();
            }
            return this;
        }

    /** Return the erasure of this type.
     */
    public Type erasure() {
        switch (this) {
        case ThisType(_):
        case SingleType(_, _):
        case ConstantType(_, _):
            return singleDeref().erasure();
        case TypeRef(Type pre, Symbol sym, Type[] args):
            switch (sym.kind) {
            case ALIAS: case TYPE:
                return sym.info().asSeenFrom(pre, sym.owner()).erasure();

            case CLASS:
                Definitions definitions = Global.instance.definitions;
                if (sym == definitions.UNIT_CLASS) return this;
                if (sym == definitions.OBJECT_CLASS ||
                    sym == definitions.ALL_CLASS ||
                    sym == definitions.ALLREF_CLASS)
                    return Type.typeRef(NoPrefix, definitions.ANY_CLASS, EMPTY_ARRAY);
                else {
                    Type this1 = unbox();
                    if (this1 != this) return this1;
                    else return Type.typeRef(NoPrefix, sym, EMPTY_ARRAY);
                }

            default: throw new ApplicationError(sym + " has wrong kind: " + sym.kind);
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
        Type erasure = erasure();
        if (Global.instance.definitions.UNIT_CLASS == erasure.symbol())
            erasure = erasure.unbox();
        return erasure;
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
            return ERRORtpe;
        case NoType:
            return NOtpe;
        case NoPrefix:
            return NOpre;
        case ThisType(Symbol sym):
            return THIStpe
                ^ (sym.hashCode() * 41);
        case TypeRef(Type pre, Symbol sym, Type[] args):
            return TYPEREFtpe
                ^ (pre.hashCode() * 41)
                ^ (sym.hashCode() * (41*41))
                ^ (hashCode(args) * (41*41*41));
        case SingleType(Type pre, Symbol sym):
            return SINGLEtpe
                ^ (pre.hashCode() * 41)
                ^ (sym.hashCode() * (41*41));
        case ConstantType(Type base, AConstant value):
            return CONSTANTtpe
                ^ (base.hashCode() * 41)
                ^ (value.hashCode() * (41*41));
        case CompoundType(Type[] parts, Scope members):
            return symbol().hashCode();
            //return COMPOUNDtpe
            //  ^ (hashCode(parts) * 41)
            //  ^ (members.hashCode() * (41 * 41));
        case MethodType(Symbol[] vparams, Type result):
            int h = METHODtpe;
            for (int i = 0; i < vparams.length; i++)
                h = (h << 4) ^ (vparams[i].flags & SOURCEFLAGS);
            return h
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
            throw new ApplicationError("bad type for hashCode: " + this);
        }
    }

    public static int hashCode(Object[] elems) {
        int h = 0;
        for (int i = 0; i < elems.length; i++)
            h = h * 41 + elems[i].hashCode();
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
            case NoPrefix:
                return that == NoPrefix;
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
            case ConstantType(Type base, AConstant value):
                switch (that) {
                case ConstantType(Type base1, AConstant value1):
                    return base.equals(base1) && value.equals(value1);
                default: return false;
                }
            case CompoundType(Type[] parts, Scope members):
                switch (that) {
                case CompoundType(Type[] parts1, Scope members1):
                    return this.symbol() == that.symbol();
                    //return parts.equals(parts1) && members.equals(members1);
                default: return false;
                }
            case MethodType(Symbol[] vparams, Type result):
                switch (that) {
                case MethodType(Symbol[] vparams1, Type result1):
                    if (vparams.length != vparams1.length)
                        return false;
                    for (int i = 0; i < vparams.length; i++)
                        if ((vparams[i].flags & SOURCEFLAGS) !=
                            (vparams1[i].flags & SOURCEFLAGS))
                            return false;
                    return
                        equals(Symbol.type(vparams), Symbol.type(vparams1)) &&
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

    public static class Malformed extends Error {
        public Malformed(Type pre, String tp) {
            super("malformed type: " + pre + "#" + tp);
        }
    }

    /** A class for throwing type errors
     */
    public static class VarianceError extends Error {
        public VarianceError(String msg) {
            super(msg);
        }
    }

    public static void explainTypes(Type found, Type required) {
        if (Global.instance.explaintypes) {
            boolean s = explainSwitch;
            explainSwitch = true;
            found.isSubType(required);
            explainSwitch = s;
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
    case ConstantType(Type base, Object value):
    case CompoundType(Type[] parts, Scope members):
    case MethodType(Symbol[] vparams, Type result):
    case PolyType(Symbol[] tparams, Type result):
    case OverloadedType(Symbol[] alts, Type[] alttypes):
*/

