/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.ast;

import scalac.Global;
import scalac.ast.Tree.*;
import scalac.symtab.*;
import scalac.typechecker.Infer;
import scalac.util.*;
import scalac.ApplicationError;

/**
 * This class provides method to build attributed trees.
 *
 * @author     Martin Odersky, Christine Roeckl
 * @version    1.0
 */
public class TreeGen implements Kinds, Modifiers, TypeTags {

    //########################################################################
    // Private Fields

    /** The global environment */
    private final Global global;

    /** The global definitions */
    private final Definitions definitions;

    /** The tree factory */
    private final TreeFactory make;

    /** the type inferencer
     */
    final Infer infer;

    /** Initializes this instance. */
    public TreeGen(Global global) {
	this(global, global.make);
    }

    /** Initializes this instance. */
    public TreeGen(Global global, TreeFactory make) {
        this.global = global;
	this.definitions = global.definitions;
        this.make = make;
	this.infer = new Infer(global, this, make);
    }

    //########################################################################
    // Public Methods - Building types

    /** Builds type references corresponding to given symbols. */
    public Tree[] mkTypeRefs(int pos, Symbol[] syms) {
        if (syms.length == 0) return Tree.EMPTY_ARRAY;
        Tree[] trees = new Tree[syms.length];
        for (int i = 0; i < trees.length; i++)
            trees[i] = mkTypeRef(pos, syms[i]);
	return trees;
    }

    /** Builds a type reference corresponding to given symbol. */
    public Tree mkTypeRef(int pos, Symbol sym) {
        assert sym.kind == TYPE: Debug.show(sym);
	sym.flags |= ACCESSED;
	return mkType(pos, sym.nextType());
    }

    /** Builds trees corresponding to given types. */
    public Tree[] mkTypes(int pos, Type[] types) {
        if (types.length == 0) return Tree.EMPTY_ARRAY;
        Tree[] trees = new Tree[types.length];
        for (int i = 0; i < trees.length; i++)
            trees[i] = mkType(pos, types[i]);
        return trees;
    }

    /** Builds a tree corresponding to given type. */
    public Tree mkType(int pos, Type type) {
	return TypeTerm(pos, type);
    }

    /** Builds a TypeTerm node corresponding to given type. */
    public TypeTerm TypeTerm(int pos, Type type) {
        TypeTerm tree = make.TypeTerm(pos);
        tree.setType(type);
        return tree;
    }

    //########################################################################
    // Public Methods - Building constants

    /** Builds a literal. */
    public Tree mkLit(int pos, Object value) {
        if (value instanceof Boolean) return mkBooleanLit(pos, (Boolean)value);
        if (value instanceof Byte) return mkByteLit(pos, (Byte)value);
        if (value instanceof Short) return mkShortLit(pos, (Short)value);
        if (value instanceof Character) return mkCharLit(pos,(Character)value);
        if (value instanceof Integer) return mkIntLit(pos, (Integer)value);
        if (value instanceof Long) return mkLongLit(pos, (Long)value);
        if (value instanceof Float) return mkFloatLit(pos, (Float)value);
        if (value instanceof Double) return mkDoubleLit(pos, (Double)value);
        if (value instanceof String) return mkStringLit(pos, (String)value);
        throw Debug.abort("unknown literal class " + value.getClass(), value);
    }

    /** Builds a unit literal. */
    public Tree mkUnitLit(int pos) {
        return make.Block(pos, Tree.EMPTY_ARRAY).
            setType(definitions.UNIT_TYPE);
    }

    /** Builds a boolean literal. */
    public Tree mkBooleanLit(int pos, boolean value) {
        return mkBooleanLit(pos, value ? Boolean.TRUE : Boolean.FALSE);
    }

    /** Builds a boolean literal. */
    public Tree mkBooleanLit(int pos, Boolean value) {
        return make.Literal(pos, value).setType(definitions.BOOLEAN_TYPE);
    }

    /** Builds a byte literal. */
    public Tree mkByteLit(int pos, byte value) {
        return mkByteLit(pos, new Byte(value));
    }

    /** Builds a byte literal. */
    public Tree mkByteLit(int pos, Byte value) {
        return make.Literal(pos, value).setType(definitions.BYTE_TYPE);
    }

    /** Builds a short literal. */
    public Tree mkShortLit(int pos, short value) {
        return mkShortLit(pos, new Short(value));
    }

    /** Builds a short literal. */
    public Tree mkShortLit(int pos, Short value) {
        return make.Literal(pos, value).setType(definitions.SHORT_TYPE);
    }

    /** Builds a character literal. */
    public Tree mkCharLit(int pos, char value) {
        return mkCharLit(pos, new Character(value));
    }

    /** Builds a character literal. */
    public Tree mkCharLit(int pos, Character value) {
        return make.Literal(pos, value).setType(definitions.CHAR_TYPE);
    }

    /** Builds an integer literal */
    public Tree mkIntLit(int pos, int value) {
        return mkIntLit(pos, new Integer(value));
    }

    /** Builds an integer literal */
    public Tree mkIntLit(int pos, Integer value) {
        return make.Literal(pos, value).setType(definitions.INT_TYPE);
    }

    /** Builds a long literal. */
    public Tree mkLongLit(int pos, long value) {
        return mkLongLit(pos, new Long(value));
    }

    /** Builds a long literal. */
    public Tree mkLongLit(int pos, Long value) {
        return make.Literal(pos, value).setType(definitions.LONG_TYPE);
    }

    /** Builds a float literal. */
    public Tree mkFloatLit(int pos, float value) {
        return mkFloatLit(pos, new Float(value));
    }

    /** Builds a float literal. */
    public Tree mkFloatLit(int pos, Float value) {
        return make.Literal(pos, value).setType(definitions.FLOAT_TYPE);
    }

    /** Builds a double literal. */
    public Tree mkDoubleLit(int pos, double value) {
        return mkDoubleLit(pos, new Double(value));
    }

    /** Builds a double literal. */
    public Tree mkDoubleLit(int pos, Double value) {
        return make.Literal(pos, value).setType(definitions.DOUBLE_TYPE);
    }

    /** Builds a string literal. */
    public Tree mkStringLit(int pos, String value) {
        return make.Literal(pos, value).setType(definitions.JAVA_STRING_TYPE);
    }

    /** Builds a null literal. */
    public Tree mkNullLit(int pos) {
        return Ident(pos, definitions.NULL);
    }

    /** Builds a zero literal. */
    public Tree mkZeroLit(int pos) {
        return Ident(pos, definitions.ZERO);
    }

    /** Builds a default zero value according to given type tag. */
    public Tree mkDefaultValue(int pos, int tag) {
        switch (tag) {
        case UNIT   : return mkUnitLit(pos);
        case BOOLEAN: return mkBooleanLit(pos, false);
        case BYTE   : return mkByteLit(pos, (byte)0);
        case SHORT  : return mkShortLit(pos, (short)0);
        case CHAR   : return mkCharLit(pos, '\0');
        case INT    : return mkIntLit(pos, 0);
        case LONG   : return mkLongLit(pos, 0l);
        case FLOAT  : return mkFloatLit(pos, 0f);
        case DOUBLE : return mkDoubleLit(pos, 0d);
        default     : throw Debug.abort("unknown type tag: " + tag);
        }
    }

    /** Builds a default zero value according to given type. */
    public Tree mkDefaultValue(int pos, Type type) {
	if (type.isSubType(definitions.ANYREF_TYPE)) return mkNullLit(pos);
        switch (type.unbox()) {
        case UnboxedType(int tag): return mkDefaultValue(pos, tag);
        }
        return mkZeroLit(pos);
    }

    //########################################################################
    // Public Methods - Building references

    /** Builds references corresponding to given symbols. */
    public Tree[] mkRefs(int pos, Symbol[] syms) {
        if (syms.length == 0) return Tree.EMPTY_ARRAY;
        Tree[] trees = new Tree[syms.length];
        for (int i = 0; i < trees.length; i++)
            trees[i] = mkRef(pos, syms[i]);
	return trees;
    }

    /** Builds a reference corresponding to given symbol. */
    public Tree mkRef(int pos, Symbol sym) {
	return mkRef(pos, sym.owner().thisType(), sym);
    }

    /** Builds a reference corresponding to given prefix & symbol. */
    public Tree mkRef(int pos, Type pre, Symbol sym) {
	if (pre.isSameAs(Type.localThisType) || pre.symbol().isRoot())
	    return Ident(pos, sym);
	else
	    return Select(pos, mkStableId(pos, pre), sym);
    }

    /** Builds a reference corresponding to given stable prefix. */
    public Tree mkStableId(int pos, Type pre) {
        switch (pre.expandModuleThis()) {
	case ThisType(Symbol sym):
	    return This(pos, sym);
        case SingleType(Type pre1, Symbol sym):
	    return mkRef(pos, pre1, sym);
        default:
            throw Debug.abort("illegal case", pre);
        }
    }

    /** Builds a This node corresponding to given class. */
    public This This(int pos, Symbol clazz) {
        assert clazz.isClass(): Debug.show(clazz);
        This tree = make.This(pos, clazz);
        global.nextPhase();
        tree.setType(clazz.thisType());
        global.prevPhase();
        return tree;
    }

    /** Builds a Super node corresponding to given class. */
    public Super Super(int pos, Symbol clazz) {
        assert clazz.isClass(): Debug.show(clazz);
        Super tree = make.Super(pos, clazz, TypeNames.EMPTY);
        global.nextPhase();
        tree.setType(clazz.thisType());
        global.prevPhase();
        return tree;
    }

    /** Builds an Ident node corresponding to given symbol. */
    public Ident Ident(int pos, Symbol sym) {
        assert sym.isTerm(): Debug.show(sym);
	sym.flags |= ACCESSED;
	Ident tree = make.Ident(pos, sym);
        global.nextPhase();
	if (sym.isStable())
            tree.setType(Type.singleType(sym.owner().thisType(), sym));
        else
            tree.setType(sym.type());
        global.prevPhase();
        return tree;
    }

    /**
     * Builds a Select node corresponding to given symbol selected
     * from given qualifier.
     */
    public Select Select(int pos, Tree qual, Symbol sym) {
	assert sym.isTerm(): Debug.show(sym);
	sym.flags |= ACCESSED | SELECTOR;
	Select tree = make.Select(pos, sym, qual);
        global.nextPhase();
	if (sym.isStable() && qual.type.isStable())
            tree.setType(Type.singleType(qual.type, sym));
        else
            tree.setType(qual.type.memberType(sym));
        global.prevPhase();
        return tree;
    }
    public Select Select(Tree qual, Symbol sym) {
	return Select(qual.pos, qual, sym);
    }

    //########################################################################
    // Public Methods - Building applications

    /**
     * Builds calls to primary constructors of given types with given
     * value arguments.
     */
    public Tree[] mkPrimaryConstrs(int pos, Type[] types, Tree[][] vargs) {
        assert types.length == vargs.length: Debug.show(types, " -- ", vargs);
        Tree[] trees = new Tree[types.length];
        for (int i = 0; i < trees.length; i++)
            trees[i] = mkPrimaryConstr(pos, types[i], vargs[i]);
        return trees;
    }

    /**
     * Builds calls to primary constructors of given types with no
     * value arguments.
     */
    public Tree[] mkPrimaryConstrs(int pos, Type[] types) {
        Tree[] trees = new Tree[types.length];
        for (int i = 0; i < trees.length; i++)
            trees[i] = mkPrimaryConstr(pos, types[i]);
        return trees;
    }

    /**
     * Builds a call to the primary constructor of given type with
     * given value arguments. Missing type arguments are extracted
     * from the given type.
     */
    public Tree mkPrimaryConstr(int pos, Type type, Tree[] vargs) {
	switch (type) {
	case TypeRef(Type pre, Symbol clazz, Type[] targs):
            return mkPrimaryConstr(pos, pre, clazz, targs, vargs);
	default:
	    throw Debug.abort("invalid type", type);
	}
    }

    /**
     * Builds a call to the primary constructor of given type with no
     * value arguments. Missing type arguments are extracted from the
     * given type.
     */
    public Tree mkPrimaryConstr(int pos, Type type) {
        return mkPrimaryConstr(pos, type, Tree.EMPTY_ARRAY);
    }

    /**
     * Builds a call to the primary constructor of given class with
     * given type and value arguments.
     */
    public Tree mkPrimaryConstr(int pos, Type pre, Symbol clazz, Type[] targs,
        Tree[] vargs)
    {
        global.nextPhase();
        Symbol constr = clazz.primaryConstructor();
        global.prevPhase();
        return mkApply(pos, mkRef(pos, constr), targs, vargs);
    }
    public Tree mkPrimaryConstr(int pos, Symbol clazz,Type[]targs,Tree[]vargs){
        return mkPrimaryConstr(pos,clazz.owner().thisType(),clazz,targs,vargs);
    }

    /**
     * Builds a call to the primary constructor of given class with
     * given type arguments and no value arguments.
     */
    public Tree mkPrimaryConstr(int pos, Type pre, Symbol clazz, Type[] targs){
        return mkPrimaryConstr(pos, pre, clazz, targs, Tree.EMPTY_ARRAY);
    }
    public Tree mkPrimaryConstr(int pos, Symbol clazz, Type[] targs) {
        return mkPrimaryConstr(pos, clazz.owner().thisType(), clazz, targs);
    }

    /**
     * Builds a call to the primary constructor of given class with no
     * type and value arguments.
     */
    public Tree mkPrimaryConstr(int pos, Type pre, Symbol clazz) {
        return mkPrimaryConstr(pos, pre, clazz, Type.EMPTY_ARRAY);
    }
    public Tree mkPrimaryConstr(int pos, Symbol clazz) {
        return mkPrimaryConstr(pos, clazz.owner().thisType(), clazz);
    }

    /** Builds an application with given function and arguments. */
    public Tree mkApply(int pos, Tree fn, Type[] targs, Tree[] vargs) {
        if (targs.length != 0) fn = TypeApply(pos, fn, mkTypes(pos, targs));
        return Apply(pos, fn, vargs);
    }
    public Tree mkApply(Tree fn, Type[] targs, Tree[] vargs) {
        return mkApply(fn.pos, fn, targs, vargs);
    }
    public Tree mkApply(int pos, Tree fn, Tree[] targs, Tree[] vargs) {
        if (targs.length != 0) fn = TypeApply(pos, fn, targs);
        return Apply(pos, fn, vargs);
    }
    public Tree mkApply(Tree fn, Tree[] targs, Tree[] vargs) {
        return mkApply(fn.pos, fn, targs, vargs);
    }

    /**
     * Builds an application with given function and type arguments
     * and with no value arguments.
     */
    public Tree mkApply(int pos, Tree fn, Type[] targs) {
        return mkApply(pos, fn, targs, Tree.EMPTY_ARRAY);
    }
    public Tree mkApply(Tree fn, Type[] targs) {
        return mkApply(fn.pos, fn, targs);
    }
    public Tree mkApply(int pos, Tree fn, Tree[] targs) {
        return mkApply(pos, fn, targs, Tree.EMPTY_ARRAY);
    }
    public Tree mkApply(Tree fn, Tree[] targs) {
        return mkApply(fn.pos, fn, targs);
    }

    /** Builds a TypeApply node with given function and arguments. */
    public TypeApply TypeApply(int pos, Tree fn, Tree[] targs) {
	try {
	    switch (fn.type) {
	    case Type.OverloadedType(Symbol[] alts, Type[] alttypes):
                global.nextPhase();
		infer.polyAlternative(fn, alts, alttypes, targs.length);
                global.prevPhase();
	    }
	    switch (fn.type) {
	    case Type.PolyType(Symbol[] tparams, Type restpe):
                global.nextPhase();
                restpe = restpe.subst(tparams, Tree.typeOf(targs));
                global.prevPhase();
		return (TypeApply)make.TypeApply(pos, fn, targs).setType(restpe);
	    }
	} catch (Type.Error ex) {
	}
	throw new ApplicationError("poly type required", fn.type);
    }
    public TypeApply TypeApply(Tree fn, Tree[] targs) {
      return TypeApply(fn.pos, fn, targs);
    }

    /** Builds an Apply node with given function and arguments. */
    public Apply Apply(int pos, Tree fn, Tree[] vargs) {
 	try {
	    switch (fn.type) {
	    case Type.OverloadedType(Symbol[] alts, Type[] alttypes):
                global.nextPhase();
		infer.methodAlternative(fn, alts, alttypes,
					Tree.typeOf(vargs), Type.AnyType);
                global.prevPhase();
	    }
	    switch (fn.type) {
	    case Type.MethodType(Symbol[] vparams, Type restpe):
		return (Apply)make.Apply(pos, fn, vargs).setType(restpe);
	    }
	} catch (Type.Error ex) {
	}
	throw new ApplicationError("method type required", fn.type);
    }
    public Apply Apply(Tree fn, Tree[] vargs) {
        return Apply(fn.pos, fn, vargs);
    }

    /** Builds an Apply node with given function and no arguments. */
    public Apply Apply(int pos, Tree fn) {
        return Apply(pos, fn, Tree.EMPTY_ARRAY);
    }
    public Apply Apply(Tree fn) {
        return Apply(fn.pos, fn);
    }

    //########################################################################
    // Public Methods - Building expressions

    /** Builds a cast with given value and type. */
    public Tree mkAsInstanceOf(int pos, Tree value, Type type) {
        return mkApply(pos, Select(value, definitions.AS), new Type[] {type});
    }
    public Tree mkAsInstanceOf(Tree value, Type type) {
        return mkAsInstanceOf(value.pos, value, type);
    }

    /** Builds a Template node with given symbol, parents and body. */
    public Template Template(int pos, Symbol local, Tree[]parents, Tree[]body){
        Template tree = make.Template(pos, local, parents, body);
        tree.setType(Type.NoType);
        return tree;
    }

    /** Builds a Block node with given statements. */
    public Tree Block(int pos, Tree[] stats) {
        Block tree = make.Block(pos, stats);
        tree.setType(stats.length == 0
            ? definitions.UNIT_TYPE
            : stats[stats.length - 1].type);
	return tree;
    }

    /** Builds a Block node with given non-empty statements list. */
    public Tree Block(Tree[] stats) {
	return Block(stats[0].pos, stats);
    }

    /** Builds an Assign node corresponding to "<lhs> = <rhs>". */
    public Assign Assign(int pos, Tree lhs, Tree rhs) {
        Assign tree = make.Assign(pos, lhs, rhs);
        tree.setType(definitions.UNIT_TYPE);
        return tree;
    }
    public Assign Assign(Tree lhs, Tree rhs) {
        return Assign(lhs.pos, lhs, rhs);
    }

    /** Builds an If node with given condition and branches. */
    public If If(int pos, Tree cond, Tree thenpart, Tree elsepart) {
	If tree = make.If(pos, cond, thenpart, elsepart);
        global.nextPhase();
        if (thenpart.type.isSameAs(elsepart.type))
            tree.setType(thenpart.type);
        else
            tree.setType(Type.lub(new Type[] {thenpart.type, elsepart.type}));
        global.prevPhase();
        return tree;
    }
    public If If(Tree cond, Tree thenpart, Tree elsepart) {
	return If(cond.pos, cond, thenpart, elsepart);
    }

    /** Builds a New node corresponding to "new <constr>". */
    public Tree New(int pos, Tree constr) {
        Tree[] constrs = { constr };
	Template templ = Template(pos, Symbol.NONE, constrs, Tree.EMPTY_ARRAY);
	New tree = make.New(pos, templ);
        tree.setType(constr.type);
        return tree;
    }
    public Tree New(Tree constr) {
        return New(constr.pos, constr);
    }

    /** Builds a Typed nodes with given value and type. */
    public Typed Typed(int pos, Tree value, Type type) {
        Typed tree = make.Typed(pos, value, TypeTerm(pos, type));
        tree.setType(type);
        return tree;
    }
    public Typed Typed(Tree value, Type type) {
        return Typed(value.pos, value, type);
    }

    //########################################################################
    // Public Methods - Building definitions

    /** Builds the type parameter section of given symbol. */
    public AbsTypeDef[] mkTypeParamsOf(Symbol sym) {
        Symbol[] tparams = sym.nextTypeParams();
        AbsTypeDef[] trees = new AbsTypeDef[tparams.length];
        for (int i = 0; i < tparams.length; i++)
            trees[i] = mkTypeParam(tparams[i]);
        return trees;
    }

    /** Builds the value parameter section of given symbol. */
    public ValDef[][] mkParamsOf(Symbol sym) {
        global.nextPhase();
        if (sym.isClass()) sym = sym.primaryConstructor();
        Type type = sym.type();
        global.prevPhase();
        ValDef[][] treess = Tree.ValDef_EMPTY_ARRAY_ARRAY;
        while (true) {
            switch (type) {
            case PolyType(_, Type result):
                type = result;
                continue;
            case MethodType(Symbol[] vparams, Type result):
                ValDef[] trees = new ValDef[vparams.length];
                for (int i = 0; i < vparams.length; i++)
                    trees[i] = mkParam(vparams[i]);
                ValDef[][] array = new ValDef[treess.length + 1][];
                for (int i = 0; i < treess.length; i++) array[i] = treess[i];
                array[treess.length] = trees;
                treess = array;
                type = result;
                continue;
            default:
                return treess;
            }
        }
    }

    /** Builds the type parameter corresponding to given symbol. */
    public AbsTypeDef mkTypeParam(Symbol sym) {
	return AbsTypeDef(sym);
    }

    /** Builds the value parameter corresponding to given symbol. */
    public ValDef mkParam(Symbol sym) {
	return ValDef(sym, Tree.Empty);
    }

    /** Builds a definition for given interface with given body. */
    public Tree mkInterfaceDef(Symbol clazz, Tree[] body) {
	Global.instance.nextPhase();
        clazz.info(); // needed until isInterface() triggers flag updates
        assert clazz.isInterface(): Debug.show(clazz);
	Type[] parents = clazz.parents();
	Global.instance.prevPhase();
        Tree[] constrs = mkPrimaryConstrs(clazz.pos, parents);
        return ClassDef(clazz, constrs, Symbol.NONE, body);
    }

    /** Builds a ClassDef node for given class with given template. */
    public ClassDef ClassDef(Symbol clazz, Template template) {
        ClassDef tree = make.ClassDef(
            clazz.pos,
            clazz,
            mkTypeParamsOf(clazz),
            mkParamsOf(clazz),
            Tree.Empty,
            template);
        tree.setType(definitions.UNIT_TYPE);
        return tree;
    }

    /**
     * Builds a ClassDef node for given class with given parent
     * constructors, local symbol and body.
     */
    public ClassDef ClassDef(Symbol clazz, Tree[] constrs, Symbol local,
        Tree[] body)
    {
        return ClassDef(clazz, Template(clazz.pos, local, constrs, body));
    }

    /** Builds a ValDef node for given symbol and with given rhs. */
    public ValDef ValDef(Symbol sym, Tree rhs) {
	ValDef tree = make.ValDef(
            sym.pos,
            sym,
            TypeTerm(sym.pos, sym.nextType()),
            rhs);
        tree.setType(definitions.UNIT_TYPE);
        return tree;
    }

    /** Builds a DefDef node for given symbol with given body. */
    public DefDef DefDef(Symbol sym, Tree body) {
        DefDef tree = make.DefDef(
            sym.pos,
            sym,
            mkTypeParamsOf(sym),
            mkParamsOf(sym),
            TypeTerm(sym.pos, sym.nextType().resultType()),
            body);
        tree.setType(definitions.UNIT_TYPE);
        return tree;
    }

    /** Builds an AbsTypeDef node for given symbol. */
    public AbsTypeDef AbsTypeDef(Symbol sym) {
	AbsTypeDef tree = make.AbsTypeDef(
	    sym.pos,
            sym,
            TypeTerm(sym.pos, sym.nextInfo()),
            TypeTerm(sym.pos, sym.loBound()));
        tree.setType(definitions.UNIT_TYPE);
        return tree;
    }

    /** Builds an AliasTypeDef node for given symbol. */
    public AliasTypeDef AliasTypeDef(Symbol sym) {
	AliasTypeDef tree = make.AliasTypeDef(
	    sym.pos,
	    sym,
            mkTypeParamsOf(sym),
	    TypeTerm(sym.pos, sym.nextInfo()));
        tree.setType(definitions.UNIT_TYPE);
        return tree;
    }

    //########################################################################
    //########################################################################
    //########################################################################

    //########################################################################
    // !!! not yet reviewed

    /** Build the expansion of (() => expr)
     */
    public Tree mkUnitFunction(Tree expr, Type type, Symbol owner) {
	return mkFunction(expr.pos, Tree.ValDef_EMPTY_ARRAY, expr, type, owner);
    }

    /** Build the expansion of ((vparams_1, ..., vparams_n) => body)
     *  with result type `restype', where `owner' is the previous owner
     *  of `body'.
     *  This is:
     *    { class $anon() extends scala.Object with
     *                            scala.Function_N[T_1, ..., T_n, restype] {
     *        def apply(vparams_1, ..., vparams_n) = body1
     *      }
     *	    new $anon()
     *    }
     *  where
     *    vparams_i: T_i
     *    `body1' results from `body' by changing owner of all defined
     *    symbols in `body' from `owner' to the apply method.
     */
    public Tree mkFunction(int pos, ValDef[] vparams, Tree body, Type restype,
			   Symbol owner) {
	int n = vparams.length;
	Symbol[] params = new Symbol[n];
	Type[] argtypes = new Type[n];
	for (int i = 0; i < n; i++) {
	    params[i] = vparams[i].symbol();
	    argtypes[i] = params[i].type();
	}
        Type[] parentTypes = {
            definitions.OBJECT_TYPE,
            definitions.functionType(argtypes, restype) };
	ClassSymbol clazz = new ClassSymbol(
	    pos, Names.ANON_CLASS_NAME.toTypeName(), owner, 0);
        clazz.setInfo(Type.compoundType(parentTypes, new Scope(), clazz));
	clazz.allConstructors().setInfo(
	    Type.MethodType(Symbol.EMPTY_ARRAY, clazz.typeConstructor()));

	Symbol applyMeth = new TermSymbol(pos, Names.apply, clazz, FINAL)
	    .setInfo(Type.MethodType(params, restype));
	clazz.info().members().enter(applyMeth);

	for (int i = 0; i < params.length; i++) {
	    params[i].setOwner(applyMeth);
	}
	changeOwner(body, owner, applyMeth);
        Tree[] parentTrees = mkPrimaryConstrs(pos, parentTypes);
        Tree[] memberTrees = { DefDef(applyMeth, body) };
        Symbol local = TermSymbol.newLocalDummy(clazz);
        Tree classDef = ClassDef(clazz, parentTrees, local, memberTrees);
	Tree alloc = New(pos, mkPrimaryConstr(pos, clazz))
            .setType(parentTypes[1]); // !!!
	return Block(new Tree[]{classDef, alloc});
    }


    public Tree mkPartialFunction(int pos, Tree applyVisitor, Tree isDefinedAtVisitor,
				  Type pattype, Type restype, Symbol owner) {
	ClassSymbol clazz = new ClassSymbol(
	    pos, Names.ANON_CLASS_NAME.toTypeName(), owner, 0);
        Type[] parentTypes = {
            definitions.OBJECT_TYPE,
            definitions.partialFunctionType(pattype, restype)};
	clazz.setInfo(Type.compoundType(parentTypes, new Scope(), clazz));
	clazz.allConstructors().setInfo(
	    Type.MethodType(Symbol.EMPTY_ARRAY, clazz.typeConstructor()));
        Tree[] parentTrees = mkPrimaryConstrs(pos, parentTypes);
        Tree[] memberTrees = {
            makeVisitorMethod(pos, Names.apply, applyVisitor,
                              pattype, restype, clazz, owner),
            makeVisitorMethod(pos, Names.isDefinedAt, isDefinedAtVisitor,
                              pattype, definitions.BOOLEAN_TYPE, clazz, owner)};
        Symbol local = TermSymbol.newLocalDummy(clazz);
        Tree classDef = ClassDef(clazz, parentTrees, local, memberTrees);
	Tree alloc = New(pos, mkPrimaryConstr(pos, clazz))
	    .setType(parentTypes[1]); // !!!
	return Block(new Tree[]{classDef, alloc});
    }
    //where
	private Tree makeVisitorMethod(int pos, Name name, Tree visitor,
				       Type pattype, Type restype,
				       Symbol clazz, Symbol prevOwner) {
	    Symbol meth = new TermSymbol(pos, name, clazz, FINAL);
	    Symbol param = new TermSymbol(pos, Name.fromString("x$"), meth, PARAM)
		.setInfo(pattype);
	    meth.setInfo(Type.MethodType(new Symbol[]{param}, restype));
	    clazz.info().members().enter(meth);
	    changeOwner(visitor, prevOwner, meth);
	    Tree body =
		mkApply(
                    Select(Ident(pos, param), definitions.MATCH),
                    new Tree[]{mkType(pos, pattype), mkType(pos, restype)},
                    new Tree[]{visitor});
	    return DefDef(meth, body);
	}

    /** Change owner of all defined symbols from `prevOwner' to `newOwner'
     */
    public void changeOwner(Tree tree, final Symbol prevOwner, final Symbol newOwner) {
	Traverser lifter = new Traverser() {
	    public void traverse(Tree tree) {
		if (TreeInfo.isDefinition(tree)) {
		    Symbol sym = tree.symbol();
                    if (sym != null && sym.owner() == prevOwner) {
			sym.setOwner(newOwner);
                    }
		}
		super.traverse(tree);
	    }
	};
	lifter.traverse(tree);
    }

    /** Build a postfix function application
     */
    public Tree postfixApply(Tree obj, Tree fn, Symbol owner) {
	if (TreeInfo.isPureExpr(obj) || TreeInfo.isPureExpr(fn)) {
	    return Apply(Select(fn, definitions.FUNCTION_APPLY(1)), new Tree[]{obj});
	} else {
	    Name tmpname = global.freshNameCreator.newName("tmp", '$');
	    Symbol tmp = new TermSymbol(
		obj.pos, tmpname, owner, SYNTHETIC | FINAL)
		.setInfo(obj.type);
	    Tree tmpdef = ValDef(tmp, obj);
	    Tree expr = postfixApply(Ident(obj.pos, tmp), fn, owner);
	    return Block(new Tree[]{tmpdef, expr});
	}
    }

    // refactoring duplicate code of LambdaLift and CodeFactory

    public Tree Nil(int pos) {
	return mkRef(pos, global.definitions.getModule(Names.scala_Nil));
    }

    public Tree Cons(int pos, Type elemtpe, Tree hd, Tree tl) {
	return New(mkPrimaryConstr(pos,
				   global.definitions
				   .getClass( Names.scala_COLONCOLON ),
				   new Type[] { elemtpe },
				   new Tree[] { hd, tl }));

    }

}
