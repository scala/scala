/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.ast;

import scalac.Global;
import scalac.ast.Tree.*;
import scalac.atree.AConstant;
import scalac.symtab.*;
import scalac.util.*;

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

    /** Initializes this instance. */
    public TreeGen(Global global) {
	this(global, global.make);
    }

    /** Initializes this instance. */
    public TreeGen(Global global, TreeFactory make) {
        this.global = global;
	this.definitions = global.definitions;
        this.make = make;
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

    /** Builds a unit literal. */
    public Tree mkUnitLit(int pos) {
        return Literal(pos, AConstant.UNIT);
    }

    /** Builds a boolean literal. */
    public Tree mkBooleanLit(int pos, boolean value) {
        return Literal(pos, AConstant.BOOLEAN(value));
    }

    /** Builds a byte literal. */
    public Tree mkByteLit(int pos, byte value) {
        return Literal(pos, AConstant.BYTE(value));
    }

    /** Builds a short literal. */
    public Tree mkShortLit(int pos, short value) {
        return Literal(pos, AConstant.SHORT(value));
    }

    /** Builds a character literal. */
    public Tree mkCharLit(int pos, char value) {
        return Literal(pos, AConstant.CHAR(value));
    }

    /** Builds an integer literal */
    public Tree mkIntLit(int pos, int value) {
        return Literal(pos, AConstant.INT(value));
    }

    /** Builds a long literal. */
    public Tree mkLongLit(int pos, long value) {
        return Literal(pos, AConstant.LONG(value));
    }

    /** Builds a float literal. */
    public Tree mkFloatLit(int pos, float value) {
        return Literal(pos, AConstant.FLOAT(value));
    }

    /** Builds a double literal. */
    public Tree mkDoubleLit(int pos, double value) {
        return Literal(pos, AConstant.DOUBLE(value));
    }

    /** Builds a string literal. */
    public Tree mkStringLit(int pos, String value) {
        return Literal(pos, AConstant.STRING(value));
    }

    /** Builds a null literal. */
    public Tree mkNullLit(int pos) {
        return Literal(pos, AConstant.NULL);
    }

    /** Builds a zero literal. */
    public Tree mkZeroLit(int pos) {
        return Literal(pos, AConstant.ZERO);
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
	if (definitions.ALLREF_TYPE().isSubType(type)) return mkNullLit(pos);
        switch (type.unbox()) {
        case UnboxedType(int tag): return mkDefaultValue(pos, tag);
        }
        return mkZeroLit(pos);
    }

    /** Builds a Literal node of given value. */
    public Literal Literal(int pos, AConstant value) {
        Literal tree = make.Literal(pos, value);
        global.nextPhase();
        tree.setType(definitions.atyper.type(value));
        global.prevPhase();
        return tree;
    }

    //########################################################################
    // Public Methods - Building references

    /**
     * Builds a reference to primary constructor of given class with
     * given qualifier.
     */
    public Tree mkPrimaryConstructorRef(int pos, Tree qualifier, Symbol clasz){
        return mkRef(pos, qualifier, primaryConstructorOf(clasz));
    }
    public Tree mkPrimaryConstructorRef(Tree qualifier, Symbol clasz) {
        return mkPrimaryConstructorRef(qualifier.pos, qualifier, clasz);
    }

    /**
     * Builds a reference to primary constructor of given class with
     * given stable prefix.
     */
    public Tree mkPrimaryConstructorRef(int pos, Type stable, Symbol clasz) {
        return mkRef(pos, stable, primaryConstructorOf(clasz));
    }

    /**
     * Builds a local reference to primary constructor of given class.
     */
    public Tree mkPrimaryConstructorLocalRef(int pos, Symbol clasz) {
        return mkLocalRef(pos, primaryConstructorOf(clasz));
    }

    /**
     * Builds a global reference to primary constructor of given
     * class.
     */
    public Tree mkPrimaryConstructorGlobalRef(int pos, Symbol clasz) {
        return mkGlobalRef(pos, primaryConstructorOf(clasz));
    }

    /** Builds local references to given symbols. */
    public Tree[] mkLocalRefs(int pos, Symbol[] symbols) {
        if (symbols.length == 0) return Tree.EMPTY_ARRAY;
        Tree[] trees = new Tree[symbols.length];
        for (int i = 0; i < trees.length; i++)
            trees[i] = mkLocalRef(pos, symbols[i]);
	return trees;
    }

    /** Builds global references to given symbols. */
    public Tree[] mkGlobalRefs(int pos, Symbol[] symbols) {
        if (symbols.length == 0) return Tree.EMPTY_ARRAY;
        Tree[] trees = new Tree[symbols.length];
        for (int i = 0; i < trees.length; i++)
            trees[i] = mkGlobalRef(pos, symbols[i]);
	return trees;
    }

    /** Builds a reference to given symbol with given qualifier. */
    public Tree mkRef(int pos, Tree qualifier, Symbol symbol) {
        return Select(pos, qualifier, symbol);
    }
    public Tree mkRef(Tree qualifier, Symbol symbol) {
        return mkRef(qualifier.pos, qualifier, symbol);
    }

    /**
     * Builds a reference to given symbol with given stable prefix.
     */
    public Tree mkRef(int pos, Type stable, Symbol symbol) {
        switch (stable) {
	case NoPrefix:
            return Ident(pos, symbol);
	case ThisType(Symbol clasz):
            return mkRef(pos, This(pos, clasz), symbol);
        case SingleType(Type prefix, Symbol member):
	    Tree qualifier = mkRef(pos, prefix, member);
	    switch (qualifier.type()) {
	    case MethodType(Symbol[] params, _):
		assert params.length == 0: qualifier.type();
		qualifier = Apply(pos, qualifier);
	    }
	    return mkRef(pos, qualifier, symbol);
        default:
            throw Debug.abort("illegal case", stable);
        }
    }

    /** Builds a local reference to given symbol. */
    public Tree mkLocalRef(int pos, Symbol symbol) {
        assert symbol.isTerm(): Debug.show(symbol);
        return mkRef(pos, symbol.owner().thisType(), symbol);
    }

    /** Builds a global reference to given symbol. */
    public Tree mkGlobalRef(int pos, Symbol symbol) {
        assert symbol.isTerm(): Debug.show(symbol);
        global.nextPhase();
        Type prefix = symbol.owner().staticPrefix();
        global.prevPhase();
        return mkRef(pos, prefix, symbol);
    }

    /** Builds a This node corresponding to given class. */
    public This This(int pos, Symbol clazz) {
        assert clazz.isClassType(): Debug.show(clazz);
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
        Type type;
        if (sym.isInitializer()) {
            type = sym.type();
            Symbol[] tparams = sym.owner().typeParams();
            if (tparams.length != 0) type = Type.PolyType(tparams, type);
        } else {
            type = sym.owner().thisType().memberStabilizedType(sym);
        }
        tree.setType(type);
        global.prevPhase();
        return tree;
    }

    /**
     * Builds a Select node corresponding to given symbol selected
     * from given qualifier.
     */
    public Select Select(int pos, Tree qualifier, Symbol sym) {
	assert sym.isTerm(): Debug.show(sym);
	sym.flags |= ACCESSED | SELECTOR;
	Select tree = make.Select(pos, sym, qualifier);
        global.nextPhase();
        tree.setType(qualifier.type.memberStabilizedType(sym));
        global.prevPhase();
        return tree;
    }
    public Select Select(Tree qualifier, Symbol sym) {
	return Select(qualifier.pos, qualifier, sym);
    }

    //########################################################################
    // Public Methods - Building applications

    /**
     * Builds an application with given function, type arguments and
     * value arguments.
     */
    public Tree mkApplyTV(int pos, Tree fn, Type[] targs, Tree[] vargs) {
        if (targs.length != 0) fn = TypeApply(pos, fn, mkTypes(pos, targs));
        return Apply(pos, fn, vargs);
    }
    public Tree mkApplyTV(Tree fn, Type[] targs, Tree[] vargs) {
        return mkApplyTV(fn.pos, fn, targs, vargs);
    }
    public Tree mkApplyTV(int pos, Tree fn, Tree[] targs, Tree[] vargs) {
        if (targs.length != 0) fn = TypeApply(pos, fn, targs);
        return Apply(pos, fn, vargs);
    }
    public Tree mkApplyTV(Tree fn, Tree[] targs, Tree[] vargs) {
        return mkApplyTV(fn.pos, fn, targs, vargs);
    }

    /**
     * Builds an application with given function and type arguments
     * and with no value arguments.
     */
    public Tree mkApplyT_(int pos, Tree fn, Type[] targs) {
        return mkApplyTV(pos, fn, targs, Tree.EMPTY_ARRAY);
    }
    public Tree mkApplyT_(Tree fn, Type[] targs) {
        return mkApplyT_(fn.pos, fn, targs);
    }
    public Tree mkApplyT_(int pos, Tree fn, Tree[] targs) {
        return mkApplyTV(pos, fn, targs, Tree.EMPTY_ARRAY);
    }
    public Tree mkApplyT_(Tree fn, Tree[] targs) {
        return mkApplyT_(fn.pos, fn, targs);
    }

    /**
     * Builds an application with given function, no type arguments
     * and given value arguments.
     */
    public Tree mkApply_V(int pos, Tree fn, Tree[] vargs) {
        return mkApplyTV(pos, fn, Tree.EMPTY_ARRAY, vargs);
    }
    public Tree mkApply_V(Tree fn, Tree[] vargs) {
        return mkApply_V(fn.pos, fn, vargs);
    }

    /**
     * Builds an application with given function and no type arguments
     * and no value arguments.
     */
    public Tree mkApply__(int pos, Tree fn) {
        return mkApplyTV(pos, fn, Tree.EMPTY_ARRAY, Tree.EMPTY_ARRAY);
    }
    public Tree mkApply__(Tree fn) {
        return mkApply__(fn.pos, fn);
    }

    /** Builds a TypeApply node with given function and arguments. */
    public TypeApply TypeApply(int pos, Tree fn, Type[] targs) {
        return TypeApply(pos, fn, mkTypes(pos, targs));
    }
    public TypeApply TypeApply(Tree fn, Type[] targs) {
        return TypeApply(fn.pos, fn, targs);
    }
    public TypeApply TypeApply(int pos, Tree fn, Tree[] targs) {
        switch (fn.type()) {
        case Type.OverloadedType(_, _):
            // TreeGen only builds trees, names must already be resolved
            throw Debug.abort("unresolved name", fn + " - " + fn.type);
        case Type.PolyType(Symbol[] tparams, Type restpe):
            global.nextPhase();
            restpe = restpe.subst(tparams, Tree.typeOf(targs));
            global.prevPhase();
            return (TypeApply)make.TypeApply(pos, fn, targs).setType(restpe);
        default:
            throw Debug.abort("illegal case", fn + " - " + fn.type());
        }
    }
    public TypeApply TypeApply(Tree fn, Tree[] targs) {
      return TypeApply(fn.pos, fn, targs);
    }

    /** Builds an Apply node with given function and arguments. */
    public Apply Apply(int pos, Tree fn, Tree[] vargs) {
        switch (fn.type()) {
        case Type.OverloadedType(_, _):
            // TreeGen only builds trees, names must already be resolved
            throw Debug.abort("unresolved name", fn + " - " + fn.type());
        case Type.MethodType(Symbol[] vparams, Type restpe):
            return (Apply)make.Apply(pos, fn, vargs).setType(restpe);
        default:
            throw Debug.abort("illegal case", fn + " - " + fn.type());
        }
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
    // Public Methods - Building expressions - Simple nodes

    /** Flattens the given tree array by inlining Block nodes. */
    public Tree[] flatten_(Tree[] trees) {
        boolean copy = false;
        int length = 0;
        for (int i = 0; i < trees.length; i++) {
            switch (trees[i]) {
            case Empty:
                copy = true;
                length -= 1;
                continue;
            case Block(Tree[] stats, Tree value):
                copy = true;
                length += stats.length + 1;
                continue;
            }
            length += 1;
        }
        if (!copy) return trees;
        Tree[] clone = new Tree[length];
        for (int i = 0, o = 0; i < trees.length; i++) {
            switch (trees[i]) {
            case Empty:
                continue;
            case Block(Tree[] stats, Tree value):
                for (int j = 0; j < stats.length; j++) clone[o++] = stats[j];
                clone[o++] = value;
                continue;
            }
            clone[o++] = trees[i];
        }
        return clone;
    }

    /** Builds an import of all names of given qualifier. */
    public Import mkImportAll(int pos, Tree qualifier) {
        return Import(pos, qualifier, new Name[]{Names.IMPORT_WILDCARD});
    }
    public Import mkImportAll(Tree qualifier) {
        return mkImportAll(qualifier.pos, qualifier);
    }
    public Import mkImportAll(int pos, Symbol qualifier) {
        return mkImportAll(pos, mkGlobalRef(pos, qualifier));
    }

    /** Builds an instance test with given value and type. */
    public Tree mkIsInstanceOf(int pos, Tree value, Type type) {
        Type[] targs = new Type[]{type};
        return mkApplyT_(pos, Select(value, definitions.ANY_IS), targs);
    }
    public Tree mkIsInstanceOf(Tree value, Type type) {
        return mkIsInstanceOf(value.pos, value, type);
    }

    /** Builds a cast with given value and type. */
    public Tree mkAsInstanceOf(int pos, Tree value, Type type) {
        Type[] targs = new Type[]{type};
        return mkApplyT_(pos, Select(value, definitions.ANY_AS), targs);
    }
    public Tree mkAsInstanceOf(Tree value, Type type) {
        return mkAsInstanceOf(value.pos, value, type);
    }

    /** Builds an expression of type Unit with given statements. */
    public Tree mkUnitBlock(int pos, Tree[] stats) {
        return mkBlock(pos, stats, mkUnitLit(pos));
    }

    /** Builds an expression of type Unit with given statement. */
    public Tree mkUnitBlock(int pos, Tree stat) {
        return mkUnitBlock(pos, new Tree[]{stat});
    }
    public Tree mkUnitBlock(Tree stat) {
        return mkUnitBlock(stat.pos, stat);
    }

    /** Builds an expression with given statements and value. */
    public Tree mkBlock(int pos, Tree[] stats, Tree value) {
        if (stats.length == 0) return value;
        return Block(pos, stats, value); // !!! add flatten?
    }
    public Tree mkBlock(Tree[] stats, Tree value) {
        return mkBlock((stats.length!=0 ? stats[0] : value).pos, stats, value);
    }

    /** Builds an expression with given statement and value. */
    public Tree mkBlock(int pos, Tree stat, Tree value) {
        switch (stat) {
        case Empty:
            return value;
        case Block(Tree[] block_stats, Tree block_value):
            Tree[] stats = Tree.cloneArray(block_stats, 1);
            stats[block_stats.length] = block_value;
            return Block(stat.pos, stats, value);
        default:
            return Block(pos, new Tree[]{stat}, value);
        }
    }
    public Tree mkBlock(Tree stat, Tree value) {
        return mkBlock(stat.pos, stat, value);
    }

    /** Builds an Import node with given qualifier and names. */
    public Import Import(int pos, Tree qualifier, Name[] names) {
        Import tree = make.Import(pos, qualifier.symbol(), qualifier, names);
        tree.setType(Type.NoType);
        return tree;
    }
    public Import Import(Tree qualifier, Name[] names) {
        return Import(qualifier.pos, qualifier, names);
    }
    public Import Import(int pos, Symbol qualifier, Name[] names) {
        return Import(pos, mkGlobalRef(pos, qualifier), names);
    }

    /** Builds a Template node with given symbol, parents and body. */
    public Template Template(int pos, Symbol local, Tree[]parents, Tree[]body){
        Template tree = make.Template(pos, local, parents, body);
        tree.setType(Type.NoType);
        return tree;
    }

    /** Builds a Block node with given statements and value. */
    public Block Block(int pos, Tree[] stats, Tree value) {
        inline:
        switch (value) {
        case Block(Tree[] value_stats, Tree value_value):
            int count = 0;
            for (int i = 0; i < value_stats.length; i++) {
                if (value_stats[i].definesSymbol()) break inline;
                if (value_stats[i] != Tree.Empty) count++;
            }
            Tree[] array = Tree.cloneArray(stats, count);
            for (int i = 0, j = stats.length; i < value_stats.length; i++) {
                if (value_stats[i] != Tree.Empty) array[j++] = value_stats[i];
            }
            stats = array;
            value = value_value;
        }
        Block tree = make.Block(pos, stats, value);
        tree.setType(value.type());
	return tree;
    }
    public Block Block(Tree[] stats, Tree value) {
        return Block((stats.length != 0 ? stats[0] : value).pos, stats, value);
    }

    /** Builds an Assign node corresponding to "<lhs> = <rhs>". */
    public Assign Assign(int pos, Tree lhs, Tree rhs) {
        Assign tree = make.Assign(pos, lhs, rhs);
        global.nextPhase();
        tree.setType(definitions.UNIT_TYPE());
        global.prevPhase();
        return tree;
    }
    public Assign Assign(Tree lhs, Tree rhs) {
        return Assign(lhs.pos, lhs, rhs);
    }

    /** Builds an If node with given components and type. */
    public If If(int pos, Tree cond, Tree thenpart, Tree elsepart, Type type) {
        assert assertTreeSubTypeOf(thenpart, type);
        assert assertTreeSubTypeOf(elsepart, type);
	If tree = make.If(pos, cond, thenpart, elsepart);
        tree.setType(type);
        return tree;
    }
    public If If(Tree cond, Tree thenpart, Tree elsepart, Type type) {
	return If(cond.pos, cond, thenpart, elsepart, type);
    }

    /** Builds an If node with given condition and branches. */
    public If If(int pos, Tree cond, Tree thenpart, Tree elsepart) {
        global.nextPhase();
        Type type = thenpart.getType().isSameAs(elsepart.getType())
            ? thenpart.type
            : Type.lub(new Type[] {thenpart.getType(), elsepart.getType()});
        global.prevPhase();
        return If(pos, cond, thenpart, elsepart, type);
    }
    public If If(Tree cond, Tree thenpart, Tree elsepart) {
	return If(cond.pos, cond, thenpart, elsepart);
    }

    /** Builds a Switch node with given components and type.
     *  @param tags a <b>sorted</b> array of tags
     */
    public Switch Switch(int pos, Tree test, int[] tags, Tree[] bodies,
        Tree otherwise, Type type)
    {
        for (int i = 0; i < bodies.length; i++) {
            assert assertTreeSubTypeOf(bodies[i], type);
            assert (i==0) || ( tags[i-1] < tags[i] ) : "expecting sorted tags";
        }
        assert assertTreeSubTypeOf(otherwise, type);
        Switch tree = make.Switch(pos, test, tags, bodies, otherwise);
        tree.setType(type);
        return tree;
    }
    public Switch Switch(Tree test, int[] tags, Tree[] bodies, Tree otherwise,
        Type type)
    {
        return Switch(test.pos, test, tags, bodies, otherwise, type);
    }

    /** Builds a Switch node with given components. */
    public Switch Switch(int pos, Tree test, int[] tags, Tree[] bodies,
        Tree otherwise)
    {
        Type[] types = new Type[bodies.length + 1];
        for (int i = 0; i < bodies.length; i++) types[i] = bodies[i].getType();
        types[bodies.length] = otherwise.getType();
        global.nextPhase();
        Type type = Type.lub(types);
        global.prevPhase();
        return Switch(pos, test, tags, bodies, otherwise, type);
    }
    public Switch Switch(Tree test, int[] tags, Tree[] bodies, Tree otherwise){
        return Switch(test.pos, test, tags, bodies, otherwise);
    }

    /** Builds a Return node of given method with given value. */
    public Return Return(int pos, Symbol method, Tree value) {
        Return tree = make.Return(pos, method, value);
        tree.setType(definitions.ALL_TYPE());
        return tree;
    }
    public Return Return(Symbol method, Tree value) {
        return Return(value.pos, method, value);
    }

    /** Builds a New node corresponding to "new <constr>". */
    public Tree New(int pos, Tree constr) {
        Tree[] constrs = { constr };
	Template templ = Template(pos, Symbol.NONE, constrs, Tree.EMPTY_ARRAY);
	New tree = make.New(pos, templ);
        tree.setType(constr.type);
        // after AddConstructor use type of symbol
        switch (constr) {
        case Apply(TypeApply(Tree fun, Tree[] targs), _):
            Symbol sym = fun.symbol();
            if (sym == null || sym.isConstructor()) break;
            Type[] args = Tree.typeOf(targs);
            tree.setType(Type.appliedType(sym.owner().nextType(), args));
            break;
        case Apply(Tree fun, _):
            Symbol sym = fun.symbol();
            if (sym == null || sym.isConstructor()) break;
            tree.setType(sym.owner().nextType());
            break;
        }
        return tree;
    }
    public Tree New(Tree constr) {
        return New(constr.pos, constr);
    }

    /** Builds a Typed nodes with given value and type. */
    public Typed Typed(int pos, Tree value, Tree type) {
        Typed tree = make.Typed(pos, value, type);
        tree.setType(type.type());
        return tree;
    }
    public Typed Typed(Tree value, Tree type) {
        return Typed(value.pos, value, type);
    }
    public Typed Typed(int pos, Tree value, Type type) {
        return Typed(pos, value, mkType(pos, type));
    }
    public Typed Typed(Tree value, Type type) {
        return Typed(value.pos, value, type);
    }

    //########################################################################
    // Public Methods - Building expressions - Lists

    /** Builds an empty list. */
    public Tree mkNil(int pos) {
	return mkGlobalRef(pos, definitions.NIL);
    }

    /** Builds a list with given element type, head and tail. */
    public Tree mkNewCons(int pos, Type element, Tree head, Tree tail) {
        // !!! these checks can be removed once they are done in Apply
        global.nextPhase();
        assert head.type().isSubType(element):
            element + " -- " + head + " : " + head.type;
        assert tail.type().isSubType(definitions.LIST_TYPE(element)):
            element + " -- " + tail + " : " + tail.type;
        global.prevPhase();
	return New(
            mkApplyTV(
                mkPrimaryConstructorGlobalRef(pos, definitions.CONS_CLASS),
                new Type[]{element},
                new Tree[]{head, tail}));
    }
    public Tree mkNewCons(Type element, Tree head, Tree tail) {
        return mkNewCons(head.pos, element, head, tail);
    }

    /** Builds a list with given element type and values. */
    public Tree mkNewList(int pos, Type element, Tree[] values) {
        Tree list = mkNil(pos);
        for (int i = values.length - 1; 0 <= i; i--)
            list = mkNewCons(pos, element, values[i], list);
        return list;
    }

    //########################################################################
    // Public Methods - Building expressions - Arrays

    /** Builds a new array tree with given element type and length. */
    public Tree mkNewArray(int pos, Type element, Tree length) {
        return New(
            mkApplyTV(
                mkPrimaryConstructorGlobalRef(pos, definitions.ARRAY_CLASS),
                new Type[]{element},
                new Tree[]{length}));
    }
    public Tree mkNewArray(Type element, Tree length) {
        return mkNewArray(length.pos, element, length);
    }
    public Tree mkNewArray(int pos, Type element, int length) {
        return mkNewArray(pos, element, mkIntLit(pos, length));
    }

    /**
     * Builds a new array tree with given element type and values. The
     * owner must be the owner of the created code. It is needed to
     * create a local variable.
     */
    public Tree mkNewArray(int pos, Type element, Tree[] values, Symbol owner){
        if (values.length == 0) return mkNewArray(pos, element, 0);
        Tree[] trees = new Tree[1 + values.length];
        Symbol array =
            newLocal(pos, Names.array, owner, definitions.ARRAY_TYPE(element));
        trees[0] = ValDef(array, mkNewArray(pos, element, values.length));
        for (int i = 0; i < values.length; i++)
            trees[1 + i] = mkArraySet(Ident(pos, array), i, values[i]);
        return Block(pos, trees, Ident(pos, array));
    }

    /** Builds an array length operation. */
    public Tree mkArrayLength(int pos, Tree array) {
        Tree function = Select(pos, array, definitions.ARRAY_LENGTH());
        return Apply(pos, function);
    }
    public Tree mkArrayLength(Tree array) {
        return mkArrayLength(array.pos, array);
    }

    /** Builds an array get operation. */
    public Tree mkArrayGet(int pos, Tree array, Tree index) {
        Tree function = Select(pos, array, definitions.ARRAY_GET());
        return Apply(pos, function, new Tree[] {index});
    }
    public Tree mkArrayGet(Tree array, Tree index) {
        return mkArrayGet(array.pos, array, index);
    }
    public Tree mkArrayGet(int pos, Tree array, int index) {
        return mkArrayGet(pos, array, mkIntLit(pos, index));
    }
    public Tree mkArrayGet(Tree array, int index) {
        return mkArrayGet(array.pos, array, index);
    }

    /** Builds an array set operation. */
    public Tree mkArraySet(int pos, Tree array, Tree index, Tree value) {
        Tree function = Select(pos, array, definitions.ARRAY_SET());
        return Apply(pos, function, new Tree[] {index, value});
    }
    public Tree mkArraySet(Tree array, Tree index, Tree value) {
        return mkArraySet(array.pos, array, index, value);
    }
    public Tree mkArraySet(int pos, Tree array, int index, Tree value) {
        return mkArraySet(pos, array, mkIntLit(pos, index), value);
    }
    public Tree mkArraySet(Tree array, int index, Tree value) {
        return mkArraySet(array.pos, array, index, value);
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

    /** Builds a PackageDef with given package and template. */
    public PackageDef PackageDef(Tree peckage, Template template) {
        PackageDef tree = make.PackageDef(peckage.pos, peckage, template);
        tree.setType(Type.NoType);
        return tree;
    }
    public PackageDef PackageDef(Symbol peckage, Template template) {
        return PackageDef(mkGlobalRef(peckage.pos, peckage), template);
    }

    /** Builds a PackageDef with given package and body. */
    public PackageDef PackageDef(Tree peckage, Tree[] body) {
        return PackageDef(
            peckage,
            Template(peckage.pos, Symbol.NONE, Tree.EMPTY_ARRAY, body));
    }
    public PackageDef PackageDef(Symbol peckage, Tree[] body) {
        return PackageDef(mkGlobalRef(peckage.pos, peckage), body);
    }

    /** Builds a ClassDef node for given class with given template. */
    public ClassDef ClassDef(Symbol clazz, Template template) {
        Tree type = Tree.Empty;
        if (clazz.thisSym() != clazz) {
            global.nextPhase();
            type = mkType(clazz.pos, clazz.typeOfThis());
            global.prevPhase();
        }
        ClassDef tree = make.ClassDef(
            clazz.pos,
            clazz,
            mkTypeParamsOf(clazz),
            mkParamsOf(clazz),
            type,
            template);
        tree.setType(Type.NoType);
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

    /** Builds a ClassDef node for given class with given body. */
    public Tree ClassDef(Symbol clazz, Tree[] body) {
        for (int i = 0; i < body.length; i++) {
            assert body[i] == Tree.Empty
                || body[i].definesSymbol() && body[i].symbol().owner() ==clazz:
                "\nclass : " + Debug.show(clazz) +
                "\nmember: " + Debug.show(body[i].symbol()) +
                "\nmember: " + body[i];
        }
	Global.instance.nextPhase();
	Type[] parents = clazz.parents();
	Global.instance.prevPhase();
        Tree[] constrs = new Tree[parents.length];
        for (int i = 0; i < constrs.length; i++) {
            switch (parents[i]) {
            case TypeRef(Type prefix, Symbol parent, Type[] targs):
                constrs[i] = mkApplyT_(
                    mkPrimaryConstructorRef(clazz.pos, prefix, parent),
                    targs);
                continue;
            default:
                throw Debug.abort("invalid type", parents[i]);
            }
        }
        return ClassDef(clazz, constrs, Symbol.NONE, body);
    }

    /** Builds a ValDef node for given symbol and with given rhs. */
    public ValDef ValDef(Symbol sym, Tree rhs) {
	ValDef tree = make.ValDef(
            sym.pos,
            sym,
            TypeTerm(sym.pos, sym.nextType()),
            rhs);
        tree.setType(Type.NoType);
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
        tree.setType(Type.NoType);
        return tree;
    }

    /** Builds an AbsTypeDef node for given symbol. */
    public AbsTypeDef AbsTypeDef(Symbol sym) {
	AbsTypeDef tree = make.AbsTypeDef(
	    sym.pos,
            sym,
            TypeTerm(sym.pos, sym.nextInfo()),
            TypeTerm(sym.pos, sym.loBound()));
        tree.setType(Type.NoType);
        return tree;
    }

    /** Builds an AliasTypeDef node for given symbol. */
    public AliasTypeDef AliasTypeDef(Symbol sym) {
	AliasTypeDef tree = make.AliasTypeDef(
	    sym.pos,
	    sym,
            mkTypeParamsOf(sym),
	    TypeTerm(sym.pos, sym.nextInfo()));
        tree.setType(Type.NoType);
        return tree;
    }

    /** Builds an CaseDef node with given pattern, guard and body. */
    public CaseDef CaseDef(Tree pattern, Tree guard, Tree body) {
        CaseDef tree = make.CaseDef(pattern.pos, pattern, guard, body);
        tree.setType(body.getType());
        return tree;
    }

    /** Builds an CaseDef node with given pattern and body. */
    public CaseDef CaseDef(Tree pattern, Tree body) {
        return CaseDef(pattern, Tree.Empty, body);
    }

    /**
     * Builds a LabelDef node for given label with given parameters
     * and body.
     */
    public LabelDef LabelDef(Symbol label, Ident[] params, Tree body) {
        LabelDef tree = make.LabelDef(label.pos, label, params, body);
        tree.setType(label.nextType().resultType());
        return tree;
    }
    public LabelDef LabelDef(Symbol label, Symbol[] params, Tree body) {
        Ident[] idents = new Ident[params.length];
        for (int i = 0; i < params.length; i++)
            idents[i] = Ident(params[i].pos, params[i]);
        return LabelDef(label, idents, body);
    }

    //########################################################################
    // Private Methods

    /** Asserts type of given tree is a subtype of given type. */
    private boolean assertTreeSubTypeOf(Tree tree, Type expected) {
        global.nextPhase();
        assert tree.getType().isSubType(expected):
            "\ntree    : " + tree +
            "\ntype    : " + tree.getType() +
            "\nexpected: " + expected;
        global.prevPhase();
        return true;
    }

    /** Creates a local variable with given prefix, owner and type. */
    private Symbol newLocal(int pos, Name prefix, Symbol owner, Type type) {
        Name name = global.freshNameCreator.newName(prefix);
        Symbol local = new TermSymbol(pos, name, owner, Modifiers.SYNTHETIC);
        global.nextPhase();
        local.setType(type);
        global.prevPhase();
        return local;
    }

    /** Returns the primary constructor of given class. */
    private Symbol primaryConstructorOf(Symbol clasz) {
        global.nextPhase();
        Symbol constr = clasz.primaryConstructor();
        global.prevPhase();
        return constr;
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
            definitions.ANYREF_TYPE(),
            definitions.FUNCTION_TYPE(argtypes, restype) };
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
        Tree[] memberTrees = { DefDef(applyMeth, body) };
        Tree classDef = ClassDef(clazz, memberTrees);
	Tree alloc = New(mkApply__(mkPrimaryConstructorLocalRef(pos, clazz)))
            .setType(parentTypes[1]); // !!!
	return mkBlock(classDef, alloc);
    }


    public Tree mkPartialFunction(int pos, Tree applyVisitor, Tree isDefinedAtVisitor,
				  Type pattype, Type restype, Symbol owner) {
	ClassSymbol clazz = new ClassSymbol(
	    pos, Names.ANON_CLASS_NAME.toTypeName(), owner, 0);
        Type[] parentTypes = {
            definitions.ANYREF_TYPE(),
            definitions.PARTIALFUNCTION_TYPE(pattype, restype)};
	clazz.setInfo(Type.compoundType(parentTypes, new Scope(), clazz));
	clazz.allConstructors().setInfo(
	    Type.MethodType(Symbol.EMPTY_ARRAY, clazz.typeConstructor()));
        Tree[] memberTrees = {
            makeVisitorMethod(pos, Names.apply, applyVisitor,
                              pattype, restype, clazz, owner),
            makeVisitorMethod(pos, Names.isDefinedAt, isDefinedAtVisitor,
                              pattype, definitions.BOOLEAN_TYPE(), clazz, owner)};
        Tree classDef = ClassDef(clazz, memberTrees);
	Tree alloc = New(mkApply__(mkPrimaryConstructorLocalRef(pos, clazz)))
	    .setType(parentTypes[1]); // !!!
	return mkBlock(classDef, alloc);
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
		mkApplyTV(
                    Select(Ident(pos, param), definitions.ANY_MATCH),
                    new Tree[]{mkType(pos, pattype), mkType(pos, restype)},
                    new Tree[]{visitor});
	    return DefDef(meth, body);
	}

    /** Change owner of all defined symbols from `prevOwner' to `newOwner'
     */
    public void changeOwner(Tree tree, final Symbol prevOwner, final Symbol newOwner) {
	Traverser lifter = new Traverser() {
	    public void traverse(Tree tree) {
		if (tree.definesSymbol()) {
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
	    return mkBlock(tmpdef, expr);
	}
    }

    // for insert debug printing code

    public Tree Console_print(int pos, String str) {
        return Console_print( pos, mkStringLit( pos, str ));
    }

    public Tree Console_print(int pos, Tree arg) {
        Symbol sym = global.definitions.getModule( Names.scala_Console );
        return Apply( Select( pos,
                              mkGlobalRef( pos, sym),
                              global.definitions.CONSOLE_PRINT()),
                      new Tree[] {
                          arg
                      });
    }

}
