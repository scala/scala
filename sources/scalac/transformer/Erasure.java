/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: Erasure.java,v 1.48 2003/01/16 14:21:19 schinz Exp $
// $Id$

package scalac.transformer;

import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;

import scalac.Global;
import scalac.Phase;
import scalac.Unit;
import scalac.ast.Tree;
import scalac.ast.Tree.Ident;
import scalac.ast.Tree.Template;
import scalac.ast.Tree.AbsTypeDef;
import scalac.ast.Tree.AliasTypeDef;
import scalac.ast.Tree.ValDef;
import scalac.ast.TreeList;
import scalac.ast.GenTransformer;
import scalac.atree.AConstant;
import scalac.symtab.Definitions;
import scalac.symtab.Kinds;
import scalac.symtab.Type;
import scalac.symtab.TypeTags;
import scalac.symtab.Modifiers;
import scalac.symtab.Scope;
import scalac.symtab.Scope.SymbolIterator;
import scalac.symtab.SymSet;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolTablePrinter;
import scalac.backend.Primitive;
import scalac.backend.Primitives;
import scalac.util.Name;
import scalac.util.Names;
import scalac.util.Debug;

/** A transformer for type erasure and bridge building
 *
 *  @author     Martin Odersky
 *  @version    1.0
 *
 *  What it does:
 *  (1) Map every type to its erasure.
 *  (2) If method A overrides a method B, and the erased type ETA of A is
 *      different from the erased type ETB of B seen as a member of A's class,
 *      add a bridge method with the same name as A,B, with signature ETB
 *      which calls A after casting parameters.
 */
public class Erasure extends GenTransformer implements Modifiers {

    //########################################################################
    // Private Constants

    /** The unboxed types */
    private static final Type
        UNBOXED_UNIT    = Type.unboxedType(TypeTags.UNIT),
        UNBOXED_BOOLEAN = Type.unboxedType(TypeTags.BOOLEAN),
        UNBOXED_BYTE    = Type.unboxedType(TypeTags.BYTE),
        UNBOXED_SHORT   = Type.unboxedType(TypeTags.SHORT),
        UNBOXED_CHAR    = Type.unboxedType(TypeTags.CHAR),
        UNBOXED_INT     = Type.unboxedType(TypeTags.INT),
        UNBOXED_LONG    = Type.unboxedType(TypeTags.LONG),
        UNBOXED_FLOAT   = Type.unboxedType(TypeTags.FLOAT),
        UNBOXED_DOUBLE  = Type.unboxedType(TypeTags.DOUBLE);

    //########################################################################
    // Private Fields

    /** The global definitions */
    private final Definitions definitions;

    /** The global primitives */
    private final Primitives primitives;

    /** The current unit */
    private Unit unit;

    private final boolean forMSIL;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public Erasure(Global global) {
        super(global);
	this.definitions = global.definitions;
        this.primitives = global.primitives;
        this.forMSIL = global.target == global.TARGET_MSIL;
    }

    //########################################################################
    // Public Methods

    /** Transforms the given unit. */
    public void apply(Unit unit) {
	this.unit = unit;
	super.apply(unit);
    }

    /** Transforms the given tree. */
    public Tree transform(Tree tree) {
        switch (tree) {

	case ClassDef(_, _, _, _, _, Template(_, Tree[] body)):
            Symbol clasz = tree.symbol();
            TreeList members = new TreeList(transform(body));
            checkOverloadedTermsOf(clasz);
            addBridges(clasz, members);
            return gen.ClassDef(clasz, members.toArray());

	case ValDef(_, _, _, Tree rhs):
            Symbol field = tree.symbol();
	    if (rhs != Tree.Empty) rhs = transform(rhs, field.nextType());
	    return gen.ValDef(field, rhs);

	case DefDef(_, _, _, _, _, Tree rhs):
            Symbol method = tree.symbol();
            if (rhs != Tree.Empty)
                rhs = transform(rhs, method.nextType().resultType());
	    return gen.DefDef(method, rhs);

        case LabelDef(_, Ident[] params, Tree body):
            Symbol label = tree.symbol();
            body = transform(body, label.nextType().resultType());
	    return gen.LabelDef(label, Tree.symbolOf(params), body);

	case Assign(Tree lhs, Tree rhs):
	    lhs = transform(lhs);
	    rhs = transform(rhs, lhs.type);
	    return gen.Assign(tree.pos, lhs, rhs);

	case Return(Tree expr):
            Symbol method = tree.symbol();
            Type type = method.nextType().resultType();
	    return gen.Return(tree.pos, method, transform(expr, type));

        case New(Template(Tree[] base, Tree[] body)):
            assert base.length == 1 && body.length == 0: tree;
            if (tree.getType().symbol() == definitions.ARRAY_CLASS) {
                switch (base[0]) {
                case Apply(_, Tree[] args):
                    assert args.length == 1: tree;
                    Type element = getArrayElementType(tree.getType()).erasure();
                    Tree size = transform(args[0]);
                    return genNewUnboxedArray(tree.pos, element, size);
                default:
                    throw Debug.abort("illegal case", tree);
                }
            }
	    return gen.New(tree.pos, transform(base[0]));

	case Apply(TypeApply(Tree fun, Tree[] targs), Tree[] vargs):
            fun = transform(fun);
            vargs = transform(vargs);
            Symbol symbol = fun.symbol();
            if (symbol == definitions.ANY_AS) {
                assert targs.length == 1 && vargs.length == 0: tree;
                return coerce(getQualifier(fun), targs[0].getType().erasure());
            }
            if (symbol == definitions.ANY_IS) {
                assert targs.length == 1 && vargs.length == 0: tree;
                Type type = targs[0].type.erasure();
                if (isUnboxedSimpleType(type)) type = targs[0].type;
                return gen.mkIsInstanceOf(tree.pos, getQualifier(fun), type);
            }
            return genApply(tree.pos, fun, vargs);

	case Apply(Tree fun, Tree[] vargs):
            fun = transform(fun);
            vargs = transform(vargs);
            switch (fun) {
            case Select(Apply(Tree bfun, Tree[] bargs), _):
                Symbol bsym = bfun.symbol();
                if (primitives.getPrimitive(bsym) != Primitive.BOX) break;
                assert bargs.length == 1: fun;
                switch (primitives.getPrimitive(fun.symbol())) {
                case COERCE:
                    assert vargs.length == 0: tree;
                    Tree value = bargs[0];
                    return coerce(value, fun.type().resultType());
                case LENGTH:
                    assert vargs.length == 0: tree;
                    Tree array = bargs[0];
                    return genUnboxedArrayLength(tree.pos, array);
                case APPLY:
                    assert vargs.length == 1: tree;
                    Tree array = bargs[0];
                    Tree index = vargs[0];
                    return genUnboxedArrayGet(tree.pos, array, index);
                case UPDATE:
                    assert vargs.length == 2: tree;
                    Tree array = bargs[0];
                    Tree index = vargs[0];
                    Tree value = vargs[1];
                    return genUnboxedArraySet(tree.pos, array, index, value);
                }
            }
            return genApply(tree.pos, fun, vargs);

	case Select(Tree qualifier, _):
            Symbol symbol = tree.symbol();
            Type prefix = qualifier.type().baseType(symbol.owner()).erasure();
            assert prefix != Type.NoType: tree + " -- " + Debug.show(symbol);
	    qualifier = transform(qualifier);
	    qualifier = coerce(qualifier, prefix);

            // Might end up with "box(unbox(...))". That's needed by backend.
            if (isUnboxedType(prefix)) qualifier = box(qualifier, true);
	    return gen.Select(tree.pos, qualifier, symbol);

        case Literal(AConstant.ZERO):
	    return gen.mkNullLit(tree.pos);

        case Block(_, _):
	case If(_, _, _):
        case Switch(_, _, _, _):
            return transform(tree, tree.getType().fullErasure());

        default:
            return super.transform(tree);
        }
    }

    //########################################################################
    // Private Methods - Tree transformation

    /** Transforms the given trees with given prototype. */
    private Tree[] transform(Tree[] trees, Type pt) {
        for (int i = 0; i < trees.length; i++) {
            Tree tree = transform(trees[i], pt);
            if (tree == trees[i]) continue;
            Tree[] array = new Tree[trees.length];
            for (int j = 0; j < i ; j++) array[j] = trees[j];
            array[i] = tree;
            while (++i < trees.length) array[i] = transform(trees[i], pt);
            return array;
        }
        return trees;
    }

    /** Transforms the given tree with given prototype. */
    private Tree transform(Tree tree, Type pt) {
        switch (tree) {

        case Block(Tree[] stats, Tree value):
            return gen.Block(tree.pos, transform(stats), transform(value, pt));

	case If(Tree cond, Tree thenp, Tree elsep):
	    cond = transform(cond, UNBOXED_BOOLEAN);
	    thenp = transform(thenp, pt);
	    elsep = transform(elsep, pt);
	    return gen.If(tree.pos, cond, thenp, elsep, pt);

        case Switch(Tree test, int[] tags, Tree[] bodies, Tree otherwise):
	    test = transform(test, UNBOXED_INT);
            bodies = transform(bodies, pt);
            otherwise = transform(otherwise, pt);
            return gen.Switch(tree.pos, test, tags, bodies, otherwise, pt);

        case Return(_):
            // !!! why do we build a block here?
            Tree value = transform(gen.mkDefaultValue(tree.pos, pt), pt);
            return gen.mkBlock(transform(tree), value);

        case LabelDef(_, _, _):
	case Assign(_, _):
        case New(_):
        case Apply(_, _):
        case Super(_, _):
        case This(_):
        case Select(_, _):
        case Ident(_):
        case Literal(_):
            return coerce(transform(tree), pt);

        default:
            throw Debug.abort("illegal case", tree);
        }
    }

    /** Transforms Unit literal with given prototype. */
    private Tree transformUnit(int pos, Type pt) {
        Tree unit = pt.isSameAs(UNBOXED_UNIT)
            ? gen.mkUnitLit(pos)
            : gen.mkApply__(gen.mkGlobalRef(pos, primitives.BOX_UVALUE));
        return coerce(unit, pt);
    }

    /** Coerces the given tree to the given type. */
    private Tree coerce(Tree tree, Type pt) {
        if (isSubType(tree.type(), pt)) {
            if (tree.type().symbol() == definitions.ARRAY_CLASS) {
                if (pt.symbol() != definitions.ARRAY_CLASS) {
                    Symbol symbol = primitives.UNBOX__ARRAY;
                    Tree unboxtree = gen.mkGlobalRef(tree.pos, symbol);
                    tree = gen.mkApply_V(unboxtree, new Tree[] {tree});
                    return coerce(tree, pt);
                }
            }
            return tree;
        }
        if (isUnboxedSimpleType(tree.type())) {
            if (isUnboxedSimpleType(pt)) return convert(tree, pt);
            Type to = pt.erasure();
            if (!isUnboxedSimpleType(to) || to.isSameAs(tree.type()))
                return box(tree);
            else
                return coerce(convert(tree, to), pt);
        } else if (isUnboxedArrayType(tree.type())) {
            if (!isUnboxedArrayType(pt)) return coerce(box(tree), pt);
        } else if (isUnboxedSimpleType(pt)) {
            Type from = tree.type().erasure();
            if (isUnboxedSimpleType(from))
                return convert(unbox(tree, from), pt);
            else
                return unbox(coerce(tree, boxUnboxedType(pt)), pt);
        } else if (isUnboxedArrayType(pt)) {
            if (tree.type.symbol() == definitions.ARRAY_CLASS)
                return unbox(tree, pt);
        } else if (pt.symbol() == definitions.ARRAY_CLASS) {
            Tree boxtree = gen.mkGlobalRef(tree.pos, primitives.BOX__ARRAY);
            return gen.mkApply_V(boxtree, new Tree[]{tree});
        }
        return gen.mkAsInstanceOf(tree, pt);
    }

    /** Boxes the given tree. */
    private Tree box(Tree tree) {
        return box(tree, false);
    }

    /** Boxes the given tree. */
    private Tree box(Tree tree, boolean force) {
        switch (tree) {
        case Apply(Tree fun, Tree[] args):
            if (primitives.getPrimitive(fun.symbol()) == Primitive.UNBOX) {
                assert args.length == 1: tree;
                if (!force) return args[0];
            }
        }
        Symbol symbol = primitives.getBoxValueSymbol(tree.getType());
	Tree boxtree = gen.mkGlobalRef(tree.pos, symbol);
        return tree.getType().equals(UNBOXED_UNIT)
            ? gen.mkBlock(tree, gen.mkApply__(boxtree))
            : gen.mkApply_V(boxtree, new Tree[]{tree});
    }

    /** Unboxes the given tree to the given type. */
    private Tree unbox(Tree tree, Type pt) {
        switch (tree) {
        case Apply(Tree fun, Tree[] args):
            if (primitives.getPrimitive(fun.symbol()) == Primitive.BOX) {
                assert args.length == 1: tree;
                return args[0];
            }
        }
        Symbol symbol = primitives.getUnboxValueSymbol(pt);
        return gen.mkApply_V(
            gen.mkGlobalRef(tree.pos, symbol), new Tree[]{tree});
    }

    /** Converts the given tree to the given type. */
    private Tree convert(Tree tree, Type to) {
        Symbol symbol = primitives.getConvertSymbol(tree.type(), to);
        return gen.mkApply_V(
            gen.mkGlobalRef(tree.pos, symbol), new Tree[]{tree});
    }

    //########################################################################
    // Private Methods - Tree generation

    /** Generates given bridge method forwarding to given method. */
    private Tree genBridgeMethod(Symbol bridge, Symbol method) {
        Type type = bridge.nextType();
        Tree body = genApply(bridge.pos,
            gen.Select(gen.This(bridge.pos, bridge.owner()), method),
            gen.mkLocalRefs(bridge.pos, type.valueParams()));
        return gen.DefDef(bridge, coerce(body, type.resultType()));
    }

    /** Generates an application with given function and arguments. */
    private Tree genApply(int pos, Tree fun, Tree[] args) {
        switch (fun.getType()) {
        case MethodType(Symbol[] params, Type result):
            Tree[] args1 = args;
            for (int i = 0; i < args.length; i++) {
                Tree arg = args[i];
                Tree arg1 = coerce(arg, params[i].nextType());
                if (arg1 != arg && args1 == args) {
                    args1 = new Tree[args.length];
                    for (int j = 0; j < i; j++) args1[j] = args[j];
                }
                args1[i] = arg1;
            }
            return gen.mkApply_V(pos, fun, args1);
        default:
            throw Debug.abort("illegal type " + fun.getType() + " for " + fun);
        }
    }

    /**
     * Generates a new unboxed array of given size and with elements
     * of given type.
     */
    private Tree genNewUnboxedArray(int pos, Type element, Tree size) {
        if (global.target == global.TARGET_INT) {
            Tree[] targs = {gen.mkType(pos, element)};
            Tree[] vargs = {coerce(size, UNBOXED_INT)};
            Tree fun = gen.mkGlobalRef(pos, primitives.NEW_OARRAY);
            return gen.mkApplyTV(fun, targs, vargs);
        }
        switch (element) {
        case UnboxedType(int kind): return genNewUnboxedArray(pos, kind, size);
        }
        String name = primitives.getNameForClassForName(element);
        Tree[] args = { coerce(size, UNBOXED_INT), gen.mkStringLit(pos,name) };
        Tree array =
            gen.mkApply_V(gen.mkGlobalRef(pos,primitives.NEW_OARRAY), args);
        return gen.mkAsInstanceOf(array, Type.UnboxedArrayType(element));
    }

    /**
     * Generates a new unboxed array of given size and with elements
     * of given unboxed type kind.
     */
    private Tree genNewUnboxedArray(int pos, int kind, Tree size) {
        Symbol symbol = primitives.getNewArraySymbol(kind);
        Tree[] args = { coerce(size, UNBOXED_INT) };
        return gen.mkApply_V(gen.mkGlobalRef(pos, symbol), args);
    }

    /** Generates an unboxed array length operation. */
    private Tree genUnboxedArrayLength(int pos, Tree array) {
        assert isUnboxedArrayType(array.getType()): array;
        Symbol symbol = primitives.getArrayLengthSymbol(array.getType());
        Tree[] args = { array };
        return gen.mkApply_V(gen.mkGlobalRef(pos, symbol), args);
    }

    /** Generates an unboxed array get operation. */
    private Tree genUnboxedArrayGet(int pos, Tree array, Tree index) {
        assert isUnboxedArrayType(array.getType()): array;
        Symbol symbol = primitives.getArrayGetSymbol(array.getType());
        index = coerce(index, UNBOXED_INT);
        Tree[] args = { array, index };
        return gen.mkApply_V(gen.mkGlobalRef(pos, symbol), args);
    }

    /** Generates an unboxed array set operation. */
    private Tree genUnboxedArraySet(int pos, Tree array,Tree index,Tree value){
        assert isUnboxedArrayType(array.getType()): array;
        Symbol symbol = primitives.getArraySetSymbol(array.getType());
        index = coerce(index, UNBOXED_INT);
        value = coerce(value, getArrayElementType(array.getType()));
        Tree[] args = { array, index, value };
        return gen.mkApply_V(gen.mkGlobalRef(pos, symbol), args);
    }

    //########################################################################
    // Private Methods - Queries

    /** Returns the qualifier of the given tree. */
    private Tree getQualifier(Tree tree) {
        switch (tree) {
        case Select(Tree qualifier, _):
            return qualifier;
        default:
            throw Debug.abort("no qualifier for tree", tree);
        }
    }

    /** Are the given erased types in a subtyping relation? */
    private boolean isSubType(Type tp1, Type tp2) {
        global.nextPhase();
        boolean result = tp1.isSubType(tp2);
        global.prevPhase();
        return result;
    }

    /** Are the given erased types in an equality relation? */
    private boolean isSameAs(Type tp1, Type tp2) {
        global.nextPhase();
        boolean result = tp1.isSameAs(tp2);
        global.prevPhase();
        return result;
    }

    /** Is the given type an unboxed type? */
    private boolean isUnboxedType(Type type) {
	switch (type) {
	case UnboxedType(_)     : return true;
	case UnboxedArrayType(_): return true;
	default                 : return false;
	}
    }

    /** Is the given type an unboxed simple type? */
    private boolean isUnboxedSimpleType(Type type) {
	switch (type) {
	case UnboxedType(_)     : return true;
	default                 : return false;
	}
    }

    /** Is the given type an unboxed array type? */
    private boolean isUnboxedArrayType(Type type) {
	switch (type) {
	case UnboxedArrayType(_): return true;
	default                 : return false;
	}
    }

    /** Returns the boxed version of the given unboxed type. */
    private Type boxUnboxedType(Type type) {
	switch (type) {
	case UnboxedType(TypeTags.UNIT):
            return definitions.UNIT_CLASS.type();
	case UnboxedType(TypeTags.BOOLEAN):
            return definitions.BOOLEAN_CLASS.type();
	case UnboxedType(TypeTags.BYTE):
            return definitions.BYTE_CLASS.type();
	case UnboxedType(TypeTags.SHORT):
            return definitions.SHORT_CLASS.type();
	case UnboxedType(TypeTags.CHAR):
            return definitions.CHAR_CLASS.type();
	case UnboxedType(TypeTags.INT):
            return definitions.INT_CLASS.type();
	case UnboxedType(TypeTags.LONG):
            return definitions.LONG_CLASS.type();
	case UnboxedType(TypeTags.FLOAT):
            return definitions.FLOAT_CLASS.type();
	case UnboxedType(TypeTags.DOUBLE):
            return definitions.DOUBLE_CLASS.type();
	case UnboxedArrayType(Type element):
            return Type.appliedType(
                definitions.ARRAY_CLASS.type(), new Type[] {element});
	default:
            throw Debug.abort("illegal case", type);
	}
    }

    /** Returns the element type of the given array type. */
    private Type getArrayElementType(Type type) {
        switch (type) {
        case TypeRef(_, Symbol symbol, Type[] args):
            if (symbol != definitions.ARRAY_CLASS) break;
            assert args.length == 1: type;
            return args[0];
        case UnboxedArrayType(Type element):
            return element;
        }
        throw Debug.abort("non-array type", type);
    }

    //########################################################################
    // Private Methods - Overlapping signatures detection

    /**
     * Checks that overloaded terms of the given class have no
     * overlapping erased signatures.
     */
    private void checkOverloadedTermsOf(Symbol clasz) {
        for (SymbolIterator si = clasz.members().iterator(); si.hasNext(); ) {
            Symbol symbol = si.next();
            if (!symbol.isTerm()) continue;
            switch (symbol.info()) {
            case OverloadedType(Symbol[] symbols, _):
                Type[] types = new Type[symbols.length];
                for (int i = 0; i < symbols.length; i++) {
                    types[i] = symbols[i].nextType();
                    for (int j = 0; j < i; j++) {
                        if (!types[i].isSameAs(types[j])) continue;
                        errorOverlappingSignatures(symbols[j], symbols[i]);
                        break;
                    }
                }
            }
        }
    }

    /** Reports an overlapping signature error for given symbols. */
    private void errorOverlappingSignatures(Symbol symbol1, Symbol symbol2) {
        SymbolTablePrinter printer = new SymbolTablePrinter(" ");
        printer.print("overlapping overloaded alternatives;").space();
        printer.print("the two following alternatives of").space();
        printer.printSymbolKindAndName(symbol1).space();
        printer.print("have the same erasure:").space();
        printer.printType(symbol1.nextType());
        Phase phase = global.currentPhase;
        global.currentPhase = global.PHASE.ANALYZER.phase();
        printer.indent();
        printer.line().print("alternative 1:").space().printSignature(symbol1);
        printer.line().print("alternative 2:").space().printSignature(symbol2);
        printer.undent();
        global.currentPhase = phase;
        unit.error(symbol2.pos, printer.toString());
    }

    //########################################################################
    //########################################################################
    //########################################################################
    //########################################################################

//////////////////////////////////////////////////////////////////////////////////
// Bridge Building
/////////////////////////////////////////////////////////////////////////////////

    private TreeList bridges;
    private HashMap bridgeSyms;

    /** Add bridge which Java-overrides `sym1' and which forwards to `sym'
     */
    public void addBridge(Symbol owner, Symbol sym, Symbol sym1) {
	Type bridgeType = sym1.nextType();

	// create bridge symbol and add to bridgeSyms(sym)
	// or return if bridge with required type already exists for sym.
	SymSet bridgesOfSym = (SymSet) bridgeSyms.get(sym);
	if (bridgesOfSym == null) bridgesOfSym = SymSet.EMPTY;
	Symbol[] brs = bridgesOfSym.toArray();
	for (int i = 0; i < brs.length; i++) {
	    if (isSameAs(brs[i].type(), bridgeType)) return;
	}
	Symbol bridgeSym = sym.cloneSymbol();
	bridgeSym.flags |= (SYNTHETIC | BRIDGE);
	bridgeSym.flags &= ~(JAVA | DEFERRED);
	bridgesOfSym = bridgesOfSym.incl(bridgeSym);
	bridgeSyms.put(sym, bridgesOfSym);
        bridgeSym.setOwner(owner);

	// check that there is no overloaded symbol with same erasure as bridge
	// todo: why only check for overloaded?
	Symbol overSym = owner.members().lookup(sym.name);
	switch (overSym.nextType()) {
	case OverloadedType(Symbol[] alts, Type[] alttypes):
	    for (int i = 0; i < alts.length; i++) {
		if (sym != alts[i] && isSameAs(bridgeType, alttypes[i])) {
		    unit.error(sym.pos, "overlapping overloaded alternatives; " +
			       "overridden " + sym1 + sym1.locationString() +
			       " has same erasure as " + alts[i] +
			       alttypes[i] + alts[i].locationString());
		}
	    }
	}

	switch (bridgeType) {
	case MethodType(Symbol[] params, Type restp):
	    // assign to bridge symbol its bridge type
	    // where owner of all parameters is bridge symbol itself.
	    Symbol[] params1 = new Symbol[params.length];
	    for (int i = 0; i < params.length; i++) {
		params1[i] = params[i].cloneSymbol(bridgeSym);
	    }
	    bridgeSym.setType(Type.MethodType(params1, restp));

            bridges.append(genBridgeMethod(bridgeSym, sym));
        }

    }

    /** Add an "empty bridge" (abstract method declaration) to satisfy
     *  CLR's requirement that classes should provide declaration
     *  for all methods of the interfaces they implement
     */
    public void addEmptyBridge(Symbol owner, Symbol method) {
	Type bridgeType = method.nextType();
	Symbol bridgeSym = method.cloneSymbol(owner);
	bridgeSym.flags = bridgeSym.flags & ~JAVA | SYNTHETIC | DEFERRED;
	//bridgeSym.setOwner(owner);
	switch (bridgeType) {
	case MethodType(Symbol[] params, Type restp):
	    // assign to bridge symbol its bridge type
	    // where owner of all parameters is bridge symbol itself.
	    Symbol[] params1 = new Symbol[params.length];
	    for (int i = 0; i < params.length; i++) {
		params1[i] = params[i].cloneSymbol(bridgeSym);
	    }
	    bridgeSym.setType(Type.MethodType(params1, restp));
	    Tree bridge = gen.DefDef(bridgeSym, Tree.Empty);
            bridges.append(bridge);
	}
    }

    private final Map interfaces/*<Symbol,Set<Symbol>>*/ = new HashMap();

    private Set getInterfacesOf(Symbol clasz) {
        assert clasz.isClass(): Debug.show(clasz);
        Set set = (Set)interfaces.get(clasz);
        if (set == null) {
            set = new HashSet();
            interfaces.put(clasz, set);
            Type parents[] = clasz.parents();
            for (int i = 0; i < parents.length; i++)
                set.addAll(getInterfacesOf(parents[i].symbol()));
            if (clasz.isInterface()) set.add(clasz);
        }
        return set;
    }

    private void addInterfaceBridges_(Symbol clasz) {
        assert clasz.isClass() && !clasz.isInterface(): Debug.show(clasz);
        assert clasz.parents().length > 0: Debug.show(clasz)+": "+clasz.info();
        Symbol svper = clasz.parents()[0].symbol();
        assert svper.isClass() && !svper.isInterface(): Debug.show(clasz);
        Set interfaces = new HashSet(getInterfacesOf(clasz));
        interfaces.removeAll(getInterfacesOf(svper));
        for (Iterator i = interfaces.iterator(); i.hasNext(); ) {
            Symbol inter = (Symbol)i.next();
            addInterfaceBridgesAux(clasz, inter.members());
        }
    }

    private void addInterfaceBridgesAux(Symbol owner, Scope symbols) {
        for (Scope.SymbolIterator i = symbols.iterator(true); i.hasNext();) {
            Symbol member = i.next();
            if (!member.isTerm() || !member.isDeferred()) continue;
            addInterfaceBridges(owner, member);
        }
    }


    private Symbol getOverriddenMethod(Symbol method) {
        Type[] parents = method.owner().parents();
        if (parents.length == 0) return Symbol.NONE;
        return method.overriddenSymbol(parents[0]);
    }

    public void addBridgeMethodsTo(Symbol method) {
        assert method.owner().isClass() && !method.owner().isInterface();
        Symbol overridden = getOverriddenMethod(method);
        if (overridden != Symbol.NONE && !isSameAs(overridden.nextType(), method.nextType()))
            addBridge(method.owner(), method, overridden);
    }

    public void addInterfaceBridges(Symbol owner, Symbol method) {
	assert owner.isClass() && !owner.isInterface(): Debug.show(owner);
        Symbol overriding = method.overridingSymbol(owner.thisType());
        if (overriding == method) {
            Symbol overridden = method.overriddenSymbol(owner.thisType().parents()[0], owner);
            if (overridden != Symbol.NONE && !isSameAs(overridden.nextType(), method.nextType()))
                addBridge(owner, method, overridden);
	    if (forMSIL && (overridden == Symbol.NONE
			    || (overridden != Symbol.NONE && overridden.owner() != owner)))
		    addEmptyBridge(owner, method);
        } else if (overriding != Symbol.NONE && !isSameAs(overriding.nextType(), method.nextType()))
            addBridge(owner, overriding, method);
    }

    private void addBridges(Symbol clasz, TreeList members) {
        TreeList savedBridges = bridges;
        HashMap savedBridgeSyms = bridgeSyms;
        bridges = new TreeList();
        bridgeSyms = new HashMap();

        int length = members.length();
        if (!clasz.isInterface()) {
            for (int i = 0; i < length; i++) {
                switch (members.get(i)) {
                case DefDef(_, _, _, _, _, Tree rhs):
                    addBridgeMethodsTo(members.get(i).symbol());
                }
            }
            addInterfaceBridges_(clasz);
        }

        members.append(bridges);
        if (bridges.length() > 0) {
            Type info = clasz.nextInfo();
            switch (info) {
            case CompoundType(Type[] parts, Scope members_):
                members_ = members_.cloneScope();
                for (int i = 0; i < bridges.length(); i++) {
                    Tree bridge = (Tree)bridges.get(i);
                    members_.enterOrOverload(bridge.symbol());
                }
                clasz.updateInfo(Type.compoundType(parts, members_, info.symbol()));
                break;
            default:
                throw Debug.abort("class = " + Debug.show(clasz) + ", " +
                    "info = " + Debug.show(info));
            }
        }
        bridgeSyms = savedBridgeSyms;
        bridges = savedBridges;
    }


}
