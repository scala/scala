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
import scalac.ast.Transformer;
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
public class Erasure extends Transformer implements Modifiers {

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

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public Erasure(Global global) {
        super(global);
	this.definitions = global.definitions;
        this.primitives = global.primitives;
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

        case Empty:
            return tree;

	case ClassDef(_, _, _, _, _, Template(_, Tree[] body)):
            Symbol clasz = tree.symbol();
            TreeList members = new TreeList(transform(body));
            checkOverloadedTermsOf(clasz);
            addBridges(clasz, members);
            return gen.ClassDef(clasz, members.toArray());

        case PackageDef(Tree packaged, Template(_, Tree[] body)):
            return gen.PackageDef(packaged.symbol(), transform(body));

	case ValDef(_, _, _, Tree rhs):
            Symbol field = tree.symbol();
	    if (rhs != Tree.Empty) rhs = transform(rhs, field.nextType());
	    return gen.ValDef(field, rhs);

	case DefDef(_, _, _, _, _, Tree rhs):
            Symbol method = tree.symbol();
            if (rhs != Tree.Empty)
                rhs = transform(rhs, method.nextType().resultType());
	    return gen.DefDef(method, rhs);

	case AbsTypeDef(_, _, _, _):
	case AliasTypeDef(_, _, _, _):
	    // eliminate // !!!
	    return Tree.Empty;

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
            if (tree.type().symbol() == definitions.ARRAY_CLASS) {
                switch (base[0]) {
                case Apply(_, Tree[] args):
                    assert args.length == 1: tree;
                    Type element = getArrayElementType(tree.type()).erasure();
                    Tree size = transform(args[0]);
                    return genNewUnboxedArray(tree.pos, element, size);
                default:
                    throw Debug.abort("illegal case", tree);
                }
            }
	    return gen.New(tree.pos, transform(base[0]));

	case Typed(Tree expr, _): // !!!
            return transform(expr);

	case Apply(TypeApply(Tree fun, Tree[] targs), Tree[] vargs):
            fun = transform(fun);
            vargs = transform(vargs);
            Symbol symbol = fun.symbol();
            if (symbol == definitions.AS) {
                assert targs.length == 1 && vargs.length == 0: tree;
                return coerce(getQualifier(fun), targs[0].type().erasure());
            }
            if (symbol == definitions.IS) {
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

        case Super(_, _):
	    return gen.Super(tree.pos, tree.symbol());

        case This(_):
	    return gen.This(tree.pos, tree.symbol());

	case Select(Tree qualifier, _):
            Symbol symbol = tree.symbol();
            qualifier = transform(qualifier);
            qualifier = coerce(qualifier, symbol.owner().type().erasure());
            if (isUnboxedType(qualifier.type())) qualifier = box(qualifier);
	    return gen.Select(tree.pos, qualifier, symbol);

	case Ident(_):
            Symbol symbol = tree.symbol();
	    if (symbol == definitions.ZERO) return gen.mkNullLit(tree.pos);
            return gen.Ident(tree.pos, symbol);

        case Literal(Object value):
	    return gen.mkLit(tree.pos, value);

        case Block(_):
	case If(_, _, _):
        case Switch(_, _, _, _):
            return transform(tree, tree.type().fullErasure());

        case Bad():
        case ModuleDef(_, _, _, _):
        case PatDef(_, _, _):
        case Import(_, _):
        case CaseDef(_, _, _):
        case Template(_, _):
        case Sequence(_):
        case Alternative(_):
        case Bind(_, _):
        case Visitor(_):
        case Function(_, _):
        case Throw(_):
        case TypeApply(_, _):

        case TypeTerm():
        case SingletonType(_):
        case SelectFromType(_, _):
        case FunType(_, _):
        case CompoundType(_, _):
        case AppliedType(_, _):
        case Try(_, _, _): // !!!
            throw Debug.abort("illegal case", tree);

        default:
            throw Debug.abort("illegal case", tree);

        }
    }

    //########################################################################
    // Private Methods - Tree transformation

    /** Transforms the given tree with given prototype. */
    private Tree transform(Tree tree, Type pt) {
        switch (tree) {

        case Block(Tree[] stats):
            if (stats.length == 0) return transformUnit(tree.pos, pt);
            stats = Tree.cloneArray(stats);
            for (int i = 0; i < stats.length - 1; i++)
                stats[i] = transform(stats[i]);
            stats[stats.length - 1] = transform(stats[stats.length - 1], pt);
            return gen.Block(tree.pos, stats);

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
            Tree value = transform(gen.mkDefaultValue(tree.pos, pt), pt);
            return gen.mkBlock(new Tree[] {transform(tree), value});

	case Typed(Tree expr, _): // !!!
	    return transform(expr, pt);

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

        case Bad():
        case Empty:
	case ClassDef(_, _, _, _, _, _):
        case PackageDef(_, _):
        case ModuleDef(_, _, _, _):
	case ValDef(_, _, _, _):
        case PatDef(_, _, _):
	case DefDef(_, _, _, _, _, _):
	case AbsTypeDef(_, _, _, _):
	case AliasTypeDef(_, _, _, _):
        case Import(_, _):
        case CaseDef(_, _, _):
        case Template(_, _):
        case Sequence(_):
        case Alternative(_):
        case Bind(_, _):
        case Visitor(_):
        case Function(_, _):
        case Throw(_):
        case TypeApply(_, _):
        case TypeTerm():
        case SingletonType(_):
        case SelectFromType(_, _):
        case FunType(_, _):
        case CompoundType(_, _):
        case AppliedType(_, _):
        case Try(_, _, _): // !!!
            throw Debug.abort("illegal case", tree);
        default:
            throw Debug.abort("illegal case", tree);
        }
    }

    /** Transforms Unit literal with given prototype. */
    private Tree transformUnit(int pos, Type pt) {
        Tree unit = pt.isSameAs(UNBOXED_UNIT)
            ? gen.mkUnitLit(pos)
            : gen.mkApply__(gen.mkRef(pos, primitives.BOX_UVALUE));
        return coerce(unit, pt);
    }

    //########################################################################
    // Private Methods - Tree generation

    /** Generates given bridge method forwarding to given method. */
    private Tree genBridgeMethod(Symbol bridge, Symbol method) {
        Type type = bridge.nextType();
        Tree body = genApply(bridge.pos,
            gen.Select(gen.This(bridge.pos, bridge.owner()), method),
            gen.mkRefs(bridge.pos, type.valueParams()));
        return gen.DefDef(bridge, coerce(body, type.resultType()));
    }

    /** Generates an application with given function and arguments. */
    private Tree genApply(int pos, Tree fun, Tree[] args) {
        switch (fun.type()) {
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
            throw Debug.abort("illegal type " + fun.type() + " for " + fun);
        }
    }

    /**
     * Generates a new unboxed array of given size and with elements
     * of given type.
     */
    private Tree genNewUnboxedArray(int pos, Type element, Tree size) {
        switch (element) {
        case UnboxedType(int kind): return genNewUnboxedArray(pos, kind, size);
        }
        if (global.target == global.TARGET_INT) {
            global.nextPhase();
            while (!element.symbol().isJava()) element = element.parents()[0];
            global.prevPhase();
        }
        String name = primitives.getNameForClassForName(element);
        Tree[] args = { coerce(size, UNBOXED_INT), gen.mkStringLit(pos,name) };
        Tree array = gen.mkApply_V(gen.mkRef(pos,primitives.NEW_OARRAY), args);
        return gen.mkAsInstanceOf(array, Type.UnboxedArrayType(element));
    }

    /**
     * Generates a new unboxed array of given size and with elements
     * of given unboxed type kind.
     */
    private Tree genNewUnboxedArray(int pos, int kind, Tree size) {
        Symbol symbol = primitives.getNewArraySymbol(kind);
        Tree[] args = { coerce(size, UNBOXED_INT) };
        return gen.mkApply_V(gen.mkRef(pos, symbol), args);
    }

    /** Generates an unboxed array length operation. */
    private Tree genUnboxedArrayLength(int pos, Tree array) {
        assert isUnboxedArray(array.type()): array;
        Symbol symbol = primitives.getArrayLengthSymbol(array.type());
        Tree[] args = { array };
        return gen.mkApply_V(gen.mkRef(pos, symbol), args);
    }

    /** Generates an unboxed array get operation. */
    private Tree genUnboxedArrayGet(int pos, Tree array, Tree index) {
        assert isUnboxedArray(array.type()): array;
        Symbol symbol = primitives.getArrayGetSymbol(array.type());
        index = coerce(index, UNBOXED_INT);
        Tree[] args = { array, index };
        return gen.mkApply_V(gen.mkRef(pos, symbol), args);
    }

    /** Generates an unboxed array set operation. */
    private Tree genUnboxedArraySet(int pos, Tree array,Tree index,Tree value){
        assert isUnboxedArray(array.type()): array;
        Symbol symbol = primitives.getArraySetSymbol(array.type());
        index = coerce(index, UNBOXED_INT);
        value = coerce(value, getArrayElementType(array.type()));
        Tree[] args = { array, index, value };
        return gen.mkApply_V(gen.mkRef(pos, symbol), args);
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
        printer.printSymbol(symbol1).space();
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
// Box/Unbox and Coercions
/////////////////////////////////////////////////////////////////////////////////

    private boolean isUnboxedType(Type type) {
	switch (type) {
	case UnboxedType(_)     : return true;
	case UnboxedArrayType(_): return true;
	default                 : return false;
	}
    }

    private boolean isUnboxedSimpleType(Type type) {
	switch (type) {
	case UnboxedType(_)     : return true;
	default                 : return false;
	}
    }

    private boolean isUnboxedArrayType(Type type) {
	switch (type) {
	case UnboxedArrayType(_): return true;
	default                 : return false;
	}
    }

    boolean isUnboxed(Type type) {
	switch (type) {
	case UnboxedType(_): case UnboxedArrayType(_): return true;
	default: return false;
	}
    }

    boolean isUnboxedArray(Type type) {
	switch (type) {
	case UnboxedArrayType(_): return true;
	default: return false;
	}
    }

    boolean isBoxed(Type type) {
	return type.unbox() != type || type.symbol() == definitions.ARRAY_CLASS;
    }

    Type boxedType(Type tp) {
	switch (tp) {
	case UnboxedType(int kind):
	    return definitions.getType(Type.boxedFullName(kind));
	case UnboxedArrayType(Type elemtp):
            return definitions.arrayType(boxedType(elemtp));
	default:
	    return tp;
	}
    }

    Symbol boxSym(Type unboxedtp) {
        return primitives.getBoxValueSymbol(unboxedtp);
    }

    /** Emit `scala.RunTime.box(tree)' or
     *  `{ tree ; scala.RunTime.box() }' if type of `tree' is `void'.
     */
    Tree box(Tree tree) {
	Tree boxtree = gen.mkRef(tree.pos, boxSym(tree.type()));
        return tree.type().equals(UNBOXED_UNIT)
            ? gen.Block(new Tree[]{tree, gen.mkApply__(boxtree)})
            : gen.mkApply_V(boxtree, new Tree[]{tree});
    }

    /** The symbol of the unbox method corresponding to unboxed type`unboxedtp'
     */
    Symbol unboxSym(Type unboxedtp) {
        return primitives.getUnboxValueSymbol(unboxedtp);
    }

    /** Emit tree.asType() or tree.asTypeArray(), where pt = Type or pt = Type[].
     */
    Tree unbox(Tree tree, Type pt) {
	return gen.mkApply__(gen.Select(tree, unboxSym(pt)));
    }

    /** Generate a select from an unboxed type.
     */
    public Tree unboxedSelect(Tree qual, Symbol sym) {
	return gen.Select(qual, sym);
    }

    /** Subtyping relation on erased types.
     */
    boolean isSubType(Tree tree, Type tp2) {
        global.nextPhase();
        boolean test = tp1.isSubType(tp2);
        global.prevPhase();
        return test;
    }

    /** Equality relation on erased types.
     */
    boolean isSameAs(Type tp1, Type tp2) {
        global.nextPhase();
        boolean result = tp1.isSameAs(tp2);
        global.prevPhase();
        return result;
    }

    Tree coerce(Tree tree, Type pt) {
	return isSubType(tree, pt) ? tree : cast(tree, pt);
    }

    Tree cast(Tree tree, Type pt) {
	if (global.debug) global.log("cast " + tree + ":" + tree.type + " to " + pt);//debug
	if (isSameAs(tree.type, pt)) {
	    return tree;
        } else if (isSubType(tree, pt)) {
	    return tree;
	} else if (isUnboxed(tree.type) && !isUnboxed(pt)) {
	    return cast(box(tree), pt);
	} else if ((isUnboxedArray(tree.type)
                    || (tree.type.symbol() == definitions.ANY_CLASS))
                   && isUnboxedArray(pt)) {
	    return gen.mkAsInstanceOf(tree, pt);
	} else if (!isUnboxed(tree.type) && isUnboxed(pt)) {
	    if (isBoxed(tree.type)) {
		return coerce(unbox(tree, pt), pt);
	    } else {
		Type bt = boxedType(pt);
		while (isBoxed(bt.parents()[0])) {
		    bt = bt.parents()[0];
		}
		return cast(coerce(tree, bt), pt);
	    }
	} else if (isUnboxed(tree.type) && isUnboxed(pt)) {
	    return gen.mkApply__(gen.Select(box(tree), unboxSym(pt)));
	} else if (!isUnboxed(tree.type) && !isUnboxed(pt) ||
		   isUnboxedArray(tree.type) && isUnboxedArray(pt)) {
	    return gen.mkAsInstanceOf(tree, pt);
	} else {
	    throw Debug.abort("cannot cast " + tree.type + " to " + pt);
	}
    }

//////////////////////////////////////////////////////////////////////////////////
// Bridge Building
/////////////////////////////////////////////////////////////////////////////////

    private TreeList bridges;
    private HashMap bridgeSyms;

    /** Add bridge which Java-overrides `sym1' and which forwards to `sym'
     */
    public void addBridge(Symbol owner, Symbol sym, Symbol sym1) {
	Type bridgeType = sym1.type().erasure();

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
	switch (overSym.type()) {
	case OverloadedType(Symbol[] alts, Type[] alttypes):
	    for (int i = 0; i < alts.length; i++) {
		if (sym != alts[i] && isSameAs(bridgeType, alttypes[i].erasure())) {
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

//////////////////////////////////////////////////////////////////////////////////
// Transformer
/////////////////////////////////////////////////////////////////////////////////

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

    Tree[] transform(Tree[] exprs, Type pt) {
        for (int i = 0; i < exprs.length; i++) {
            Tree tree = transform(exprs[i], pt);
            if (tree == exprs[i]) continue;
            Tree[] trees = new Tree[exprs.length];
            for (int j = 0; j < i ; j++) trees[j] = exprs[j];
            trees[i] = tree;
            while (++i < exprs.length) trees[i] = transform(exprs[i], pt);
            return trees;
        }
        return exprs;
    }

    private Tree getQualifier(Tree tree) {
        switch (tree) {
        case Select(Tree qual, _):
            return qual;
        default:
            throw Debug.abort("no qualifier for tree", tree);
        }
    }


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

}

