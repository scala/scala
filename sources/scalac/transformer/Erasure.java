/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: Erasure.java,v 1.48 2003/01/16 14:21:19 schinz Exp $
// $Id$

package scalac.transformer;

import java.util.HashMap;

import scalac.Global;
import scalac.Unit;
import scalac.ast.Tree;
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
import scalac.symtab.SymSet;
import scalac.symtab.Symbol;
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
    //########################################################################
    //########################################################################
    //########################################################################

    public void apply(Unit unit) {
	this.unit = unit;
	unit.body = transform(unit.body);
    }

//////////////////////////////////////////////////////////////////////////////////
// Box/Unbox and Coercions
/////////////////////////////////////////////////////////////////////////////////
    private boolean hasBoxing(Tree tree) {
        switch (tree) {
        case Apply(Tree fun, _):
            return primitives.getPrimitive(fun.symbol()) == Primitive.BOX;
        default:
            return false;
        }
    }

    /**
     * Return the unboxed version of the given tree.
     */
    private Tree removeBoxing(Tree tree) {
        switch (tree) {
        case Apply(Tree fun, Tree[] args):
            assert primitives.getPrimitive(fun.symbol())==Primitive.BOX: tree;
            assert args.length == 1: tree;
            return args[0];
        default:
            throw Debug.abort("illegal case", tree);
        }
    }

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
	return type.unbox() != type || type.symbol().fullName() == Names.scala_Array;
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
	Tree boxtree = gen.mkRef(tree.pos, primitives.RUNTIME_TYPE, boxSym(tree.type));
	switch (tree.type) {
	case UnboxedType(int kind):
	    if (kind == TypeTags.UNIT)
		return gen.Block(
		    tree.pos, new Tree[]{tree, gen.Apply(boxtree, new Tree[0])});
	}
	return gen.Apply(boxtree, new Tree[]{tree});
    }

    /** The symbol of the unbox method corresponding to unboxed type`unboxedtp'
     */
    Symbol unboxSym(Type unboxedtp) {
        return primitives.getUnboxValueSymbol(unboxedtp);
    }

    /** Emit tree.asType() or tree.asTypeArray(), where pt = Type or pt = Type[].
     */
    Tree unbox(Tree tree, Type pt) {
	Tree sel = gen.Select(tree, unboxSym(pt));
	return gen.Apply(sel, new Tree[0]);
    }

    /** Generate a select from an unboxed type.
     */
    public Tree unboxedSelect(Tree qual, Symbol sym) {
	return make.Select(qual.pos, qual, sym.name)
	    .setSymbol(sym)
            .setType(Type.singleType(boxedType(qual.type).symbol().thisType(),sym).erasure());
    }

    /** Subclass relation for class types; empty for other types.
     */
    boolean isSubClass(Type tp1, Type tp2) {
	Symbol sym1 = tp1.symbol();
	Symbol sym2 = tp2.symbol();
	return sym1 != null && sym2 != null && sym1.isSubClass(sym2);
    }

    /** Subtyping relation on erased types.
     */
    boolean isSubType(Type tp1, Type tp2) {
 	if (isSameAs(tp1, tp2)) return true;
	switch (tp2) {
	case UnboxedType(_):
	    return tp1.isSubType(tp2);
	case UnboxedArrayType(Type elemtp2):
	    switch (tp1) {
	    case UnboxedArrayType(Type elemtp1):
		return !(elemtp1 instanceof Type.UnboxedType) &&
		    isSubType(elemtp1, elemtp2);
	    default:
		return tp1.isSubType(tp2);
	    }
	}
	switch (tp1) {
	case UnboxedArrayType(Type elemtp1):
            if (tp2.symbol() == definitions.ANY_CLASS) return true;
        }
	return isSubClass(tp1, tp2);
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
	return isSubType(tree.type, pt) ? tree : cast(tree, pt);
    }

    Tree cast(Tree tree, Type pt) {
	if (global.debug) global.log("cast " + tree + ":" + tree.type + " to " + pt);//debug
	if (isSameAs(tree.type, pt)) {
	    return tree;
	} else if (isSubType(tree.type, pt)) {
	    return tree;
	} else if (isUnboxed(tree.type) && !isUnboxed(pt)) {
	    return cast(box(tree), pt);
	} else if ((isUnboxedArray(tree.type)
                    || (tree.type.symbol() == definitions.ANY_CLASS))
                   && isUnboxedArray(pt)) {
	    return
		make.Apply(tree.pos,
		    make.TypeApply(tree.pos,
			unboxedSelect(tree, definitions.AS),
			new Tree[]{gen.mkType(tree.pos, pt)})
		    .setType(new Type.MethodType(Symbol.EMPTY_ARRAY, pt)),
                   new Tree[0])
		.setType(pt);
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
	    return gen.Apply(
		unboxedSelect(box(tree), unboxSym(pt)),
		new Tree[0]);
	} else if (!isUnboxed(tree.type) && !isUnboxed(pt) ||
		   isUnboxedArray(tree.type) && isUnboxedArray(pt)) {
	    return
		gen.Apply(
		    gen.TypeApply(
			gen.Select(tree, definitions.AS),
			new Tree[]{gen.mkType(tree.pos, pt)}),
		    new Tree[0]);
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

    public void addBridges(Symbol sym) {
        Symbol c = sym.owner();
	if (c.isClass() && !c.isInterface()) {
            //global.nextPhase(); System.out.println("!!! " + Debug.show(sym) + " : " + sym.type().erasure() + " @ " + Debug.show(c)); global.prevPhase();
	    Type[] basetypes = c.parents();
	    for (int i = 0; i < basetypes.length; i++) {
                addBridges(basetypes[i], sym);
	    }
        }
    }

    public void addBridges(Type basetype, Symbol sym) {
        Symbol sym1 = sym.overriddenSymbol(basetype);
        //global.nextPhase();  System.out.println("!!!     " + Debug.show(sym) + " @ " + basetype + " -> " + Debug.show(sym1) + (sym1.kind == Kinds.NONE ? "" : " : " + sym1.type().erasure() + " => " + (isSameAs(sym1.type().erasure(), sym.type().erasure()) ? "ok" : "ADD BRIDGE"))); global.prevPhase();

        if (sym1.kind != Kinds.NONE &&
            !isSameAs(sym1.type().erasure(), sym.type().erasure())) {
            //System.out.println("!!! " + Debug.show(sym) + " adding bridge for " + Debug.show(sym1));
            addBridge(sym.owner(), sym, sym1);
        }
    }

    public void addInterfaceBridges(Symbol c, Symbol sym) {
	assert c.isClass() && !c.isInterface(): Debug.show(c);
        //global.nextPhase(); System.out.println("!!! " + Debug.show(sym) + " : " + sym.type().erasure() + " @ " + Debug.show(c)); global.prevPhase();
        Symbol member = sym;

        // !!! create a clone to make overriddenSymbol return the right symbol
        Symbol clone = sym.cloneSymbol();
        clone.setOwner(c);
        clone.setType(c.thisType().memberType(sym));

        sym = clone;
        //global.nextPhase(); System.out.println("!!! " + Debug.show(sym) + " : " + sym.type().erasure() + " @ " + Debug.show(c)); global.prevPhase();

        Type basetype = c.thisType();
        Symbol sym1 = sym.overriddenSymbol(basetype);
        sym = member;
        //global.nextPhase();  System.out.println("!!!     " + Debug.show(sym) + " @ " + basetype + " -> " + Debug.show(sym1) + (sym1.kind == Kinds.NONE ? "" : " : " + sym1.type().erasure() + " => " + (isSameAs(sym1.type().erasure(), sym.type().erasure()) ? "ok" : "ADD BRIDGE"))); global.prevPhase();

        if (sym1.kind != Kinds.NONE &&
            !isSameAs(sym1.type().erasure(), sym.type().erasure())) {
            //System.out.println("!!! " + Debug.show(sym) + " adding bridge for " + Debug.show(sym1));
            addBridge(c, sym1, member);
        }

    }

//////////////////////////////////////////////////////////////////////////////////
// Transformer
/////////////////////////////////////////////////////////////////////////////////

    private void addBridges(Symbol clasz, TreeList members) {
        TreeList savedBridges = bridges;
        HashMap savedBridgeSyms = bridgeSyms;
        bridges = new TreeList();
        bridgeSyms = new HashMap();

        int length = members.length();
        for (int i = 0; i < length; i++) {
            switch (members.get(i)) {
            case DefDef(_, _, _, _, _, Tree rhs):
                addBridges(members.get(i).symbol());
            }
        }

        if (!clasz.isInterface()) addInterfaceBridges(clasz);
        members.append(bridges);
        if (bridges.length() > 0) {
            Type info = clasz.nextInfo();
            switch (info) {
            case CompoundType(Type[] parts, Scope members_):
                members_ = new Scope(members_);
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

    /** Contract: every node needs to be transformed so that it's type is the
     *  erasure of the node's original type.  The only exception are functions;
     *  these are mapped to the erasure of the function symbol's type.
     */
    public Tree transform(Tree tree, boolean eraseFully) {
	assert tree.type != null : tree;
	Type owntype = eraseFully ? tree.type.fullErasure() : tree.type.erasure();
	switch (tree) {

	case ClassDef(_, _, _, _, _, Template(_, Tree[] body)):
            Symbol clasz = tree.symbol();
            TreeList members = new TreeList(transform(body));
            addBridges(clasz, members);
            return gen.ClassDef(clasz, members.toArray());

	case DefDef(_, _, _, _, _, Tree rhs):
            Symbol method = tree.symbol();
            if (rhs != Tree.Empty)
                rhs = transform(rhs, method.nextType().resultType());
	    return gen.DefDef(method, rhs);

	case ValDef(_, _, _, Tree rhs):
            Symbol field = tree.symbol();
	    if (rhs != Tree.Empty) rhs = transform(rhs, field.nextType());
	    return gen.ValDef(field, rhs);

	case AbsTypeDef(_, _, _, _):
	case AliasTypeDef(_, _, _, _):
	    // eliminate
	    return Tree.Empty;

        case Block(Tree[] stats):
            Tree[] newStats = new Tree[stats.length];
            for (int i = 0; i < stats.length; ++i)
                newStats[i] = transform(stats[i], true);
            return gen.Block(tree.pos, newStats);

	case Assign(Tree lhs, Tree rhs):
	    Tree lhs1 = transform(lhs);
	    Tree rhs1 = transform(rhs, lhs1.type);
	    return copy.Assign(tree, lhs1, rhs1).setType(owntype.fullErasure());

	case If(Tree cond, Tree thenp, Tree elsep):
	    Tree cond1 = transform(cond, Type.unboxedType(TypeTags.BOOLEAN));
	    Tree thenp1 = transform(thenp, owntype);
	    Tree elsep1 = (elsep == Tree.Empty) ? elsep : transform(elsep, owntype);
	    return copy.If(tree, cond1, thenp1, elsep1).setType(owntype);

        case Switch(Tree test, int[] tags, Tree[] bodies, Tree otherwise):
	    Tree test1 = transform(test, Type.unboxedType(TypeTags.INT));
            Tree[] bodies1 = transform(bodies, owntype);
            Tree otherwise1 = transform(otherwise, owntype);
            return copy.Switch(tree, test1, tags, bodies1, otherwise1).setType(owntype);

	case Return(Tree expr):
	    Tree expr1 = transform(expr,
                                   tree.symbol().type().resultType().fullErasure());
            Tree zero = gen.Ident(tree.pos, definitions.NULL);
	    return make.Block(tree.pos, new Tree[] {
                copy.Return(tree, expr1).setType(owntype), zero}).setType(zero.type());

        case New(Template templ):
            if (tree.type.symbol() == definitions.ARRAY_CLASS) {
                switch (templ.parents[0]) {
                case Apply(_, Tree[] args):
                    args = transform(args);
                    switch (owntype) {
                    case UnboxedArrayType(Type element):
                        return genNewUnboxedArray(tree.pos, element, args[0]);
                    default:
                        throw Debug.abort("illegal case", owntype);
                    }
                default:
                    throw Debug.abort("illegal case", templ.parents[0]);
                }
            }
	    return super.transform(tree).setType(owntype);

	case Typed(Tree expr, Tree tpe):
	    // coerce expr to tpe
	    Tree tpe1 = gen.mkType(tpe.pos, tpe.type().erasure()); // !!! was transform(tpe);
            // !!! More generally, we should never transform a tree
            // that represents a type. We should always transform
            // types and then reconstruct the corresponding tree.
	    return transform(expr, tpe1.type);

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
            case Select(Tree qualifier, _):
                if (!hasBoxing(qualifier)) break;
                switch (primitives.getPrimitive(fun.symbol())) {
                case LENGTH:
                    assert vargs.length == 0: tree;
                    Tree array = removeBoxing(qualifier);
                    return genUnboxedArrayLength(tree.pos, array);
                case APPLY:
                    assert vargs.length == 1: tree;
                    Tree array = removeBoxing(qualifier);
                    Tree index = vargs[0];
                    return genUnboxedArrayGet(tree.pos, array, index);
                case UPDATE:
                    assert vargs.length == 2: tree;
                    Tree array = removeBoxing(qualifier);
                    Tree index = vargs[0];
                    Tree value = vargs[1];
                    return genUnboxedArraySet(tree.pos, array, index, value);
                }
            }
            return genApply(tree.pos, fun, vargs);

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

        case LabelDef(Name name, Tree.Ident[] params,Tree body):
	    Tree.Ident[] new_params = new Tree.Ident[params.length];
	    for (int i = 0; i < params.length; i++) {
		new_params[i] = (Tree.Ident)gen.Ident(params[i].pos, params[i].symbol());
	    }

	    return copy.LabelDef(tree, new_params, transform(body)).setType(owntype);



        case Empty:
        case PackageDef(_,_):
        case Template(_,_):
        case Sequence(_): // !!! ? [BE:was Tuple before]
        case Super(_, _):
        case This(_):
        case Literal(_):
        case TypeTerm():
	    return super.transform(tree).setType(owntype);

        case Bad():
        case ModuleDef(_,_,_,_):
        case PatDef(_,_,_):
        case Import(_,_):
        case CaseDef(_,_,_):
        case Visitor(_):
        case Function(_,_):
        case SingletonType(_):
        case SelectFromType(_,_):
        case FunType(_,_):
        case CompoundType(_,_):
        case AppliedType(_, _):
            throw Debug.abort("illegal case", tree);

	default:
            throw Debug.abort("unknown case", tree);
	}
    }

    public Tree transform(Tree tree) {
        return transform(tree, false);
    }

    // !!! This is just rapid fix. Needs to be reviewed.
    private void addInterfaceBridges(Symbol owner) {
        Type[] parents = owner.info().parents();
        for (int i = 1; i < parents.length; i++)
            addInterfaceBridgesRec(owner, parents[i].symbol());
    }
    private void addInterfaceBridgesRec(Symbol owner, Symbol interfase) {
        addInterfaceBridgesAux(owner, interfase.nextInfo().members());
        Type[] parents = interfase.parents();
        for (int i = 0; i < parents.length; i++) {
            Symbol clasz = parents[i].symbol();
            if (clasz.isInterface()) addInterfaceBridgesRec(owner, clasz);
        }
    }
    private void addInterfaceBridgesAux(Symbol owner, Scope symbols) {
        for (Scope.SymbolIterator i = symbols.iterator(true); i.hasNext();) {
            Symbol member = i.next();
            if (!member.isTerm() || !member.isDeferred()) continue;
            addInterfaceBridges(owner, member);
        }
    }

    /** Transform with prototype
     */
    Tree transform(Tree expr, Type pt) {
	return coerce(transform(expr), pt);
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

