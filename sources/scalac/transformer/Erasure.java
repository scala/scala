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
import scalac.ast.Tree.TypeDef;
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

    private final Definitions definitions;
    private final Primitives primitives;

    private Unit unit;

    public Erasure(Global global) {
        super(global);
	this.definitions = global.definitions;
        this.primitives = global.primitives;
    }

    public void apply(Unit unit) {
	this.unit = unit;
	unit.body = transform(unit.body);
    }

//////////////////////////////////////////////////////////////////////////////////
// Box/Unbox and Coercions
/////////////////////////////////////////////////////////////////////////////////

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
    public void addBridge(Symbol sym, Symbol sym1) {
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
	bridgeSym.flags &= ~JAVA;
	bridgesOfSym = bridgesOfSym.incl(bridgeSym);
	bridgeSyms.put(sym, bridgesOfSym);

	// check that there is no overloaded symbol with same erasure as bridge
	Symbol overSym = sym.owner().members().lookup(sym.name);
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
		params1[i] = params[i].cloneSymbol();
		params1[i].setOwner(bridgeSym);
	    }
	    bridgeSym.setType(Type.MethodType(params1, restp));

	    // create bridge definition
	    Type symtype = sym.type().erasure();
	    switch (symtype) {
	    case MethodType(Symbol[] symparams, Type symrestp):
		assert params1.length == symparams.length;
		Tree[] args = new Tree[params1.length];
		for (int i = 0; i < args.length; i++) {
		    args[i] = cast(gen.Ident(params1[i]), symparams[i].type().erasure());
		}
		Tree fwd = make.Apply(sym.pos, gen.Ident(sym).setType(symtype), args)
		    .setType(symrestp);
		bridges.append(gen.DefDef(bridgeSym, coerce(fwd, restp)));
		return;
	    }
	}
	throw Debug.abort("bad bridge types " + bridgeType + "," + sym.type().erasure());
    }

    public void addBridges(Symbol sym) {
	Symbol c = sym.owner();
	if (c.isClass() && !c.isInterface()) {
	    Type[] basetypes = c.parents();

	    //System.out.println("trying " + c + " <= " + ArrayApply.toString(c.basetypes()));//DEBUG

	    for (int i = 0; i < basetypes.length; i++) {
		Symbol sym1 = sym.overriddenSymbol(basetypes[i]);

		//if (sym1.kind != NONE) System.out.println("overridden: " + sym1 + sym1.locationString() + " by " + sym + sym.locationString());//DEBUG

		if (sym1.kind != Kinds.NONE &&
		    !isSameAs(sym1.type().erasure(), sym.type().erasure())) {

		    //System.out.println("add bridge: " + sym1 + sym1.locationString() + " by " + sym + sym.locationString());//DEBUG

		    addBridge(sym, sym1);
		}
	    }
	}
    }

//////////////////////////////////////////////////////////////////////////////////
// Transformer
/////////////////////////////////////////////////////////////////////////////////

    /** Contract: every node needs to be transformed so that it's type is the
     *  erasure of the node's original type.  The only exception are functions;
     *  these are mapped to the erasure of the function symbol's type.
     */
    Symbol currentClass = null;
    public Tree transform(Tree tree, boolean eraseFully) {
	assert tree.type != null : tree;
	Type owntype = eraseFully ? tree.type.fullErasure() : tree.type.erasure();
	switch (tree) {
	case ClassDef(_, _, TypeDef[] tparams, ValDef[][] vparams, Tree tpe, Template impl):
            Symbol oldCurrentClass = currentClass;
            currentClass = tree.symbol();
            Tree newTree =
                copy.ClassDef(tree, new TypeDef[0],
                              transform(vparams), tpe, transform(impl, tree.symbol()))
		.setType(owntype);
            currentClass = oldCurrentClass;
            return newTree;

	case DefDef(_, _, TypeDef[] tparams, ValDef[][] vparams, Tree tpe, Tree rhs):
	    addBridges(tree.symbol());
	    Tree tpe1 = gen.mkType(tpe.pos, tpe.type.fullErasure());
	    Tree rhs1 = (rhs == Tree.Empty) ? rhs : transform(rhs, tpe1.type);
	    return copy.DefDef(
		tree, new TypeDef[0], transform(vparams), tpe1, rhs1)
		.setType(owntype);

	case ValDef(_, _, Tree tpe, Tree rhs):
	    Tree tpe1 = transform(tpe);
	    Tree rhs1 = (rhs == Tree.Empty) ? rhs : transform(rhs, tpe1.type);
	    return copy.ValDef(tree, tpe1, rhs1).setType(owntype);

	case TypeDef(_, _, _, _):
	    // eliminate
	    return Tree.Empty;

        case Block(Tree[] stats):
            Tree[] newStats = new Tree[stats.length];
            for (int i = 0; i < stats.length; ++i)
                newStats[i] = transform(stats[i], true);
            return copy.Block(tree, newStats).setType(owntype.fullErasure());

	case Assign(Tree lhs, Tree rhs):
	    Tree lhs1 = transformLhs(lhs);
	    Tree rhs1 = transform(rhs, lhs1.type);
	    return copy.Assign(tree, lhs1, rhs1).setType(owntype.fullErasure());

	case If(Tree cond, Tree thenp, Tree elsep):
	    Tree cond1 = transform(cond, Type.unboxedType(TypeTags.BOOLEAN));
	    Tree thenp1 = transform(thenp, owntype);
	    Tree elsep1 = (elsep == Tree.Empty) ? elsep : transform(elsep, owntype);
	    return copy.If(tree, cond1, thenp1, elsep1).setType(owntype);

        case New(Template templ):
            if (tree.type.symbol() == definitions.UNIT_CLASS)
                // !!! return Tree.Literal(UNIT, null).setType(owntype);
                throw Debug.abort("found unit literal");
            if (tree.type.symbol() == definitions.ARRAY_CLASS) {
                switch (templ.parents[0]) {
                case Apply(_, Tree[] args):
                    args = transform(args);
                    switch (owntype) {
                    case UnboxedArrayType(Type elemtp):
                        switch (elemtp) {
                        case UnboxedType(int kind):
                            return genNewArray(tree.pos,args[0],kind);
                        default:
                            return genNewArray(tree.pos,args[0],elemtp);
                        }
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

	case TypeApply(Tree fun, Tree[] args):
            Symbol sym = fun.symbol();
            if (sym == definitions.AS || sym == definitions.IS) {
                Type tp = args[0].type.erasure();
                if (isUnboxed(tp)) {
                    Tree qual1 = transform(getQualifier(currentClass, fun));
                    if (isUnboxed(qual1.type)) qual1 = box(qual1);
                    Symbol primSym = (sym == definitions.AS)
                        ? primitives.getUnboxValueSymbol(tp)
                        : primitives.getInstanceTestSymbol(tp);
                    qual1 = coerce(qual1, primSym.owner().type());
                    return gen.Select(qual1, primSym);
                } else
                    return copy.TypeApply(tree, transform(fun), transform(args))
                        .setType(owntype);
            } else
                return transform(fun);

	case Apply(Tree fun, Tree[] args):
            switch (fun) {
            case Select(Tree array, _):
                if (isUnboxedArray(array.type().erasure())) {
                    switch (primitives.getPrimitive(fun.symbol())) {
                    case LENGTH: return transformLength(tree);
                    case APPLY: return transformApply(tree);
                    case UPDATE: return transformUpdate(tree);
                    }
                }
            }
	    Tree fun1 = transform(fun);
            if (fun1.symbol() == definitions.NULL) return fun1.setType(owntype);
	    if (global.debug) global.log("fn: " + fun1.symbol() + ":" + fun1.type);//debug
	    switch (fun1.type) {
	    case MethodType(Symbol[] params, Type restpe):
		Tree[] args1 = args;
		for (int i = 0; i < args.length; i++) {
		    Tree arg = args[i];
		    Type pt1 = params[i].type().erasure();
		    Tree arg1 = cast(transform(arg, pt1), pt1);
		    if (arg1 != arg && args1 == args) {
			args1 = new Tree[args.length];
			System.arraycopy(args, 0, args1, 0, i);
		    }
		    args1[i] = arg1;
		}
		return coerce(copy.Apply(tree, fun1, args1).setType(restpe), owntype);
	    default:
		global.debugPrinter.print(fun1);
		throw Debug.abort("bad method type: " + Debug.show(fun1.type) + " " + Debug.show(fun1.symbol()));
	    }

	case Select(_, _):
	case Ident(_):
	    Tree tree1 = transformLhs(tree);
	    //global.log("id: " + tree1+": "+tree1.type+" -> "+owntype);//DEBUG
	    return (tree1.type instanceof Type.MethodType) ? tree1
		: coerce(tree1, owntype);

        case Empty:
        case PackageDef(_,_):
        case Template(_,_):
        case Sequence(_): // !!! ? [BE:was Tuple before]
        case Super(_):
        case This(_):
        case Literal(_):
        case TypeTerm():
	    return super.transform(tree).setType(owntype);

        case Bad():
        case ModuleDef(_,_,_,_):
        case PatDef(_,_,_):
        case Import(_,_):
        case CaseDef(_,_,_):
        case LabelDef(_,_):
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

    public Template transform(Template templ, Symbol clazz) {
	TreeList savedBridges = bridges;
	HashMap savedBridgeSyms = bridgeSyms;
	bridges = new TreeList();
	bridgeSyms = new HashMap();
	Tree[] bases1 = transform(templ.parents);
	TreeList body1 = new TreeList(transform(templ.body));
	body1.append(bridges);
        if (bridges.length() > 0) {
            Type info = clazz.nextInfo();
            switch (info) {
            case CompoundType(Type[] parts, Scope members):
                members = new Scope(members);
                for (int i = 0; i < bridges.length(); i++) {
                    Tree bridge = (Tree)bridges.get(i);
                    members.enterOrOverload(bridge.symbol());
                }
                clazz.updateInfo(Type.compoundType(parts, members, info.symbol()));
                break;
            default:
                throw Debug.abort("class = " + Debug.show(clazz) + ", " +
                    "info = " + Debug.show(info));
            }
        }
	bridges = savedBridges;
	bridgeSyms = savedBridgeSyms;
	return (Template) copy.Template(templ, bases1, body1.toArray())
	    .setType(templ.type.erasure());
    }

    /** Transform without keeping the previous transform's contract.
     */
    Tree transformLhs(Tree tree) {
	Tree tree1;
	switch (tree) {
	case Ident(_):
	    tree1 = tree;
	    break;
	case Select(Tree qual, _):
            Symbol sym = tree.symbol();
	    Tree qual1 = transform(qual);
	    if (isUnboxed(qual1.type))
                if (!isUnboxedArray(qual1.type) || sym == definitions.ARRAY_CLASS)
                    qual1 = box(qual1);
	    tree1 = copy.Select(tree, sym, qual1);
	    break;
	default:
	    throw Debug.abort("illegal case", tree);
	}
	if (global.debug) global.log("id: " + tree1.symbol() + ":" + tree1.symbol().type().erasure());//debug
	return tree1.setType(tree1.symbol().type().erasure());
    }

    /** Transform with prototype
     */
    Tree transform(Tree expr, Type pt) {
	return coerce(transform(expr), pt);
    }

    /** Transform an array length */
    Tree transformLength(Tree tree) {
        switch (tree) {
        case Apply(Select(Tree array, _), Tree[] args):
            assert args.length == 0 : Debug.show(args);
            Type finalType = tree.type().erasure();
            array = transform(array);
            Symbol symbol = primitives.getArrayLengthSymbol(array.type());
            Tree method = gen.mkRef(tree.pos,primitives.RUNTIME_TYPE,symbol);
            args = new Tree[] { array };
            return coerce(gen.Apply(tree.pos, method, args), finalType);
        default:
            throw Debug.abort("illegal case", tree);
        }
    }

    /** Transform an array apply */
    Tree transformApply(Tree tree) {
        switch (tree) {
        case Apply(Select(Tree array, _), Tree[] args):
            assert args.length == 1 : Debug.show(args);
            Type finalType = tree.type().erasure();
            array = transform(array);
            Symbol symbol = primitives.getArrayGetSymbol(array.type());
            Tree method = gen.mkRef(tree.pos,primitives.RUNTIME_TYPE,symbol);
            args = new Tree[] { array, transform(args[0]) };
            return coerce(gen.Apply(tree.pos, method, args), finalType);
        default:
            throw Debug.abort("illegal case", tree);
        }
    }

    /** Transform an array update */
    Tree transformUpdate(Tree tree) {
        switch (tree) {
        case Apply(Select(Tree array, _), Tree[] args):
            assert args.length == 2 : Debug.show(args);
            array = transform(array);
            Symbol symbol = primitives.getArraySetSymbol(array.type());
            Tree method = gen.mkRef(tree.pos,primitives.RUNTIME_TYPE,symbol);
            args = new Tree[] { array, transform(args[0]),transform(args[1]) };
            return gen.Apply(tree.pos, method, args);
        default:
            throw Debug.abort("illegal case", tree);
        }
    }

    private Tree getQualifier(Symbol currentClass, Tree tree) {
        switch (tree) {
        case Select(Tree qual, _):
            return qual;
        case Ident(_):
            assert currentClass != null;
            if (currentClass.isSubClass(tree.symbol().owner()))
                return gen.This(tree.pos, currentClass);
            else
                throw Debug.abort("no qualifier for tree", tree);
        default:
            throw Debug.abort("no qualifier for tree", tree);
        }
    }

    private Tree genNewArray(int pos, Tree size, Type elemtp) {
        Tree classname = make.Literal(pos,
            primitives.getNameForClassForName(elemtp))
            .setType(definitions.JAVA_STRING_TYPE);
        Tree array = gen.Apply(pos,
            gen.mkRef(pos, primitives.RUNTIME_TYPE, primitives.NEW_OARRAY),
            new Tree[] {size, classname});
        Tree cast = gen.TypeApply(pos, gen.Select(pos, array, definitions.AS),
            new Tree[] {gen.mkType(pos, Type.UnboxedArrayType(elemtp))});
        return gen.Apply(cast, new Tree[0]);
    }

    private Tree genNewArray(int pos, Tree size, int kind) {
        return gen.Apply(pos,
            gen.mkRef(pos,
                primitives.RUNTIME_TYPE, primitives.getNewArraySymbol(kind)),
            new Tree[] {size});
    }
}
