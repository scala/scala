/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.ast;

import java.io.*;
import java.util.*;
import scalac.*;
import scalac.symtab.*;
import scalac.util.*;
import Tree.*;

/** A helper class for building trees
 *
 *  @author     Martin Odersky, Christine Roeckl
 *  @version    1.0
 */
public class TreeGen implements Kinds, Modifiers {

    /********************************************************************************/
    /********************************************************************************/
    /** VARIABLES **/

    /** the global environment
     */
    protected Global global;

    /** the global definitions
     */
    protected Definitions definitions;

    /** the tree factory
     */
    public TreeFactory make;

    /************************************************************************/
    /************************************************************************/
    /** CONSTRUCTORS **/

    public TreeGen(Global global, TreeFactory make) {
        this.global = global;
	this.definitions = global.definitions;
        this.make = make;
    }

    public TreeGen(Global global) {
	this(global, global.make);
    }

    /*************************************************************************/
    /*************************************************************************/
    /** METHODS **/

    /** Create a dummy symbol to be used for templates.
     */
    public Symbol localDummy(int pos, Symbol owner) {
	return new TermSymbol(pos, Name.EMPTY, owner, 0)
	    .setInfo(Type.NoType);
    }

    public Tree mkRef(int pos, Type pre, Symbol sym) {
	if (pre.isSameAs(Type.localThisType) || pre.symbol().isRoot())
	    return Ident(pos, sym);
	else
	    return Select(pos, mkStableId(pos, pre), sym);
    }

    /** Build and attribute stable identifier tree corresponding to given prefix.
     */
    public Tree mkStableId(int pos, Type pre) {
        switch (pre.expandModuleThis()) {
	case ThisType(Symbol sym):
	    return make.This(pos, Ident(pos, sym)).setType(pre);
        case SingleType(Type pre1, Symbol sym):
	    return mkRef(pos, pre1, sym);
        default:
            throw new ApplicationError();
        }
    }

    /** Build and attribute tree corresponding to given type.
     */
    public Tree mkType(int pos, Type type) {
	Tree tree = mkTycon(pos, type);
	switch (type) {
	case TypeRef(Type pre, Symbol sym, Type[] args):
	    if (args.length != 0)
		return make.AppliedType(pos, tree, mkType(pos, args))
		    .setType(type);
	}
	return tree;
    }

    /** Build and attribute tree corresponding to given type constructor.
     */
    public Tree mkTycon(int pos, Type type) {
	//System.out.println("making type " + type);//DEBUG
        switch (type) {

        case NoType:
	    return Tree.Empty;

	case ErrorType:
	case AnyType:
	    return make.Bad(pos).setSymbol(Symbol.ERROR).setType(type);

	case ThisType(_):
	case SingleType(_, _):
	    return make.SingletonType(pos, mkStableId(pos, type)).setType(type);

        case TypeRef(Type pre, Symbol sym, Type[] args):
	    return mkRef(pos, pre, sym);

	case CompoundType(Type[] parents, Scope members):
	    if (parents.length == 1 && members.elems == Scope.Entry.NONE)
		return mkType(pos, parents[0]);
	    else
		return make.CompoundType(
		    pos, mkType(pos, parents), mkDefs(pos, members.elements()))
		    .setType(type);

	case CovarType(Type tp):
	    return make.CovariantType(pos, mkType(pos, tp))
		.setType(type);

	case UnboxedType(_):
	case UnboxedArrayType(_):
	    return make.Ident(pos, Name.fromString(type.toString()).toTypeName())
		.setType(type);

	default:
	    throw new ApplicationError("illegal type", type);
        }
    }

    /** Build and attribute tree array corresponding to given type array.
     */
    public Tree[] mkType(int pos, Type[] types) {
        Tree[] res = new Tree[types.length];
        for (int i = 0; i < types.length; i++) {
	    res[i] = mkType(pos, types[i]);
        }
        return res;
    }

    /** Build and attribute tree corresponding to symbol's declaration.
     */
    public Tree mkDef(int pos, Symbol sym) {
	switch (sym.kind) {
	case ERROR:
	    return make.Bad(pos).setSymbol(Symbol.ERROR).setType(Type.ErrorType);
	case TYPE: case ALIAS:
	    return TypeDef(pos, sym);
	case VAL:
	    if (sym.isMethod()) return DefDef(pos, sym, Tree.Empty);
	    else return Param(pos, sym);
	default:
	    throw new ApplicationError();
	}
    }

    /** Build and attribute tree array corresponding to given symbol's declarations.
     */
    public Tree[] mkDefs(int pos, Symbol[] syms) {
        Tree[] res = new Tree[syms.length];
        for (int i = 0; i < syms.length; i++) {
	    res[i] = mkDef(pos, syms[i]);
        }
        return res;
    }

    /** Build a tree to be used as a base class constructor for a template.
     */
    public Tree mkParentConstr(int pos, Type parentType) {
	switch (parentType) {
	case TypeRef(Type pre, Symbol sym, Type[] args):
	    Tree ref = mkRef(pos, pre, sym.constructor());
	    Tree constr = (args.length == 0) ? ref
		: TypeApply(ref, mkType(sym.pos, args));
	    switch (parentType) {
	    case MethodType(Symbol[] params, Type restpe):
		assert params.length == 0 : parentType;
		return Apply(constr, Tree.EMPTY_ARRAY);
	    default:
		return constr;
	    }
	default:
	    throw global.fail("invalid parent type", parentType);
	}
    }

    /** Build an array of trees to be used as base classes for a template.
     */
    public Tree[] mkParentConstrs(int pos, Type[] parents) {
        Tree[] constrs = new Tree[parents.length];
        for (int i = 0; i < parents.length; ++i)
	    constrs[i] = mkParentConstr(pos, parents[i]);
        return constrs;
    }

    /** Build parameter sections corresponding to type.
     */
    public ValDef[][] mkParams(int pos, Type type) {
	switch (type) {
        case PolyType(Symbol[] tparams, Type restype):
	    return mkParams(pos, restype);
        case MethodType(Symbol[] vparams, Type restype):
             ValDef[] params1 = mkParams(pos, vparams);
	     ValDef[][] paramss = mkParams(pos, restype);
	     if (paramss.length == 0) {
		 return new ValDef[][]{params1};
	     } else {
		 ValDef[][] paramss1 = new ValDef[paramss.length + 1][];
		 paramss1[0] = params1;
		 System.arraycopy(paramss, 0, paramss1, 1, paramss.length);
		 return paramss1;
	     }
        default:
             return new ValDef[][]{};
        }
    }

    /** Build parameter section corresponding to given array of symbols .
     */
    public ValDef[] mkParams(int pos, Symbol[] symbols) {
        ValDef[] res = new ValDef[symbols.length];
        for (int i = 0; i < symbols.length; i++) {
	    res[i] = Param(pos, symbols[i]);
        }
	return res;
    }

    /** Build type parameter section corresponding to given array of symbols .
     */
    public TypeDef[] mkTypeParams(int pos, Symbol[] symbols) {
        TypeDef[] res = new TypeDef[symbols.length];
        for (int i = 0; i < symbols.length; i++) {
	    res[i] = (TypeDef)TypeDef(pos, symbols[i]);
        }
        return res;
    }

    /** Build type definition corresponding to given symbol .
     */
    public TypeDef TypeDef(int pos, Symbol sym) {
	Global.instance.nextPhase();
	Type symtype = sym.info();
	Global.instance.prevPhase();
	return (TypeDef) make.TypeDef(
	    pos,
	    sym.flags & SOURCEFLAGS,
	    sym.name,
	    mkTypeParams(pos, sym.typeParams()),
	    mkType(pos, symtype))
	    .setSymbol(sym).setType(definitions.UNIT_TYPE);
    }

    public Tree TypeDef(Symbol sym) {
	return TypeDef(sym.pos, sym);
    }

    /** Build parameter
     */
    public ValDef Param(int pos, Symbol sym) {
        global.log("use of obsolete Param method in TreeGen");
	return (ValDef)ValDef(pos, sym, Tree.Empty);
    }

    public ValDef Param(Symbol sym) {
        global.log("use of obsolete Param method in TreeGen");
	return Param(sym.pos, sym);
    }

    public ValDef ValDef(int pos, Symbol sym) {
	return (ValDef)ValDef(pos, sym, Tree.Empty);
    }

    public ValDef ValDef(Symbol sym) {
	return Param(sym.pos, sym);
    }

    /** Build and attribute block with given statements, starting
     *  at given position. The type is the type of the last
     *  statement in the block.
     */
    public Tree Block(int pos, Tree[] stats) {
	Type tp = (stats.length == 0) ? definitions.UNIT_TYPE
	    : stats[stats.length - 1].type;
	return make.Block(pos, stats).setType(tp);
    }

    /** Build and attribute non-empty block with given statements.
     */
    public Tree Block(Tree[] stats) {
	return Block(stats[0].pos, stats);
    }

    public Tree Typed(Tree tree, Type tp) {
	return make.Typed(tree.pos, tree, mkType(tree.pos, tp)).setType(tp);
    }

    /** Build and attribute the assignment lhs = rhs
     */
    public Tree Assign(int pos, Tree lhs, Tree rhs) {
        return make.Assign(pos, lhs, rhs).setType(definitions.UNIT_TYPE);
    }

    public Tree Assign(Tree lhs, Tree rhs) {
        return Assign(lhs.pos, lhs, rhs);
    }

    /** Build and attribute new B, given constructor expression B.
     */
    public Tree New(Tree constr) {
	Template templ = make.Template(
	    constr.pos, new Tree[]{constr}, Tree.EMPTY_ARRAY);
	templ.setType(constr.type);
        templ.setSymbol(localDummy(constr.pos, Symbol.NONE));
	return make.New(constr.pos, templ).setType(constr.type);	    }

    /** Build an allocation   new P.C[TARGS](ARGS)
     *  given a (singleton) type P, class C, type arguments TARGS and arguments ARGS
     */
    public Tree New(int pos, Type pre, Symbol clazz,
		    Type[] targs, Tree[] args) {
	Tree constr = mkRef(pos, pre, clazz.constructor());
	if (targs.length != 0) constr = TypeApply(constr, mkType(pos, targs));
	Tree base = Apply(constr, args);
	return New(base);
    }

    /** Build a monomorphic allocation   new P.C(ARGS)
     *  given a prefix P, class C and arguments ARGS
     */
    public Tree New(int pos, Type pre, Symbol clazz, Tree[] args) {
	return New(pos, pre, clazz, Type.EMPTY_ARRAY, args);
    }

    /** Build and attribute application node with given function
     *  and argument trees.
     */
    public Tree Apply(int pos, Tree fn, Tree[] args) {
        switch (fn.type) {
        case Type.MethodType(Symbol[] vparams, Type restpe):
	    return make.Apply(pos, fn, args).setType(restpe);
        default:
            throw new ApplicationError("method type required", fn.type);
        }
    }

    public Tree Apply(Tree fn, Tree[] args) {
      return Apply(fn.pos, fn, args);
    }

    /** Build and attribute type application node with given function
     *  and argument trees.
     */
    public Tree TypeApply(int pos, Tree fn, Tree[] args) {
        switch (fn.type) {
        case Type.PolyType(Symbol[] tparams, Type restpe):
	    return make.TypeApply(pos, fn, args)
		.setType(restpe.subst(tparams, Tree.typeOf(args)));
        default:
            throw new ApplicationError("poly type required", fn.type);
        }
    }

    public Tree TypeApply(Tree fn, Tree[] args) {
      return TypeApply(fn.pos, fn, args);
    }

    /** Build and applied type node with given function
     *  and argument trees.
     */
    public Tree AppliedType(int pos, Tree fn, Tree[] args) {
	return make.AppliedType(pos, fn, args)
	    .setType(Type.appliedType(fn.type, Tree.typeOf(args)));
    }

    public Tree AppliedType(Tree fn, Tree[] args) {
	return AppliedType(fn.pos, fn, args);
    }

    /** Build and attribute select node of given symbol.
     *  It is assumed that the prefix is not empty.
     */
    public Tree Select(int pos, Tree qual, Symbol sym) {
	assert sym.kind != NONE;
	Global.instance.nextPhase();
	Type symtype = qual.type.memberType(sym);
	Global.instance.prevPhase();
	if (sym.kind == VAL && qual.type.isStable() && symtype.isObjectType())
	    symtype = Type.singleType(qual.type, sym);
	return make.Select(pos, qual, sym.name)
	    .setSymbol(sym).setType(symtype);
    }

    public Tree Select(Tree qual, Symbol sym) {
	return Select(qual.pos, qual, sym);
    }

    public Tree Select(Tree qual, Name name) {
	Symbol sym = qual.type.lookup(name);
	assert (sym.kind != NONE && sym != Symbol.ERROR) : name + " from " + qual.type;
	return Select(qual, sym);
    }

    /** Build and attribute ident node with given symbol.
     */
    public Tree Ident(int pos, Symbol sym) {
	Global.instance.nextPhase();
	Type symtype = sym.type();
	Global.instance.prevPhase();
	if (sym.kind == VAL && symtype.isObjectType())
	    symtype = Type.singleType(sym.owner().thisType(), sym);
	return make.Ident(pos, sym.name)
	    .setSymbol(sym).setType(symtype);
    }

    public Tree Ident(Symbol sym) {
        return Ident(sym.pos, sym);
    }

    /** Build and attribute this node with given symbol.
     */
    public Tree This(int pos, Symbol sym) {
        return make.This(pos, Ident(pos, sym)).setType(sym.thisType());
    }

    /** Build and attribute super node with given type.
     */
    public Tree Super(int pos, Type type) {
        return make.Super(pos, mkType(pos, type)).setType(type);
    }

    /** Build and attribute value/variable/let definition node whose signature
     *  corresponds to given symbol and which has given rhs.
     */
    public Tree ValDef(int pos, Symbol sym, Tree rhs) {
	Global.instance.nextPhase();
	Type symtype = sym.type();
	Global.instance.prevPhase();
	return make.ValDef(pos,
			   sym.flags & SOURCEFLAGS,
			   sym.name,
			   mkType(pos, symtype),
			   rhs)
	    .setSymbol(sym).setType(definitions.UNIT_TYPE);
    }

    public Tree ValDef(Symbol sym, Tree rhs) {
	return ValDef(sym.pos, sym, rhs);
    }

    /** Build and attribute value/variable/let definition node whose signature
     *  corresponds to given symbol and which has given body.
     */
    public Tree DefDef(int pos, Symbol sym, Tree body) {
	Global.instance.nextPhase();
	Type symtype = sym.type();
	Global.instance.prevPhase();
        return make.DefDef(pos,
                           sym.flags & SOURCEFLAGS,
                           sym.name,
                           mkTypeParams(pos, symtype.typeParams()),
                           mkParams(pos, symtype),
                           mkType(pos, symtype.resultType()),
                           body)
            .setSymbol(sym).setType(definitions.UNIT_TYPE);
    }

    public Tree DefDef(Symbol sym, Tree rhs) {
	return DefDef(sym.pos, sym, rhs);
    }

    /** Generate class definition from class symbol, parent constructors, and body.
     */
    public Tree ClassDef(int pos, Symbol clazz, Tree[] constrs, Tree[] body) {
	Global.instance.nextPhase();
	Type clazzinfo = clazz.info();
	Type constrtype = clazz.constructor().info();
	Global.instance.prevPhase();
	switch (clazzinfo) {
	case CompoundType(Type[] parents, Scope members):
	    Template templ = make.Template(pos, constrs, body);
	    templ.setType(clazzinfo);
	    templ.setSymbol(localDummy(pos, clazz.owner()));
	    return make.ClassDef(
		pos,
		clazz.flags & SOURCEFLAGS,
		clazz.name,
		mkTypeParams(pos, constrtype.typeParams()),
		mkParams(pos, constrtype),
		Tree.Empty,
		templ)
		.setSymbol(clazz).setType(definitions.UNIT_TYPE);
	default:
	    throw new ApplicationError();
	}
    }

    public Tree ClassDef(Symbol clazz, Tree[] constrs, Tree[] body) {
	return ClassDef(clazz.pos, clazz, constrs, body);
    }

    /** Generate class definition from class symbol and body.
     *  All parents must by parameterless, or take unit parameters.
     */
    public Tree ClassDef(int pos, Symbol clazz, Tree[] body) {
	Global.instance.nextPhase();
	Type clazzinfo = clazz.info();
	Global.instance.prevPhase();
	return ClassDef(pos, clazz, mkParentConstrs(pos, clazzinfo.parents()), body);
    }

    public Tree ClassDef(Symbol clazz, Tree[] body) {
	return ClassDef(clazz.pos, clazz, body);
    }

    /** Build the expansion of (() => expr)
     *  This is:
     *    { class $clazz() extends scala.Function0 { def apply() = expr } ; new $clazz() }
     */
    public Tree mkUnitFunction(Tree expr, Type tp, Symbol owner) {
	int pos = expr.pos;
	Type f0t = definitions.functionType(Type.EMPTY_ARRAY, tp);

	ClassSymbol clazz = new ClassSymbol(
	    pos, Names.ANON_CLASS_NAME.toTypeName(), owner, 0);
	clazz.setInfo(Type.compoundType(new Type[]{f0t}, new Scope(), clazz));
	clazz.constructor().setInfo(
	    Type.MethodType(
		Symbol.EMPTY_ARRAY,
		Type.TypeRef(owner.thisType(), clazz, Type.EMPTY_ARRAY)));

	Symbol applyMeth = new TermSymbol(pos, Names.apply, clazz, FINAL)
	    .setInfo(Type.MethodType(Symbol.EMPTY_ARRAY, tp));
	clazz.info().members().enter(applyMeth);

	Tree applyDef = DefDef(applyMeth, changeOwner(expr, owner, applyMeth));
	Tree classDef = ClassDef(clazz, new Tree[]{applyDef});
	Tree alloc = New(pos, Type.localThisType, clazz, Tree.EMPTY_ARRAY);
	return Block(new Tree[]{classDef, alloc});
    }

    /** Change owner of all defined symbols from `prevOwner' to `newOwner'
     */
    public Tree changeOwner(Tree tree, final Symbol prevOwner, final Symbol newOwner) {
	Transformer lifter = new Transformer(global, global.currentPhase) {
	    public Tree transform(Tree tree) {
		if (TreeInfo.isDefinition(tree)) {
		    Symbol sym = tree.symbol();
                    if (sym != null && sym.owner() == prevOwner) {
			sym.setOwner(newOwner);
			if (sym.kind == Kinds.CLASS)
                            sym.constructor().setOwner(newOwner);
                    }
		}
		return super.transform(tree);
	    }
	};
	return lifter.transform(tree);
    }
}
