/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.ast;

import java.io.*;
import java.util.*;
import scalac.*;
import scalac.symtab.*;
import scalac.typechecker.Infer;
import scalac.util.*;
import Tree.*;

/** A helper class for building trees
 *
 *  @author     Martin Odersky, Christine Roeckl
 *  @version    1.0
 */
public class TreeGen implements Kinds, Modifiers, TypeTags {

    /********************************************************************************/
    /********************************************************************************/
    /** VARIABLES **/

    /** the global environment
     */
    protected final Global global;

    /** the global definitions
     */
    protected final Definitions definitions;

    /** the tree factory
     */
    public final TreeFactory make;

    /** the type inferencer
     */
    final Infer infer;

    /************************************************************************/
    /************************************************************************/
    /** CONSTRUCTORS **/

    public TreeGen(Global global, TreeFactory make) {
        this.global = global;
	this.definitions = global.definitions;
        this.make = make;
	this.infer = new Infer(global, this, make);
    }

    public TreeGen(Global global) {
	this(global, global.make);
    }

    /*************************************************************************/
    /*************************************************************************/
    /** METHODS **/

    public Type deref(Type tp) {
	switch (tp) {
	case PolyType(Symbol[] tparams, Type restp):
	    if (tparams.length == 0) return restp;
	}
	return tp;
    }

    /** Create a dummy symbol to be used for templates.
     */
    public Symbol localDummy(int pos, Symbol owner) {
	return new TermSymbol(pos, Names.LOCAL(owner), owner, 0)
	    .setInfo(Type.NoType);
    }

    public Tree mkStable(Tree tree) {
	Symbol sym = tree.symbol();
	if (sym.isStable()) {
	    switch (tree) {
	    case Ident(_):
		tree.setType(Type.singleType(sym.owner().thisType(), sym));
		break;
	    case Select(Tree qual, _):
		if (qual.type.isStable())
		    tree.setType(Type.singleType(qual.type, sym));
	    }
	}
	return tree;
    }

    public Tree mkRef(int pos, Type pre, Symbol sym) {
	if (pre.isSameAs(Type.localThisType) || pre.symbol().isRoot())
	    return Ident(pos, sym);
	else
	    return Select(pos, mkStableId(pos, pre), sym);
    }

    public Tree mkRef(int pos, Symbol sym) {
	return mkRef(pos, sym.owner().thisType(), sym);
    }

    /** Build and attribute stable identifier tree corresponding to given prefix.
     */
    public Tree mkStableId(int pos, Type pre) {
        switch (pre.expandModuleThis()) {
	case ThisType(Symbol sym):
	    return This(pos, sym);
        case SingleType(Type pre1, Symbol sym):
	    return mkStable(mkRef(pos, pre1, sym));
        default:
            throw new ApplicationError(pre);
        }
    }

    /** Build and attribute tree corresponding to given type.
     */
    public Tree mkType(int pos, Type type) {
	return TypeTerm(pos, type);
    }

    /** Build and attribute tree array corresponding to given type array.
     */
    public Tree[] mkTypes(int pos, Type[] types) {
        Tree[] res = new Tree[types.length];
        for (int i = 0; i < types.length; i++) {
          res[i] = mkType(pos, types[i]);
        }
        return res;
    }

    /** Build and attribute tree corresponding to given type.
     */
    public Tree TypeTerm(int pos, Type type) {
	return make.TypeTerm(pos).setType(type);
    }

    /** Build and attribute tree corresponding to symbol's declaration.
     */
    public Tree mkDef(int pos, Symbol sym) {
	switch (sym.kind) {
	case ERROR:
	    return make.Bad(pos, Symbol.ERROR).setType(Type.ErrorType);
	case TYPE:
	    return AbsTypeDef(pos, sym);
	case ALIAS:
	    return AliasTypeDef(pos, sym);
	case VAL:
	    if (sym.isMethod()) return DefDef(pos, sym, Tree.Empty);
	    else return ValDef(pos, sym, Tree.Empty);
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

    /** Build a boolean constant tree.
     */
    public Tree mkBooleanLit(int pos, boolean bool) {
        return make.Literal(pos, bool ? Boolean.TRUE : Boolean.FALSE).
            setType(definitions.BOOLEAN_TYPE);
    }

    /** Build a string literal
     */
    public Tree mkStringLit(int pos, String str) {
        return make.Literal(pos, str).setType(definitions.JAVA_STRING_TYPE);
    }

    /** Build an integer literal
     */
    public Tree mkIntLit(int pos, int value) {
        return make.Literal(pos, new Integer(value)).setType(definitions.INT_TYPE);
    }

    /** Build a default zero value according to type
     */
    public Tree mkDefaultValue(int pos, Type tp) {
	if (tp.isSubType(definitions.ANYREF_TYPE)) {
	    return Ident(pos, definitions.NULL);
	} else {
	    switch (tp.unbox()) {
	    case UnboxedType(BOOLEAN):
		return mkBooleanLit(pos, false);
	    case UnboxedType(BYTE):
	    case UnboxedType(SHORT):
	    case UnboxedType(CHAR):
	    case UnboxedType(INT):
		return mkIntLit(pos, 0);
	    case UnboxedType(LONG):
		return make.Literal(pos, new Long(0)).setType(definitions.LONG_TYPE);
	    case UnboxedType(FLOAT):
		return make.Literal(pos, new Float(0)).setType(definitions.FLOAT_TYPE);
	    case UnboxedType(DOUBLE):
		return make.Literal(pos, new Double(0)).setType(definitions.DOUBLE_TYPE);
	    case UnboxedType(UNIT):
		return Block(pos, Tree.EMPTY_ARRAY);
	    default:
		return Ident(pos, definitions.ZERO);
	    }
	}
    }

    /** Build a tree to be used as a base class constructor for a template.
     */
    public Tree mkParentConstr(int pos, Type parentType, Tree[] parentArgs) {
	switch (parentType) {
	case TypeRef(Type pre, Symbol sym, Type[] args):
	    Tree ref = mkRef(pos, pre, sym.allConstructors());
	    switch (ref.type) {
	    case OverloadedType(Symbol[] alts, Type[] alttypes):
		infer.methodAlternative(
		    ref, alts, alttypes, Tree.typeOf(parentArgs), Type.AnyType);
	    }
	    Tree constr = (args.length == 0) ? ref
		: TypeApply(ref, mkTypes(sym.pos, args));
	    return Apply(constr, parentArgs);
	default:
	    throw global.fail("invalid parent type", parentType);
	}
    }

    public Tree mkParentConstr(int pos, Type parentType) {
        return mkParentConstr(pos, parentType, Tree.EMPTY_ARRAY);
    }

    /** Build an array of trees to be used as base classes for a template.
     */
    public Tree[] mkParentConstrs(int pos, Type[] parents, Tree[][] parentArgs) {
        Tree[] constrs = new Tree[parents.length];
        for (int i = 0; i < parents.length; ++i)
	    constrs[i] = (parentArgs.length == 0 ?
                          mkParentConstr(pos, parents[i])
                          : mkParentConstr(pos, parents[i], parentArgs[i]));
        return constrs;
    }

    public Tree[] mkParentConstrs(int pos, Type[] parents) {
        return mkParentConstrs(pos, parents, new Tree[][]{});
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
	    res[i] = mkParam(pos, symbols[i]);
        }
	return res;
    }

    /** Build parameter corresponding to given symbol .
     */
    public ValDef mkParam(int pos, Symbol sym) {
	return ValDef(pos, sym, Tree.Empty);
    }

    public ValDef mkParam(Symbol sym) {
	return mkParam(sym.pos, sym);
    }

    /** Build type parameter section corresponding to given array of symbols .
     */
    public AbsTypeDef[] mkTypeParams(int pos, Symbol[] symbols) {
        AbsTypeDef[] res = new AbsTypeDef[symbols.length];
        for (int i = 0; i < symbols.length; i++) {
	    res[i] = mkTypeParam(pos, symbols[i]);
        }
        return res;
    }

    /** Build type parameter corresponding to given symbol .
     */
    public AbsTypeDef mkTypeParam(int pos, Symbol sym) {
	return AbsTypeDef(pos, sym);
    }

    public AbsTypeDef mkTypeParam(Symbol sym) {
	return mkTypeParam(sym.pos, sym);
    }

    /** Build abstract type definition corresponding to given symbol .
     */
    public AbsTypeDef AbsTypeDef(int pos, Symbol sym) {
	Global.instance.nextPhase();
	Type symtype = sym.info();
	Global.instance.prevPhase();
	AbsTypeDef res = make.AbsTypeDef(
	    pos, sym, TypeTerm(pos, symtype), TypeTerm(pos, sym.loBound()));
        res.setType(definitions.UNIT_TYPE);
        return res;
    }

    public AbsTypeDef AbsTypeDef(Symbol sym) {
	return AbsTypeDef(sym.pos, sym);
    }

    /** Build type definition corresponding to given symbol .
     */
    public AliasTypeDef AliasTypeDef(int pos, Symbol sym) {
	Global.instance.nextPhase();
	Type symtype = sym.info();
	Global.instance.prevPhase();
	AliasTypeDef res = make.AliasTypeDef(
	    pos,
	    sym,
            mkTypeParams(pos, sym.typeParams()),
	    TypeTerm(pos, symtype));
        res.setType(definitions.UNIT_TYPE);
        return res;
    }

    public AliasTypeDef AliasTypeDef(Symbol sym) {
	return AliasTypeDef(sym.pos, sym);
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
	return make.Typed(tree.pos, tree, TypeTerm(tree.pos, tp)).setType(tp);
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
    public Tree New(int pos, Tree constr) {
        Symbol local = localDummy(pos, Symbol.NONE);
	Template templ = make.Template(
            pos, local, new Tree[]{constr}, Tree.EMPTY_ARRAY);
	templ.setType(constr.type);
	return make.New(pos, templ).setType(constr.type);
    }

    public Tree New(Tree constr) {
        return New(constr.pos, constr);
    }


    /** Build an allocation   new P.C[TARGS](ARGS)
     *  given a (singleton) type P, class C, type arguments TARGS and arguments ARGS
     */
    public Tree New(int pos, Type pre, Symbol clazz,
		    Type[] targs, Tree[] args) {
	Tree constr = mkRef(pos, pre, clazz.primaryConstructor());
	if (targs.length != 0)
	    constr = TypeApply(constr, mkTypes(pos, targs));
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
 	try {
	    switch (fn.type) {
	    case Type.OverloadedType(Symbol[] alts, Type[] alttypes):
		infer.methodAlternative(fn, alts, alttypes,
					Tree.typeOf(args), Type.AnyType);
	    }
	    switch (fn.type) {
	    case Type.MethodType(Symbol[] vparams, Type restpe):
		return make.Apply(pos, fn, args).setType(restpe);
	    }
	} catch (Type.Error ex) {
	}
	throw new ApplicationError("method type required", fn.type);
    }

    public Tree Apply(Tree fn, Tree[] args) {
      return Apply(fn.pos, fn, args);
    }

    /** Build and attribute type application node with given function
     *  and argument trees.
     */
    public Tree TypeApply(int pos, Tree fn, Tree[] args) {
	try {
	    switch (fn.type) {
	    case Type.OverloadedType(Symbol[] alts, Type[] alttypes):
		infer.polyAlternative(fn, alts, alttypes, args.length);
	    }
	    switch (fn.type) {
	    case Type.PolyType(Symbol[] tparams, Type restpe):
		return make.TypeApply(pos, fn, args)
		    .setType(restpe.subst(tparams, Tree.typeOf(args)));
	    }
	} catch (Type.Error ex) {
	}
	throw new ApplicationError("poly type required", fn.type);
    }

    public Tree TypeApply(Tree fn, Tree[] args) {
      return TypeApply(fn.pos, fn, args);
    }

    public Tree If(int pos, Tree cond, Tree thenpart, Tree elsepart) {
	return
	    make.If(pos, cond, thenpart, elsepart).setType(thenpart.type);
    }

    public Tree If(Tree cond, Tree thenpart, Tree elsepart) {
	return If(cond.pos, cond, thenpart, elsepart);
    }

    /** Build and applied type node with given function
     *  and argument trees.
    public Tree AppliedType(int pos, Tree fn, Tree[] args) {
	return make.AppliedType(pos, fn, args)
	    .setType(Type.appliedType(fn.type, Tree.typeOf(args)));
    }

    public Tree AppliedType(Tree fn, Tree[] args) {
	return AppliedType(fn.pos, fn, args);
    }
     */

    /** Build and attribute select node of given symbol.
     *  It is assumed that the prefix is not empty.
     */
    public Tree Select(int pos, Tree qual, Symbol sym) {
	assert sym.kind != NONE;
	Global.instance.nextPhase();
	Type symtype = qual.type.memberType(sym);
	Global.instance.prevPhase();
	sym.flags |= ACCESSED | SELECTOR;
	return make.Select(pos, sym, qual).setType(deref(symtype));
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
	sym.flags |= ACCESSED;
	return make.Ident(pos, sym).setType(deref(symtype));
    }

    public Tree Ident(Symbol sym) {
        return Ident(sym.pos, sym);
    }

    /** Build and attribute this node with given symbol.
     */
    public Tree This(int pos, Symbol sym) {
        return make.This(pos, sym).setType(sym.thisType());
    }

    /** Build and attribute super node with given type.
     */
    public Tree Super(int pos, Symbol sym) {
        return make.Super(pos, sym, TypeNames.EMPTY).setType(sym.thisType());
    }

    /** Build and attribute value/variable/let definition node whose signature
     *  corresponds to given symbol and which has given rhs.
     */
    public ValDef ValDef(int pos, Symbol sym, Tree rhs) {
	Global.instance.nextPhase();
	Type symtype = sym.type();
	Global.instance.prevPhase();
	ValDef res = make.ValDef(pos, sym, TypeTerm(pos, symtype), rhs);
        res.setType(definitions.UNIT_TYPE);
        return res;
    }

    public ValDef ValDef(Symbol sym, Tree rhs) {
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
                           sym,
                           mkTypeParams(pos, symtype.typeParams()),
                           mkParams(pos, symtype),
                           TypeTerm(pos, symtype.resultType()),
                           body)
            .setType(definitions.UNIT_TYPE);
    }

    public Tree DefDef(Symbol sym, Tree rhs) {
	return DefDef(sym.pos, sym, rhs);
    }

    /** Generate class definition from class symbol, and template.
     */
    public Tree ClassDef(int pos, Symbol clazz, Template template) {
	Global.instance.nextPhase();
	Type constrtype = clazz.primaryConstructor().info();
	Global.instance.prevPhase();
        return make.ClassDef(
            pos,
            clazz,
            mkTypeParams(pos, constrtype.typeParams()),
            mkParams(pos, constrtype),
            Tree.Empty,
            template)
            .setType(definitions.UNIT_TYPE);
    }

    public Tree ClassDef(Symbol clazz, Template template) {
        return ClassDef(clazz.pos, clazz, template);
    }

    /** Generate class definition from class symbol, parent constructors, and body.
     */
    public Tree ClassDef(int pos, Symbol clazz, Tree[] constrs, Symbol local, Tree[] body) {
	Global.instance.nextPhase();
	Type clazzinfo = clazz.info();
	Global.instance.prevPhase();
	switch (clazzinfo) {
	case CompoundType(Type[] parents, Scope members):
	    Template templ = make.Template(pos, local, constrs, body);
	    templ.setType(clazzinfo);
	    return ClassDef(pos, clazz, templ);
	default:
	    throw new ApplicationError();
	}
    }

    public Tree ClassDef(int pos, Symbol clazz, Tree[] constrs, Tree[] body) {
	return ClassDef(pos, clazz, constrs, localDummy(pos, clazz), body);
    }

    public Tree ClassDef(Symbol clazz, Tree[] constrs, Symbol local, Tree[] body) {
	return ClassDef(clazz.pos, clazz, constrs, local, body);
    }

    public Tree ClassDef(Symbol clazz, Tree[] constrs, Tree[] body) {
	return ClassDef(clazz.pos, clazz, constrs, body);
    }



    /** Generate class definition from class symbol and body.
     *  All parents must by parameterless, or take unit parameters.
     */
    public Tree ClassDef(int pos, Symbol clazz, Symbol local, Tree[] body) {
	Global.instance.nextPhase();
	Type clazzinfo = clazz.info();
	Global.instance.prevPhase();
	return ClassDef(pos, clazz, mkParentConstrs(pos, clazzinfo.parents()), local, body);
    }

    public Tree ClassDef(int pos, Symbol clazz, Tree[] body) {
	return ClassDef(pos, clazz, localDummy(pos, clazz), body);
    }

    public Tree ClassDef(Symbol clazz, Symbol local, Tree[] body) {
	return ClassDef(clazz.pos, clazz, local, body);
    }

    public Tree ClassDef(Symbol clazz, Tree[] body) {
	return ClassDef(clazz.pos, clazz, body);
    }

    /** Build the expansion of (() => expr)
     */
    public Tree mkUnitFunction(Tree expr, Type tp, Symbol owner) {
	return mkFunction(expr.pos, Tree.ValDef_EMPTY_ARRAY, expr, tp, owner);
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
	Type ft = definitions.functionType(argtypes, restype);

	ClassSymbol clazz = new ClassSymbol(
	    pos, Names.ANON_CLASS_NAME.toTypeName(), owner, 0);
	clazz.setInfo(Type.compoundType(new Type[]{definitions.OBJECT_TYPE, ft},
                                        new Scope(), clazz));
	clazz.allConstructors().setInfo(
	    Type.MethodType(Symbol.EMPTY_ARRAY, clazz.typeConstructor()));

	Symbol applyMeth = new TermSymbol(pos, Names.apply, clazz, FINAL)
	    .setInfo(Type.MethodType(params, restype));
	clazz.info().members().enter(applyMeth);

	for (int i = 0; i < params.length; i++) {
	    params[i].setOwner(applyMeth);
	}
	changeOwner(body, owner, applyMeth);
	Tree applyDef = DefDef(applyMeth, body);
	Tree classDef = ClassDef(clazz, new Tree[]{applyDef});
	Tree alloc = New(pos, Type.localThisType, clazz, Tree.EMPTY_ARRAY)
	    .setType(ft);
	return Block(new Tree[]{classDef, alloc});
    }


    public Tree mkPartialFunction(int pos, Tree applyVisitor, Tree isDefinedAtVisitor,
				  Type pattype, Type restype, Symbol owner) {
	Type pft = definitions.partialFunctionType(pattype, restype);
	ClassSymbol clazz = new ClassSymbol(
	    pos, Names.ANON_CLASS_NAME.toTypeName(), owner, 0);
	clazz.setInfo(Type.compoundType(new Type[]{definitions.OBJECT_TYPE, pft},
                                        new Scope(), clazz));
	clazz.allConstructors().setInfo(
	    Type.MethodType(Symbol.EMPTY_ARRAY, clazz.typeConstructor()));

	Tree classDef = ClassDef(clazz, new Tree[]{
	    makeVisitorMethod(pos, Names.apply, applyVisitor,
			      pattype, restype, clazz, owner),
	    makeVisitorMethod(pos, Names.isDefinedAt, isDefinedAtVisitor,
			      pattype, definitions.BOOLEAN_TYPE, clazz, owner)});
	Tree alloc = New(pos, Type.localThisType, clazz, Tree.EMPTY_ARRAY)
	    .setType(pft);
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
		Apply(
		    TypeApply(
			Select(Ident(param), definitions.MATCH),
			new Tree[]{mkType(pos, pattype), mkType(pos, restype)}),
		    new Tree[]{visitor})
		.setType(restype);
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
	    return Apply(Select(fn, Names.apply), new Tree[]{obj});
	} else {
	    Name tmpname = global.freshNameCreator.newName("tmp", '$');
	    Symbol tmp = new TermSymbol(
		obj.pos, tmpname, owner, SYNTHETIC | FINAL)
		.setInfo(obj.type);
	    Tree tmpdef = ValDef(tmp, obj);
	    Tree expr = postfixApply(Ident(tmp), fn, owner);
	    return Block(new Tree[]{tmpdef, expr});
	}
    }
}
