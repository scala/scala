/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.ast;

import scalac.util.Name;
import Tree.*;

public class LazyTreeFactory extends AbstractTreeCopyFactory {
    protected final TreeFactory make;

    public LazyTreeFactory(TreeFactory make) {
        this.make = make;
    }

    public Tree Bad(Tree tree) {
        return tree;
    }

    public Tree ClassDef(Tree tree,
                         int mods,
                         Name name,
                         TypeDef[] tparams,
                         ValDef[][] vparams,
			 Tree tpe,
                         Template impl) {
        ClassDef t = (ClassDef)tree;
        if ((t.mods == mods) &&
            (t.name == name) &&
            (t.tparams == tparams) &&
            (t.vparams == vparams) &&
	    (t.tpe == tpe) &&
            (t.impl == impl))
            return t;
        tree = make.ClassDef(t.pos, mods, name, tparams, vparams, tpe, impl);
        attribute(tree, t);
        return tree;
    }

    public Tree PackageDef(Tree tree,
                           Tree packaged,
                           Template impl) {
        PackageDef t = (PackageDef)tree;
        if ((t.packaged == packaged) &&
            (t.impl == impl))
            return t;
        tree = make.PackageDef(t.pos, packaged, impl);
        attribute(tree, t);
        return tree;
    }

    public Tree ModuleDef(Tree tree,
                          int mods,
                          Name name,
                          Tree tpe,
                          Template impl) {
        ModuleDef t = (ModuleDef)tree;
        if ((t.mods == mods) &&
            (t.name == name) &&
            (t.tpe == tpe) &&
            (t.impl == impl))
            return t;
        tree = make.ModuleDef(t.pos, mods, name, tpe, impl);
        attribute(tree, t);
        return tree;
    }

    public Tree ValDef(Tree tree,
                       int mods,
                       Name name,
                       Tree tpe,
                       Tree rhs) {
        ValDef t = (ValDef)tree;
        if ((t.mods == mods) &&
            (t.name == name) &&
            (t.tpe == tpe) &&
            (t.rhs == rhs))
            return t;
        tree = make.ValDef(t.pos, mods, name, tpe, rhs);
        attribute(tree, t);
        return tree;
    }

    public Tree PatDef(Tree tree,
                       int mods,
                       Tree pat,
                       Tree rhs) {
        PatDef t = (PatDef)tree;
        if ((t.mods == mods) &&
            (t.pat == pat) &&
            (t.rhs == rhs))
            return t;
        tree = make.PatDef(t.pos, mods, pat, rhs);
        attribute(tree, t);
        return tree;
    }

    public Tree DefDef(Tree tree,
                       int mods,
                       Name name,
                       TypeDef[] tparams,
                       ValDef[][] vparams,
                       Tree tpe,
                       Tree rhs) {
        DefDef t = (DefDef)tree;
        if ((t.mods == mods) &&
            (t.name == name) &&
            (t.tparams == tparams) &&
            (t.vparams == vparams) &&
            (t.tpe == tpe) &&
            (t.rhs == rhs))
            return t;
        tree = make.DefDef(t.pos, mods, name, tparams, vparams, tpe, rhs);
        attribute(tree, t);
        return tree;
    }

    public Tree TypeDef(Tree tree,
                        int mods,
                        Name name,
			TypeDef[] tparams,
                        Tree rhs) {
        TypeDef t = (TypeDef)tree;
        if ((t.mods == mods) &&
            (t.name == name) &&
	    (t.tparams == tparams) &&
            (t.rhs == rhs))
            return t;
        tree = make.TypeDef(t.pos, mods, name, tparams, rhs);
        attribute(tree, t);
        return tree;
    }

    public Tree Import(Tree tree,
                       Tree expr,
		       Name[] selectors) {
        Import t = (Import)tree;
        if (t.expr == expr && t.selectors == selectors)
            return t;
        tree = make.Import(t.pos, expr, selectors);
        attribute(tree, t);
        return tree;
    }

    public Tree CaseDef(Tree tree,
                        Tree pat,
                        Tree guard,
                        Tree body) {
        CaseDef t = (CaseDef)tree;
        if ((t.pat == pat) &&
            (t.guard == guard) &&
            (t.body == body))
            return t;
        tree = make.CaseDef(t.pos, pat, guard, body);
        attribute(tree, t);
        return tree;
    }

    public Template Template(Tree tree,
                             Tree[] parents,
                             Tree[] body) {
        Template t = (Template)tree;
        if ((t.parents == parents) &&
            (t.body == body))
            return t;
        Template newTree = make.Template(t.pos, parents, body);
        attribute(newTree, t);
        return newTree;
    }

    public Tree LabelDef(Tree tree,
			 Tree[] params,
			 Tree rhs) {
	LabelDef t = (LabelDef)tree;
	if ((t.params == params) &&
	    (t.rhs == rhs))
	    return t;
	tree = make.LabelDef(t.pos,params,rhs);
	attribute(tree,t);
	return tree;
    }

    public Tree Block(Tree tree,
                      Tree[] stats) {
        Block t = (Block)tree;
        if (t.stats == stats)
            return t;
        tree = make.Block(t.pos, stats);
        attribute(tree, t);
        return tree;
    }

    public Tree Tuple(Tree tree,
                      Tree[] trees) {
        Tuple t = (Tuple)tree;
        if (t.trees == trees)
            return t;
        tree = make.Tuple(t.pos, trees);
        attribute(tree, t);
        return tree;
    }

    public Tree Visitor(Tree tree,
                        CaseDef[] cases) {
        Visitor t = (Visitor)tree;
        if (t.cases == cases)
            return t;
        tree = make.Visitor(t.pos, cases);
        attribute(tree, t);
        return tree;
    }

    public Tree Function(Tree tree,
                         ValDef[] vparams,
                         Tree body) {
        Function t = (Function)tree;
        if ((t.vparams == vparams) &&
            (t.body == body))
            return t;
        tree = make.Function(t.pos, vparams, body);
        attribute(tree, t);
        return tree;
    }

    public Tree Assign(Tree tree,
                       Tree lhs,
                       Tree rhs) {
        Assign t = (Assign)tree;
        if ((t.lhs == lhs) &&
            (t.rhs == rhs))
            return t;
        tree = make.Assign(t.pos, lhs, rhs);
        attribute(tree, t);
        return tree;
    }

    public Tree If(Tree tree,
                   Tree cond,
                   Tree thenp,
                   Tree elsep) {
        If t = (If)tree;
        if ((t.cond == cond) &&
            (t.thenp == thenp) &&
            (t.elsep == elsep))
            return t;
        tree = make.If(t.pos, cond, thenp, elsep);
        attribute(tree, t);
        return tree;
    }

    public Tree New(Tree tree,
                    Template templ) {
        New t = (New)tree;
        if (t.templ == templ)
            return t;
        tree = make.New(t.pos, templ);
        attribute(tree, t);
        return tree;
    }

    public Tree Typed(Tree tree,
                      Tree expr,
                      Tree tpe) {
        Typed t = (Typed)tree;
        if ((t.expr == expr) &&
            (t.tpe == tpe))
            return t;
        tree = make.Typed(t.pos, expr, tpe);
        attribute(tree, t);
        return tree;
    }

    public Tree TypeApply(Tree tree,
                          Tree fun,
                          Tree[] args) {
        TypeApply t = (TypeApply)tree;
        if ((t.fun == fun) &&
            (t.args == args))
            return t;
        tree = make.TypeApply(t.pos, fun, args);
        attribute(tree, t);
        return tree;
    }

    public Tree Apply(Tree tree,
                      Tree fun,
                      Tree[] args) {
        Apply t = (Apply)tree;
        if ((t.fun == fun) &&
            (t.args == args))
            return t;
        tree = make.Apply(t.pos, fun, args);
        attribute(tree, t);
        return tree;
    }

    public Tree Super(Tree tree,
                      Tree tpe) {
        Super t = (Super)tree;
        if (t.tpe == tpe)
            return t;
        tree = make.Super(t.pos, tpe);
        attribute(tree, t);
        return tree;
    }

    public Tree This(Tree tree,
		     Tree qualifier) {
	This t = (This)tree;
	if (t.qualifier == qualifier)
	    return t;
	tree = make.This(t.pos, qualifier);
	attribute(tree, t);
	return tree;
    }

    public Tree Select(Tree tree,
                       Tree qualifier,
                       Name selector) {
        Select t = (Select)tree;
        if ((t.qualifier == qualifier) &&
            (t.selector == selector))
            return t;
        tree = make.Select(t.pos, qualifier, selector);
        attribute(tree, t);
        return tree;
    }

    public Tree Ident(Tree tree,
                      Name name) {
        Ident t = (Ident)tree;
        if (t.name == name)
            return t;
        tree = make.Ident(t.pos, name);
        attribute(tree, t);
        return tree;
    }

    public Tree Literal(Tree tree,
                        Object value) {
        Literal t = (Literal)tree;
        if (t.value == value)
            return t;
        tree = make.Literal(t.pos, value);
        attribute(tree, t);
        return tree;
    }

    public Tree SingletonType(Tree tree,
			      Tree ref) {
	SingletonType t = (SingletonType)tree;
	if (t.ref == ref)
	    return t;
	tree = make.SingletonType(t.pos, ref);
	attribute(tree, t);
	return tree;
    }

    public Tree SelectFromType(Tree tree,
			       Tree qualifier,
			       Name selector) {
	SelectFromType t = (SelectFromType)tree;
	if (t.qualifier == qualifier &&
	    t.selector == selector)
	    return t;
	tree = make.SelectFromType(t.pos, qualifier, selector);
	attribute(tree, t);
	return tree;
    }

    public Tree FunType(Tree tree,
                        Tree[] argtpes,
                        Tree restpe) {
        FunType t = (FunType)tree;
        if ((t.argtpes == argtpes) &&
            (t.restpe == restpe))
            return t;
        tree = make.FunType(t.pos, argtpes, restpe);
        attribute(tree, t);
        return tree;
    }

    public Tree CompoundType(Tree tree,
                             Tree[] parents,
                             Tree[] refinements) {
        CompoundType t = (CompoundType)tree;
        if ((t.parents == parents) &&
            (t.refinements == refinements))
            return t;
        tree = make.CompoundType(t.pos, parents, refinements);
        attribute(tree, t);
        return tree;
    }

    public Tree AppliedType(Tree tree,
                            Tree tpe,
                            Tree[] args) {
        AppliedType t = (AppliedType)tree;
        if ((t.tpe == tpe) &&
            (t.args == args))
            return t;
        tree = make.AppliedType(t.pos, tpe, args);
        attribute(tree, t);
        return tree;
    }

    public Tree CovariantType(Tree tree,
			      Tree tpe) {
	CovariantType t = (CovariantType)tree;
	if (t.tpe == tpe) return t;
	tree = make.CovariantType(t.pos, tpe);
	attribute(tree, t);
	return tree;
    }
}
