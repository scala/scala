/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $OldId: StrictTreeFactory.java,v 1.6 2002/04/19 10:57:23 gamboni Exp $
// $Id$

package scalac.ast;

import scalac.util.Name;
import Tree.*;

public class StrictTreeCopier extends AbstractTreeCopier {
    protected final TreeFactory make;

    public StrictTreeCopier(TreeFactory make) {
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
        tree = make.ClassDef(t.pos, mods, name, tparams, vparams, tpe, impl);
        attribute(tree, t);
        return tree;
    }

    public Tree PackageDef(Tree tree,
                           Tree packaged,
                           Template impl) {
        PackageDef t = (PackageDef)tree;
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
        tree = make.ValDef(t.pos, mods, name, tpe, rhs);
        attribute(tree, t);
        return tree;
    }

    public Tree PatDef(Tree tree,
                       int mods,
                       Tree pat,
                       Tree rhs) {
        PatDef t = (PatDef)tree;
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
        tree = make.DefDef(t.pos, mods, name, tparams, vparams, tpe, rhs);
        attribute(tree, t);
        return tree;
    }

    public Tree TypeDef(Tree tree,
                        int mods,
                        Name name,
                        Tree rhs) {
        TypeDef t = (TypeDef)tree;
        tree = make.TypeDef(t.pos, mods, name, rhs);
        attribute(tree, t);
        return tree;
    }

    public Tree Import(Tree tree,
                       Tree expr,
                       Name[] selectors) {
        Import t = (Import)tree;
        tree = make.Import(t.pos, expr, selectors);
        attribute(tree, t);
        return tree;
    }

    public Tree CaseDef(Tree tree,
                        Tree pat,
                        Tree guard,
                        Tree body) {
        CaseDef t = (CaseDef)tree;
        tree = make.CaseDef(t.pos, pat, guard, body);
        attribute(tree, t);
        return tree;
    }

    public Template Template(Tree tree,
                             Tree[] baseClasses,
                             Tree[] body) {
        Template t = (Template)tree;
        Template newTree = make.Template(t.pos, baseClasses, body);
        attribute(newTree, t);
        return newTree;
    }

    public Tree LabelDef(Tree tree,
			 Tree[] params,
			 Tree rhs) {
	LabelDef t = (LabelDef)tree;
	tree = make.LabelDef(t.pos,params,rhs);
	attribute(tree,t);
	return tree;
    }

    public Tree Block(Tree tree,
                      Tree[] stats) {
        Block t = (Block)tree;
        tree = make.Block(t.pos, stats);
        attribute(tree, t);
        return tree;
    }

    public Tree Tuple(Tree tree,
                      Tree[] trees) {
        Tuple t = (Tuple)tree;
        tree = make.Tuple(t.pos, trees);
        attribute(tree, t);
        return tree;
    }

    public Tree Visitor(Tree tree,
                        CaseDef[] cases) {
        Visitor t = (Visitor)tree;
        tree = make.Visitor(t.pos, cases);
        attribute(tree, t);
        return tree;
    }

    public Tree Function(Tree tree,
                         ValDef[] vparams,
                         Tree body) {
        Function t = (Function)tree;
        tree = make.Function(t.pos, vparams, body);
        attribute(tree, t);
        return tree;
    }

    public Tree Assign(Tree tree,
                       Tree lhs,
                       Tree rhs) {
        Assign t = (Assign)tree;
        tree = make.Assign(t.pos, lhs, rhs);
        attribute(tree, t);
        return tree;
    }

    public Tree If(Tree tree,
                   Tree cond,
                   Tree thenp,
                   Tree elsep) {
        If t = (If)tree;
        tree = make.If(t.pos, cond, thenp, elsep);
        attribute(tree, t);
        return tree;
    }

    public Tree New(Tree tree,
                    Template templ) {
        New t = (New)tree;
        tree = make.New(t.pos, templ);
        attribute(tree, t);
        return tree;
    }

    public Tree Typed(Tree tree,
                      Tree expr,
                      Tree tpe) {
        Typed t = (Typed)tree;
        tree = make.Typed(t.pos, expr, tpe);
        attribute(tree, t);
        return tree;
    }

    public Tree TypeApply(Tree tree,
                          Tree fun,
                          Tree[] args) {
        TypeApply t = (TypeApply)tree;
        tree = make.TypeApply(t.pos, fun, args);
        attribute(tree, t);
        return tree;
    }

    public Tree Apply(Tree tree,
                      Tree fun,
                      Tree[] args) {
        Apply t = (Apply)tree;
        tree = make.Apply(t.pos, fun, args);
        attribute(tree, t);
        return tree;
    }

    public Tree This(Tree tree, Tree qualifier) {
        This t = (This)tree;
        tree = make.This(t.pos, qualifier);
        attribute(tree, t);
        return tree;
    }

    public Tree Super(Tree tree,
                      Tree tpe) {
        Super t = (Super)tree;
        tree = make.Super(t.pos, tpe);
        attribute(tree, t);
        return tree;
    }

    public Tree Select(Tree tree,
                       Tree qualifier,
                       Name selector) {
        Select t = (Select)tree;
        tree = make.Select(t.pos, qualifier, selector);
        attribute(tree, t);
        return tree;
    }

    public Tree Ident(Tree tree,
                      Name name) {
        Ident t = (Ident)tree;
        tree = make.Ident(t.pos, name);
        attribute(tree, t);
        return tree;
    }

    public Tree Literal(Tree tree,
                        Object value) {
        Literal t = (Literal)tree;
        tree = make.Literal(t.pos, value);
        attribute(tree, t);
        return tree;
    }

    public Tree TypeTerm(Tree tree) {
	TypeTerm t = (TypeTerm) tree;
	tree = make.TypeTerm(t.pos);
	attribute(tree, t);
	return tree;
    }

    public Tree SingletonType(Tree tree, Tree ref) {
	SingletonType t = (SingletonType)tree;
	tree = make.SingletonType(t.pos, ref);
	attribute(tree, t);
	return tree;
    }

    public Tree SelectFromType(Tree tree, Tree qualifier, Name selector) {
	SelectFromType t = (SelectFromType)tree;
	tree = make.SelectFromType(t.pos, qualifier, selector);
	attribute(tree, t);
	return tree;
    }

    public Tree FunType(Tree tree,
                        Tree[] argtpe,
                        Tree restpe) {
        FunType t = (FunType)tree;
        tree = make.FunType(t.pos, argtpe, restpe);
        attribute(tree, t);
        return tree;
    }

    public Tree CompoundType(Tree tree,
                             Tree[] baseTypes,
                             Tree[] refinements) {
        CompoundType t = (CompoundType)tree;
        tree = make.CompoundType(t.pos, baseTypes, refinements);
        attribute(tree, t);
        return tree;
    }

    public Tree CovariantType(Tree tree,
                              Tree clazz) {
        CovariantType t = (CovariantType)tree;
        tree = make.CovariantType(t.pos, clazz);
        attribute(tree, t);
        return tree;
    }

    public Tree AppliedType(Tree tree,
                            Tree tpe,
                            Tree[] args) {
        AppliedType t = (AppliedType)tree;
        tree = make.AppliedType(t.pos, tpe, args);
        attribute(tree, t);
        return tree;
    }
}
