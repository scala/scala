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
import scalac.util.*;
import scalac.symtab.*;
import Tree.*;


/** A default transformer class. This class traverses the abstract
 *  syntax tree but does not do any transformations.
 *
 *  @author     Matthias Zenger
 *  @version    1.0
 */
public class Transformer extends Phase {
    /** the tree factory
     */
    public final TreeFactory make;

    /** a factory for copying trees; the attribution is preserved
     *  or translated according to the TreeCopyFactory; trees are
     *  only copied if new tree introduces changes
     */
    public final TreeCopyFactory copy;

    /** a tree generator
     */
    public final TreeGen gen;

    /** various constructors
     */
    public Transformer(Global global, PhaseDescriptor descr) {
        this(global, descr, global.make, new LazyTreeFactory(global.make));
    }

    public Transformer(Global global,
                       PhaseDescriptor descr,
                       TreeFactory make) {
        this(global, descr, make, new LazyTreeFactory(make));
    }

    public Transformer(Global global,
                       PhaseDescriptor descr,
                       TreeFactory make,
                       TreeCopyFactory copy) {
        super(global, descr);
        this.make = make;
        this.copy = copy;
        this.gen = global.treeGen;
    }

    public void apply() {
        super.apply();
    }

    public void apply(Unit unit) {
        unit.body = transform(unit.body);
    }

    public Tree[] transform(Tree[] ts) {
        for (int i = 0; i < ts.length; i++) {
            Tree t = transform(ts[i]);
            if (t != ts[i]) {
                Tree[] res = new Tree[ts.length];
                System.arraycopy(ts, 0, res, 0, i);
                res[i] = t;
                for (int j = i + 1; j < ts.length; j++)
                    res[j] = transform(ts[j]);
                return res;
            }
        }
        return ts;
    }

    public Tree[][] transform(Tree[][] ts) {
        for (int i = 0; i < ts.length; i++) {
            Tree[] t = transform(ts[i]);
            if (t != ts[i]) {
                Tree[][] res = new Tree[ts.length][];
                System.arraycopy(ts, 0, res, 0, i);
                res[i] = t;
                for (int j = i + 1; j < ts.length; j++)
                    res[j] = transform(ts[j]);
                return res;
            }
        }
        return ts;
    }

    public ValDef[][] transform(ValDef[][] ts) {
        for (int i = 0; i < ts.length; i++) {
            ValDef[] t = transform(ts[i]);
            if (t != ts[i]) {
                ValDef[][] res = new ValDef[ts.length][];
                System.arraycopy(ts, 0, res, 0, i);
                res[i] = t;
                for (int j = i + 1; j < ts.length; j++)
                    res[j] = transform(ts[j]);
                return res;
            }
        }
        return ts;
    }

    public ValDef[] transform(ValDef[] ts) {
        for (int i = 0; i < ts.length; i++) {
            Tree t = transform(ts[i]);
            if (t != ts[i]) {
                ValDef[] res = new ValDef[ts.length];
                System.arraycopy(ts, 0, res, 0, i);
                res[i] = (ValDef)t;
                for (int j = i + 1; j < ts.length; j++)
                    res[j] = (ValDef)transform(ts[j]);
                return res;
            }
        }
        return ts;
    }

    public TypeDef[] transform(TypeDef[] ts) {
        for (int i = 0; i < ts.length; i++) {
            Tree t = transform(ts[i]);
            if (t != ts[i]) {
                TypeDef[] res = new TypeDef[ts.length];
                System.arraycopy(ts, 0, res, 0, i);
                res[i] = (TypeDef)t;
                for (int j = i + 1; j < ts.length; j++)
                    res[j] = (TypeDef)transform(ts[j]);
                return res;
            }
        }
        return ts;
    }

    public CaseDef[] transform(CaseDef[] ts) {
        for (int i = 0; i < ts.length; i++) {
            Tree t = transform(ts[i]);
            if (t != ts[i]) {
                CaseDef[] res = new CaseDef[ts.length];
                System.arraycopy(ts, 0, res, 0, i);
                res[i] = (CaseDef)t;
                for (int j = i + 1; j < ts.length; j++)
                    res[j] = (CaseDef)transform(ts[j]);
                return res;
            }
        }
        return ts;
    }

    public Template transform(Template tree) {
        return (Template)transform((Tree)tree);
    }

    public Tree transform(Tree tree) {
        if (tree == null)
            return null;
        switch (tree) {
	case ClassDef(int mods,
		      Name name,
		      TypeDef[] tparams,
		      ValDef[][] vparams,
		      Tree tpe,
		      Template impl):
	    return copy.ClassDef(tree,
				 mods,
				 name,
				 transform(tparams),
				 transform(vparams),
				 transform(tpe),
				 transform(impl));
	case PackageDef(Tree packaged, Template impl):
	    return copy.PackageDef(tree,
				   transform(packaged),
				   transform(impl));
	case ModuleDef(int mods,
		       Name name,
		       Tree tpe,
		       Template impl):
	    return copy.ModuleDef(tree,
				  mods,
				  name,
				  transform(tpe),
				  transform(impl));
	case ValDef(int mods,
		    Name name,
		    Tree tpe,
		    Tree rhs):
	    return copy.ValDef(tree,
			       mods,
			       name,
			       transform(tpe),
			       transform(rhs));
	case PatDef(int mods,
		    Tree pat,
		    Tree rhs):
	    return copy.PatDef(tree,
			       mods,
			       transform(pat),
			       transform(rhs));
	case DefDef(int mods,
		    Name name,
		    TypeDef[] tparams,
		    ValDef[][] vparams,
		    Tree tpe,
		    Tree rhs):
	    return copy.DefDef(tree,
			       mods,
			       name,
			       transform(tparams),
			       transform(vparams),
			       transform(tpe),
			       transform(rhs));
	case TypeDef(int mods,
		     Name name,
		     Tree rhs):
	    return copy.TypeDef(tree,
				mods,
				name,
				transform(rhs));
	case Import(Tree expr, Name[] selectors):
	    return copy.Import(tree,
			       transform(expr),
			       selectors);
	case CaseDef(Tree pat, Tree guard, Tree body):
	    return copy.CaseDef(tree,
				transform(pat),
				transform(guard),
				transform(body));
	case Template(Tree[] parents, Tree[] body):
	    return copy.Template(tree,
				 transform(parents),
				 transform(body));
	case LabelDef(Tree[] params,Tree rhs):
	    return copy.LabelDef(tree,
				 transform(params),
				 transform(rhs));
	case Block(Tree[] stats):
	    return copy.Block(tree,
			      transform(stats));
	case Tuple(Tree[] trees):
	    return copy.Tuple(tree,
			      transform(trees));
	case Visitor(CaseDef[] cases):
	    return copy.Visitor(tree,
				transform(cases));
	case Function(ValDef[] vparams, Tree body):
	    return copy.Function(tree,
				 transform(vparams),
				 transform(body));
	case Assign(Tree lhs, Tree rhs):
	    return copy.Assign(tree,
			       transform(lhs),
			       transform(rhs));
	case If(Tree cond, Tree thenp, Tree elsep):
	    return copy.If(tree,
			   transform(cond),
			   transform(thenp),
			   transform(elsep));
	case New(Template templ):
	    return copy.New(tree,
			    transform(templ));
	case Typed(Tree expr, Tree tpe):
	    return copy.Typed(tree,
			      transform(expr),
			      transform(tpe));
	case TypeApply(Tree fun, Tree[] args):
	    return copy.TypeApply(tree,
				  transform(fun),
				  transform(args));
	case Apply(Tree fun, Tree[] args):
	    return copy.Apply(tree,
			      transform(fun),
			      transform(args));
	case Super(Tree tpe):
	    return copy.Super(tree,
			      transform(tpe));
	case This(Tree qualifier):
	    return copy.This(tree,
			     transform(qualifier));
	case Select(Tree qualifier, Name selector):
	    return copy.Select(tree,
			       transform(qualifier),
			       selector);
	case Ident(Name name):
	    return copy.Ident(tree, name);
	case Literal(Object value):
	    return copy.Literal(tree, value);
	case TypeTerm():
	    return copy.TypeTerm(tree);
	case SingletonType(Tree ref):
	    return copy.SingletonType(tree,
				      transform(ref));
	case SelectFromType(Tree qualifier, Name selector):
	    return copy.SelectFromType(tree,
				       transform(qualifier),
				       selector);
	case FunType(Tree[] argtpes, Tree restpe):
	    return copy.FunType(tree,
				transform(argtpes),
				transform(restpe));
	case CompoundType(Tree[] parents, Tree[] refinements):
	    return copy.CompoundType(tree,
				     transform(parents),
				     transform(refinements));
	case AppliedType(Tree tpe, Tree[] args):
	    return copy.AppliedType(tree,
				    transform(tpe),
				    transform(args));
	case CovariantType(Tree tpe):
	    return copy.CovariantType(tree,
				      transform(tpe));
	default:
	    return tree;
        }
    }
    /* a full pattern-matching statement:

        switch (tree) {
        case PackageDef(Tree packaged, Template impl):

	case ClassDef(int mods, Name name, TypeDef[] tparams, ValDef[][] vparams,
	              Template impl):

	case ModuleDef(int mods, Name name, Tree tpe, Template impl):

	case ValDef(int mods, Name name, Tree tpe, Tree rhs):

	case PatDef(int mods, Tree pat, Tree rhs):

	case DefDef(int mods, Name name, TypeDef[] tparams, ValDef[][] vparams,
	            Tree tpe, Tree rhs):

	case TypeDef(int mods, Name name, Tree rhs):

	case Import(Tree expr):

	case CaseDef(Tree pat, Tree guard, Tree body):

	case Template(Tree[] baseClasses, Tree[] body):

	case LabelDef(Tree[] params,Tree rhs):

	case Block(Tree[] stats):

	case Tuple(Tree[] trees):

	case Visitor(CaseDef[] cases):

	case Function(ValDef[] vparams, Tree body):

	case Assign(Tree lhs, Tree rhs):

	case If(Tree cond, Tree thenp, Tree elsep):

	case New(Template templ):

	case Typed(Tree expr, Tree tpe):

	case TypeApply(Tree fun, Tree[] args):

	case Apply(Tree fun, Tree[] args):

	case Super(Tree tpe):

	case Select(Tree qualifier, Name selector):

	case Ident(Name name):

	case Literal(int kind, Object value):

	case TypeTerm():

	case SingletonType(Tree ref):

	case SelectFromType(Tree qualifier, Name selector):

	case FunType(Tree argtpe, Tree restpe):

	case CompoundType(Tree[] baseTypes, Tree[] refinements):

	case AppliedType(Tree tpe, Tree[] args):

	default:
	    return tree;
        }
    */

}
