/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.ast;

import Tree.*;
import scalac.util.Name;


public class DefaultTreeFactory implements TreeFactory {

    public Tree Bad(int pos) {
        Tree t = new ExtBad();
        t.pos = pos;
        return t;
    }

    public Tree ClassDef(int pos,
                         int mods,
                         Name name,
                         TypeDef[] tparams,
                         ValDef[][] vparams,
			 Tree tpe,
                         Template impl) {
        Tree t = new ExtClassDef(mods, name, tparams, vparams, tpe, impl);
        t.pos = pos;
        return t;
    }

    public Tree PackageDef(int pos,
                           Tree packaged,
                           Template impl) {
        Tree t = new PackageDef(packaged, impl);
        t.pos = pos;
        return t;
    }

    public Tree ModuleDef(int pos,
                          int mods,
                          Name name,
                          Tree tpe,
                          Template body) {
        Tree t = new ExtModuleDef(mods, name, tpe, body);
        t.pos = pos;
        return t;
    }

    public Tree ValDef(int pos,
                       int mods,
                       Name name,
                       Tree tpe,
                       Tree rhs) {
        Tree t = new ExtValDef(mods, name, tpe, rhs);
        t.pos = pos;
        return t;
    }

    public Tree PatDef(int pos,
                       int mods,
                       Tree pat,
                       Tree rhs) {
        Tree t = new PatDef(mods, pat, rhs);
        t.pos = pos;
        return t;
    }

    public Tree DefDef(int pos,
                       int mods,
                       Name name,
                       TypeDef[] tparams,
                       ValDef[][] vparams,
                       Tree tpe,
                       Tree rhs) {
        Tree t = new ExtDefDef(mods, name, tparams, vparams, tpe, rhs);
        t.pos = pos;
        return t;
    }


    public Tree TypeDef(int pos,
                        int mods,
                        Name name,
                        Tree rhs) {
        Tree t = new ExtTypeDef(mods, name, rhs);
        t.pos = pos;
        return t;
    }

    public Tree Import(int pos,
                       Tree expr,
		       Name[] selectors) {
        Tree t = new ExtImport(expr, selectors);
        t.pos = pos;
        return t;
    }

    public CaseDef CaseDef(int pos,
			   Tree pat,
			   Tree guard,
			   Tree body) {
        CaseDef t = new CaseDef(pat, guard, body);
        t.pos = pos;
        return t;
    }

    public Template Template(int pos,
                             Tree[] baseClasses,
                             Tree[] body) {
        Template t = new ExtTemplate(baseClasses, body);
        t.pos = pos;
        return t;
    }

    public Tree LabelDef(int pos,
			 Tree[] params,
			 Tree body) {
	Tree t = new ExtLabelDef(params,body);
	t.pos = pos;
	return t;
    }

    public Tree Block(int pos,
                      Tree[] stats) {
        Tree t = new Block(stats);
        t.pos = pos;
        return t;
    }

    public Tree Tuple(int pos,
                      Tree[] trees) {
        Tree t = new Tuple(trees);
        t.pos = pos;
        return t;
    }

    public Tree Visitor(int pos,
                        CaseDef[] cases) {
        Tree t = new Visitor(cases);
        t.pos = pos;
        return t;
    }

    public Tree Function(int pos,
                         ValDef[] vparams,
                         Tree body) {
        Tree t = new Function(vparams, body);
        t.pos = pos;
        return t;
    }

    public Tree Assign(int pos,
                       Tree lhs,
                       Tree rhs) {
        Tree t = new Assign(lhs, rhs);
        t.pos = pos;
        return t;
    }

    public Tree If(int pos,
                   Tree cond,
                   Tree thenp,
                   Tree elsep) {
        Tree t = new If(cond, thenp, elsep);
        t.pos = pos;
        return t;
    }

    public Tree New(int pos,
                    Template templ) {
        Tree t = new New(templ);
        t.pos = pos;
        return t;
    }

    public Tree Typed(int pos,
                      Tree expr,
                      Tree tpe) {
        Tree t = new Typed(expr, tpe);
        t.pos = pos;
        return t;
    }

    public Tree TypeApply(int pos,
                          Tree fun,
                          Tree[] tparams) {
        Tree t = new TypeApply(fun, tparams);
        t.pos = pos;
        return t;
    }

    public Tree Apply(int pos,
                      Tree fun,
                      Tree[] vparam) {
        Tree t = new Apply(fun, vparam);
        t.pos = pos;
        return t;
    }

    public Tree Super(int pos,
                      Tree tpe) {
        Tree t = new Super(tpe);
        t.pos = pos;
        return t;
    }

    public Tree This(int pos,
		     Tree qualifier) {
        Tree t = new This(qualifier);
        t.pos = pos;
        return t;
    }

    public Tree Select(int pos,
                       Tree qualifier,
                       Name selector) {
        Tree t = new ExtSelect(qualifier, selector);
        t.pos = pos;
        return t;
    }

    public Tree Ident(int pos,
                      Name name) {
        Tree t = new ExtIdent(name);
        t.pos = pos;
        return t;
    }

    public Tree Literal(int pos,
                        Object value) {
        Tree t = new Literal(value);
        t.pos = pos;
        return t;
    }


    public Tree TypeTerm(int pos) {
	Tree t = new TypeTerm();
	t.pos = pos;
	return t;
    }

    public Tree SingletonType(int pos, Tree ref) {
        Tree t = new SingletonType(ref);
        t.pos = pos;
        return t;
    }

    public Tree SelectFromType(int pos,
			       Tree qualifier,
			       Name selector) {
        Tree t = new ExtSelectFromType(qualifier, selector);
        t.pos = pos;
        return t;
    }

    public Tree FunType(int pos,
                        Tree[] argtpes,
                        Tree restpe) {
        Tree t = new FunType(argtpes, restpe);
        t.pos = pos;
        return t;
    }

    public Tree CompoundType(int pos,
                             Tree[] mixins,
                             Tree[] fields) {
        Tree t = new CompoundType(mixins, fields);
        t.pos = pos;
        return t;
    }

    public Tree AppliedType(int pos,
                            Tree tpe,
                            Tree[] args) {
        Tree t = new AppliedType(tpe, args);
        t.pos = pos;
        return t;
    }

    public Tree CovariantType(int pos,
			      Tree tpe) {
        Tree t = new CovariantType(tpe);
        t.pos = pos;
        return t;
    }
}
