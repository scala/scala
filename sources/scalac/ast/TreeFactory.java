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

public interface TreeFactory {

    public Tree Bad(int pos);

    public Tree ClassDef(int pos,
                         int mods,
                         Name name,
                         TypeDef[] tparams,
                         ValDef[][] vparams,
			 Tree tpe,
                         Template impl);

    public Tree PackageDef(int pos,
                           Tree packaged,
                           Template impl);

    public Tree ModuleDef(int pos,
                          int mods,
                          Name name,
                          Tree tpe,
                          Template impl);

    public Tree ValDef(int pos,
                       int mods,
                       Name name,
                       Tree tpe,
                       Tree rhs);

    public Tree PatDef(int pos,
                       int mods,
                       Tree pat,
                       Tree rhs);

    public Tree DefDef(int pos,
                       int mods,
                       Name name,
                       TypeDef[] tparams,
                       ValDef[][] vparams,
                       Tree tpe,
                       Tree rhs);

    public Tree TypeDef(int pos,
                        int mods,
                        Name name,
                        Tree rhs);

    public Tree Import(int pos,
                       Tree expr,
		       Name[] selectors);

    public CaseDef CaseDef(int pos,
			   Tree pat,
			   Tree guard,
			   Tree body);

    public Template Template(int pos,
                             Tree[] baseClasses,
                             Tree[] body);

    public Tree LabelDef(int pos,
			 Tree[] params,
			 Tree rhs);

    public Tree Block(int pos,
                      Tree[] stats);

    public Tree Tuple(int pos,
                      Tree[] trees);

    public Tree Visitor(int pos,
                        CaseDef[] cases);

    public Tree Function(int pos,
                         ValDef[] vparams,
                         Tree body);

    public Tree Assign(int pos,
                       Tree lhs,
                       Tree rhs);

    public Tree If(int pos,
                   Tree cond,
                   Tree thenp,
                   Tree elsep);

    public Tree New(int pos,
                    Template templ);

    public Tree Typed(int pos,
                      Tree expr,
                      Tree tpe);

    public Tree TypeApply(int pos,
                          Tree fun,
                          Tree[] tparams);

    public Tree Apply(int pos,
                      Tree fun,
                      Tree[] vparam);

    public Tree Super(int pos,
                      Tree tpe);

    public Tree This(int pos,
		     Tree qualifier);

    public Tree Select(int pos,
                       Tree qualifier,
                       Name selector);

    public Tree Ident(int pos,
                      Name name);

    public Tree Literal(int pos,
                        Object value);

    public Tree TypeTerm(int pos);

    public Tree SingletonType(int pos,
			      Tree ref);

    public Tree SelectFromType(int pos,
			       Tree qualifier,
			       Name selector);

    public Tree FunType(int pos,
                        Tree[] argtpes,
                        Tree restpe);

    public Tree CompoundType(int pos,
                             Tree[] baseTypes,
                             Tree[] refinements);

    public Tree AppliedType(int pos,
                            Tree tpe,
                            Tree[] args);

    public Tree CovariantType(int pos,
			      Tree tpe);
}
