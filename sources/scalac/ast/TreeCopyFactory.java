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

/**
 * Interface for a kind of factory which, for each node constructor,
 * takes an original node from which some data will be copied or shared.
 *
 * @author Michel Schinz
 * @version 1.1
 */
public interface TreeCopyFactory {

    public void attribute(Tree newTree, Tree oldTree);

    public Tree Bad(Tree tree);

    public Tree ClassDef(Tree tree,
                         int mods,
                         Name name,
                         TypeDef[] tparams,
                         ValDef[][] vparams,
			 Tree tpe,
                         Template impl);

    public Tree PackageDef(Tree tree,
                           Tree packaged,
                           Template impl);

    public Tree ModuleDef(Tree tree,
                          int mods,
                          Name name,
                          Tree tpe,
                          Template impl);

    public Tree ValDef(Tree tree,
                       int mods,
                       Name name,
                       Tree tpe,
                       Tree rhs);

    public Tree PatDef(Tree tree,
                       int mods,
                       Tree pat,
                       Tree rhs);

    public Tree DefDef(Tree tree,
                       int mods,
                       Name name,
                       TypeDef[] tparams,
                       ValDef[][] vparams,
                       Tree tpe,
                       Tree rhs);

    public Tree TypeDef(Tree tree,
                        int mods,
                        Name name,
			TypeDef[] tparams,
                        Tree rhs);

    public Tree Import(Tree tree,
                       Tree expr,
		       Name[] selectors);

    public Tree CaseDef(Tree tree,
                        Tree pat,
                        Tree guard,
                        Tree body);

    public Template Template(Tree tree,
                             Tree[] baseClasses,
                             Tree[] body);

    public Tree LabelDef(Tree tree,
			 Tree[] params,
			 Tree rhs);

    public Tree Block(Tree tree,
                      Tree[] stats);

    public Tree Tuple(Tree tree,
                      Tree[] trees);

    public Tree Visitor(Tree tree,
                        CaseDef[] cases);

    public Tree Function(Tree tree,
                         ValDef[] vparams,
                         Tree body);

    public Tree Assign(Tree tree,
                       Tree lhs,
                       Tree rhs);

    public Tree If(Tree tree,
                   Tree cond,
                   Tree thenp,
                   Tree elsep);

    public Tree New(Tree tree,
                    Template templ);

    public Tree Typed(Tree tree,
                      Tree expr,
                      Tree tpe);

    public Tree TypeApply(Tree tree,
                          Tree fun,
                          Tree[] args);

    public Tree Apply(Tree tree,
                      Tree fun,
                      Tree[] args);

    public Tree Super(Tree tree,
                      Tree tpe);

    public Tree This(Tree tree,
		     Tree qualifier);

    public Tree Select(Tree tree,
                       Tree qualifier,
                       Name selector);

    public Tree Ident(Tree tree,
                      Name name);

    public Tree Literal(Tree tree,
                        Object value);

    public Tree SingletonType(Tree tree,
			      Tree ref);

    public Tree SelectFromType(Tree tree,
			       Tree qualifier,
			       Name selector);

    public Tree FunType(Tree tree,
                        Tree[] argtpes,
                        Tree restpe);

    public Tree CompoundType(Tree tree,
                             Tree[] baseTypes,
                             Tree[] refinements);

    public Tree AppliedType(Tree tree,
                            Tree tpe,
                            Tree[] args);

    public Tree CovariantType(Tree tree,
			      Tree tpe);

}
