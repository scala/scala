/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $Id$

package scalac.ast;

import java.io.*;
import java.util.*;
import scalac.*;
import scalac.util.*;
import Tree.*;


/** Class to traverse a tree without modifying it.
 *
 *  @author     Michel Schinz
 */

public class Traverser {
    public Traverser() {}

    // this should be removed in the future
    public Traverser(Global global) {}

    public void traverse(Unit unit) {
        traverse(unit.body);
    }

    public void traverse(Tree tree) {
        switch (tree) {
        case Bad():
        case Empty:
        case Ident(_):
        case Literal(_):
            return;

        case ClassDef(int mods,
                      Name name,
                      TypeDef[] tparams,
                      ValDef[][] vparams,
		      Tree tpe,
                      Template impl):
            traverse(tparams);
            traverse(vparams);
	    traverse(tpe);
            traverse(impl);
            return;

        case PackageDef(Tree packaged, Template impl) :
            traverse(packaged);
            traverse(impl);
            return;

        case ModuleDef(int mods, Name name, Tree tpe, Template impl):
            traverse(tpe);
            traverse(impl);
            return;

        case ValDef(int mods, Name name, Tree tpe, Tree rhs):
            traverse(tpe);
            traverse(rhs);
            return;

        case PatDef(int mods, Tree pat, Tree rhs):
            traverse(pat);
            traverse(rhs);
            return;

        case DefDef(int mods,
                    Name name,
                    TypeDef[] tparams,
                    ValDef[][] vparams,
                    Tree tpe,
                    Tree rhs):
            traverse(tparams);
            traverse(vparams);
            traverse(tpe);
            traverse(rhs);
            return;

        case TypeDef(int mods, Name name, TypeDef[] tparams, Tree rhs):
	    traverse(tparams);
            traverse(rhs);
            return;

        case Import(Tree expr, Name[] selectors):
            traverse(expr);
            return;

        case CaseDef(Tree pat, Tree guard, Tree body):
            traverse(pat);
            traverse(guard);
            traverse(body);
            return;

        case Template(Tree[] baseClasses, Tree[] body):
            traverse(baseClasses);
            traverse(body);
            return;

	case LabelDef(Tree[] params,Tree rhs):
	    traverse(params);
	    traverse(rhs);
	    return;

        case Block(Tree[] stats):
            traverse(stats);
            return;

        case Tuple(Tree[] trees):
            traverse(trees);
            return;

        case Visitor(CaseDef[] cases):
            traverse(cases);
            return;

        case Function(ValDef[] vparams, Tree body):
            traverse(vparams);
            traverse(body);
            return;

        case Assign(Tree lhs, Tree rhs):
            traverse(lhs);
            traverse(rhs);
            return;

        case If(Tree cond, Tree thenp, Tree elsep):
            traverse(cond);
            traverse(thenp);
            traverse(elsep);
            return;

        case New(Template templ):
            traverse(templ);
            return;

        case Typed(Tree expr, Tree tpe):
            traverse(expr);
            traverse(tpe);
            return;

        case TypeApply(Tree fun, Tree[] tparams):
            traverse(fun);
            traverse(tparams);
            return;

        case Apply(Tree fun, Tree[] vparam):
            traverse(fun);
            traverse(vparam);
            return;

        case Super(Tree tpe):
            traverse(tpe);
            return;

        case This(Tree qualifier):
            traverse(qualifier);
            return;

        case Select(Tree qualifier, Name selector):
            traverse(qualifier);
            return;

	case SingletonType(Tree ref):
	    traverse(ref);
	    return;

	case SelectFromType(Tree qualifier, Name selector):
	    traverse(qualifier);
	    return;

        case FunType(Tree[] argtpes, Tree restpe):
            traverse(argtpes);
            traverse(restpe);
            return;

        case CompoundType(Tree[] baseTypes, Tree[] refinements):
            traverse(baseTypes);
            traverse(refinements);
            return;

        case TupleType(Tree[] types):
            traverse(types);
            return;

        case AppliedType(Tree tpe, Tree[] args):
            traverse(tpe);
            traverse(args);
            return;

        case CovariantType(Tree tpe):
            traverse(tpe);
            return;

        default:
            throw new ApplicationError("unknown node " + tree);
        }
    }

    public void traverse(Tree[] array) {
        for (int i = 0; i < array.length; ++i)
            traverse(array[i]);
    }

    public void traverse(Tree[][] array) {
        for (int i = 0; i < array.length; ++i)
            traverse(array[i]);
    }
}
