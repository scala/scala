/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.checkers;

import scalac.util.*;
import scalac.ast.*;
import scalac.symtab.*;
import scalac.Global;
import scalac.util.Debug;
import Tree.*;

/**
 * Check that the owner of symbols is set correctly.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class CheckOwners extends Checker {
    protected Symbol currentOwner;

    public CheckOwners(Global global) {
        super(global);
	currentOwner = global.definitions.ROOT_CLASS;
    }

    protected void traverse(Tree tree, Symbol owner) {
        Symbol prevOwner = currentOwner;
        currentOwner = owner;
        traverse(tree);
        currentOwner = prevOwner;
    }

    protected void traverse(Tree[] array, Symbol owner) {
        Symbol prevOwner = currentOwner;
        currentOwner = owner;
        traverse(array);
        currentOwner = prevOwner;
    }

    protected void traverse(Tree[][] array, Symbol owner) {
        Symbol prevOwner = currentOwner;
        currentOwner = owner;
        traverse(array);
        currentOwner = prevOwner;
    }

    protected void traverse(Template templ, Symbol owner) {
	Symbol prevOwner = currentOwner;
	if (owner.kind == Kinds.CLASS)
	    currentOwner = owner.constructor();
	traverse(templ.parents);
	currentOwner = owner;

        Symbol templSymbol = templ.symbol();
        Tree[] body = templ.body;
        for (int i = 0; i < body.length; ++i) {
            switch (body[i]) {
            case PackageDef(_,_):
            case ClassDef(_,_,_,_,_,_):
            case ModuleDef(_,_,_,_):
            case DefDef(_,_,_,_,_,_):
            case ValDef(_,_,_,_):
            case TypeDef(_,_,_, _):
                traverse(body[i], owner);
                break;
            default:
                traverse(body[i], templSymbol);
            }
        }

	currentOwner = prevOwner;
    }

    protected void checkOwner(Tree tree, Symbol sym) {
        Symbol owner = sym.owner();
        verify(tree,
               owner == currentOwner,
               "owner",
               "incorrect owner for " + Debug.toString(sym) + ":\n"
               + "  found:    " + Debug.toString(owner) + "\n"
               + "  required: " + Debug.toString(currentOwner));
    }

    public void traverse(Tree tree) {
	switch(tree) {
	case PackageDef(Tree packaged, Template impl):
            check(tree);
            traverse(packaged);
            traverse(impl, packaged.symbol());
            break;

        case ClassDef(int mods,
                      Name name,
                      TypeDef[] tparams,
                      ValDef[][] vparams,
		      Tree tpe,
                      Template impl): {
            check(tree);
            traverse(tparams, tree.symbol().constructor());
            traverse(vparams, tree.symbol().constructor());
	    traverse(tpe);
            traverse(impl, tree.symbol());
        } break;

        case ModuleDef(int mods, Name name, Tree tpe, Template impl): {
            check(tree);
            traverse(tpe);
            traverse(impl, tree.symbol().moduleClass());
        } break;

        case DefDef(int mods,
                    Name name,
                    TypeDef[] tparams,
                    ValDef[][] vparams,
                    Tree tpe,
                    Tree rhs): {
            check(tree);
            traverse(tparams, tree.symbol());
            traverse(vparams, tree.symbol());
            traverse(tpe, tree.symbol());
            traverse(rhs, tree.symbol());
        } break;

        case ValDef(int mods, Name name, Tree tpe, Tree rhs): {
            check(tree);
            traverse(tpe);
            traverse(rhs, tree.symbol());
        } break;

        case TypeDef(int mods, Name name, Tree rhs, Tree lobound): {
            check(tree);
            traverse(rhs, tree.symbol());
	    // todo: we should do something about lobound here.
        } break;

	default:
	    super.traverse(tree);
        }
    }

    public void check(Tree tree) {
        switch (tree) {
        case PackageDef(_,_):
        case ClassDef(_,_,_,_,_,_):
        case ModuleDef(_,_,_,_):
        case DefDef(_,_,_,_,_,_):
        case ValDef(_,_,_,_):
        case TypeDef(_,_,_,_): {
            Symbol sym = tree.symbol();
            if (sym != null && sym != Symbol.NONE) {
                checkOwner(tree, sym);
                if (sym.kind == Kinds.CLASS)
                    checkOwner(tree, sym.constructor());
            }
        } break;

        default:
            ;                   // nothing to do
        }
    }
}
