/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scaladoc;

import java.util.HashSet;
import java.util.Set;

import scalac.ast.Tree;
import scalac.ast.Tree.*;
import scalac.symtab.Symbol;
import scalac.util.*;

/**
 * Computes the symbols defined in a syntactic tree.
 */
class TreeSymbols {

   protected Set/*<Symbol>*/ syms;

    public TreeSymbols(Tree tree) {
	this.syms = new HashSet();
	traverse(tree);
    }

    public boolean contains(Symbol sym) {
	return syms.contains(sym);
    }

    protected void traverse(Tree[] members) {
	for(int i = 0; i< members.length; i++)
	    traverse(members[i]);
    }
    // with
    protected void traverse(Tree tree) {
	if (tree.hasSymbol() && !ScalaSearch.isGenerated(tree.symbol())) {
	    switch (tree) {
	    case PackageDef(Tree packaged, Template impl):
		traverse(impl.body);
		break;
		case ClassDef(int mods, Name name, AbsTypeDef[] tparams,
			      ValDef[][] vparams, Tree tpe, Template impl):
		    syms.add(tree.symbol());
		for(int i = 0; i < tparams.length; i++)
		    syms.add(tparams[i].symbol());
		for(int i = 0; i < vparams.length; i++)
		    for(int j = 0; j < vparams[i].length; j++)
			syms.add(vparams[i][j].symbol());
		traverse(impl.body);
		break;
	    case ModuleDef(int mods, Name name, Tree tpe, Template impl):
		syms.add(tree.symbol());
		if (tpe.isMissing())
		    traverse(impl.body);
		break;
	    case ValDef(int mods, Name name, Tree tpe, Tree rhs):
		syms.add(tree.symbol());
		break;
	    case DefDef(int mods, Name name, AbsTypeDef[] tparams,
			ValDef[][] vparams, Tree tpe, Tree rhs):

		syms.add(tree.symbol());
	    for(int i = 0; i < tparams.length; i++)
		syms.add(tparams[i].symbol());
	    for(int i = 0; i < vparams.length; i++)
		for(int j = 0; j < vparams[i].length; j++)
		    syms.add(vparams[i][j].symbol());
	    break;
	    case AbsTypeDef(_, _, _, _):
	    case AliasTypeDef(_, _, _, _):
		syms.add(tree.symbol());
		break;
	    default:
	    }
	}
    }
}
