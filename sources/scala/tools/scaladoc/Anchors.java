/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scaladoc;

import java.util.HashMap;
import java.util.Map;

import scalac.ast.Tree;
import scalac.ast.Tree.*;
import scalac.symtab.Symbol;
import scalac.util.*;

/**
 * Computes a unique URL for every defined symbol.
 */
class Anchors {

    protected final Map/*<Symbol, String>*/ anchors = new HashMap();

    /**
     * Main function.
     *
     * @param tree
     */
    static Map/*<Symbol, String>*/ apply(Tree tree) {
	Anchors a = new Anchors();
	a.traverse(tree, new IntIterator(), null);
	return a.anchors;
    }

    /**
     * ..
     */
    int currentpage = 0;

    /**
     * ..
     *
     * @param sym
     */
    String symbolAnchor(Symbol sym) {
        String anchorId = (String) anchors.get(sym);
        if (anchorId == null) {
	    //anchorId = sym.isRoot() ? "root.html" : new Integer(anchors.size()).toString() + ".html";
	    if (sym.isRoot())
		anchorId = "root.html";
	    else {
		anchorId = new Integer(currentpage).toString() + ".html";
		currentpage++;
	    }
	    anchors.put(sym, anchorId);
	}
	return anchorId;
    }

    /**
     * The natural numbers.
     */
    static class IntIterator {
	private int current = 0;
	public boolean hasNext() { return true; }
	public int next() { int res = current; current = current + 1; return res; }
    }

    /**
     * Computes a unique URL for every defined symbol.
     *
     * @param members
     * @param intIt
     * @param ownerpage
     */
    void traverse(Tree[] members, IntIterator intIt, String ownerpage) {
	for(int i = 0; i< members.length; i++)
	    traverse(members[i], intIt, ownerpage);
    }

    void traverse(Tree tree, IntIterator intIt, String ownerpage) {
	if (tree.hasSymbol() && !ScalaSearch.isGenerated(tree.symbol())) {
	    switch (tree) {
	    case PackageDef(Tree packaged, Template impl):
		traverse(impl.body, null, null);
		break;
	    case ClassDef(int mods, Name name, AbsTypeDef[] tparams, ValDef[][] vparams, Tree tpe, Template impl):
		String anchor = symbolAnchor(tree.symbol());
		for(int i = 0; i < tparams.length; i++)
		    anchors.put(tparams[i].symbol(), anchor);
		for(int i = 0; i < vparams.length; i++)
		    for(int j = 0; j < vparams[i].length; j++)
			anchors.put(vparams[i][j].symbol(), anchor);
		traverse(impl.body, new IntIterator(), anchor);
		break;
	    case ModuleDef(int mods, Name name, Tree tpe, Template impl):
		String anchor = symbolAnchor(tree.symbol());
		if (tpe.isMissing())
		    traverse(impl.body, new IntIterator(), anchor);
		break;
	    case ValDef(int mods, Name name, Tree tpe, Tree rhs):
		anchors.put(tree.symbol(), ownerpage + "#" + intIt.next());
		break;
	    case DefDef(int mods, Name name, AbsTypeDef[] tparams, ValDef[][] vparams, Tree tpe, Tree rhs):
		String anchor = ownerpage + "#" + intIt.next();
		anchors.put(tree.symbol(), anchor);
		for(int i = 0; i < tparams.length; i++)
		    anchors.put(tparams[i].symbol(), anchor);
		for(int i = 0; i < vparams.length; i++)
		    for(int j = 0; j < vparams[i].length; j++)
			anchors.put(vparams[i][j].symbol(), anchor);
		break;
	    case AbsTypeDef(_, _, _, _):
	    case AliasTypeDef(_, _, _, _):
		anchors.put(tree.symbol(), ownerpage + "#" + intIt.next());
		break;
	    default:
	    }
	}
    }

}
