/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$


package scalac.transformer.matching;

import scalac.ast.Traverser;

import scalac.ast.Tree;
import Tree.Ident;
import Tree.Bind;

import scalac.util.Name;
import scalac.util.FreshNameCreator;

import scalac.symtab.*;

import java.util.HashMap;
import java.util.Vector;


/** A tree traverser for handling fresh variables
 * todo: access method instead of
 * @author  Burak Emir
 * @version 1.0
 */
class FreshVariableTraverser extends VariableTraverser {

    int    pos;
    Symbol owner;
    FreshNameCreator fresh;

    /**
     */
    private HashMap helpMap;

    /**
     * @param pos
     * @param owner
     * @param fresh
     */
    public FreshVariableTraverser(int pos,
                                  Symbol owner,
                                  FreshNameCreator fresh) {
        this.pos   = pos;
        this.owner = owner;
        this.fresh = fresh;

        helpMap = new HashMap();
    }

    public HashMap getHelpMap() {
	return helpMap;
    }

    /**
     * @param sym
     */
    void handleVariableSymbol(Symbol sym) {
        Symbol helpVar =
            new TermSymbol(pos,
                           fresh.newName(sym.name.toString()),
                           owner,
                           0);
        helpVar.setType(sym.type());

        helpMap.put(sym, helpVar);
    }

    public static HashMap getVars( Tree t, Symbol owner, FreshNameCreator fresh ) {
	FreshVariableTraverser fvt = new FreshVariableTraverser( t.pos, owner, fresh );
	fvt.traverse( t );
	return fvt.helpMap;
    }

}
