/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.ast;

import java.util.Map;
import java.util.HashMap;

import scalac.symtab.Symbol;
import scalac.symtab.SymbolCloner;
import scalac.util.Name;
import scalac.util.Debug;

/**
 * This class implements a tree traverser that clones specified
 * symbols it encounters.
 */
public class TreeSymbolCloner extends Traverser {

    //########################################################################
    // Private Fields

    // !!! replace Idents in patterns by ValDefs and remove this field
    /** Indicates whether we are in a pattern */
    private boolean inPattern = false;

    //########################################################################
    // Public Fields

    /** The symbol cloner used to clone symbols */
    public final SymbolCloner cloner;

    //########################################################################
    // Public Constructors

    /** Initializes a new instance. */
    public TreeSymbolCloner(SymbolCloner cloner) {
        this.cloner = cloner;
    }

    //########################################################################
    // Public Methods

    /**
     * Returns true iff the symbol of the given tree symbol must be
     * cloned. The default implementation returns true iff the tree
     * defines a symbol and that symbol hasn't been cloned yet.
     */
    public boolean mustCloneSymbolOf(Tree tree) {
        // !!! replace Idents in patterns by ValDefs and remove this switch
        switch (tree) {
        case Ident(Name name):
            if (!inPattern || !name.isVariable()) return false; else break;
        default:
            if (!tree.definesSymbol()) return false; else break;
        }
        return !cloner.clones.containsKey(tree.symbol());
    }

    /**
     * Clones the given symbol. The default implementation invokes
     * getOwnerOfCloneOf to obtain the owner of the cloned symbol and
     * then invokes the symbol cloner to clone the symbol.
     */
    public Symbol cloneSymbol(Symbol symbol) {
        return cloner.cloneSymbol(symbol);
    }

    /**
     * Traverses the tree and clones symbols. The default
     * implemenation invokes cloneSymbol with the symbol of every tree
     * nodes for which mustCloneSymbolOf returns true.
     */
    public void traverse(Tree tree) {
        if (mustCloneSymbolOf(tree)) cloneSymbol(tree.symbol());
        // !!!replace Idents in patterns by ValDefs and remove this switch
        switch (tree) {
        case PatDef(_, Tree pat, Tree rhs):
            inPattern = true;
            traverse(pat);
            inPattern = false;
            traverse(rhs);
            return;
        case CaseDef(Tree pat, Tree guard, Tree body):
            inPattern = true;
            traverse(pat);
            inPattern = false;
            traverse(guard);
            traverse(body);
            return;
        default:
            super.traverse(tree);
            return;
        }
    }

    //########################################################################
}
