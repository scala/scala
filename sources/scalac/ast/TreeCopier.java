/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: TreeCopier.java,v 1.17 2002/06/13 12:04:12 schinz Exp $
// $Id$

package scalac.ast;

import scalac.*;
import scalac.util.Name;
import scalac.symtab.*;
import java.util.*;

/**
 * Superclass for tree copiers. Takes care of duplicating symbols and
 * types when needed.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class TreeCopier extends SubstTransformer {
    public TreeCopier(Global global, TreeFactory make) {
        super(global, make);
    }

    private boolean inPattern = false;

    // Return true iff tree's symbol must be copied. By default,
    // symbols which are defined are copied.
    public boolean mustCopySymbol(Tree tree) {
        switch (tree) {
        case Ident(Name name):
            return (inPattern && name.isVariable()) || tree.definesSymbol();
        default:
            return tree.definesSymbol();
        }
    }

    public Tree copy(Tree tree) {
        // Copy all symbols that have to be copied.
        Traverser symCopier = new Traverser() {
                public void traverse(Tree tree) {
                    if (tree.hasSymbol()) {
                        Symbol sym = tree.symbol();

                        if (sym != Symbol.NONE
                            && mustCopySymbol(tree)
                            && !symbolMap.containsKey(sym)) {
                            Symbol newSym = sym.cloneSymbol();

                            if (symbolMap.containsKey(newSym.owner()))
                                newSym.setOwner((Symbol)symbolMap.get(newSym.owner()));

                            symbolMap.put(sym, newSym);
                        }
                    }
                    switch (tree) {
                    case CaseDef(Tree pat, Tree guard, Tree body):
                        inPattern = true; traverse(pat); inPattern = false;
                        traverse(guard);
                        traverse(body);
                        break;
                    default:
                        super.traverse(tree);
                    }
                }
            };
        symCopier.traverse(tree);

        // Copy tree
        Tree newTree = transform(tree);

        // Update symbols
        Iterator symbolsIt = symbolMap.entrySet().iterator();
        while (symbolsIt.hasNext()) {
            Map.Entry symPair = (Map.Entry)symbolsIt.next();
            Symbol oldSym = (Symbol)symPair.getKey();
            Symbol newSym = (Symbol)symPair.getValue();

            newSym.setInfo(smApplier.apply(typeMap.apply(oldSym.info())));
        }

        return newTree;
    }
}
