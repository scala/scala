/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.ast;

import java.util.Map;

import scalac.Global;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.symtab.SymbolSubstTypeMap;
import scalac.util.Debug;

/**
 * This class implements a deep tree cloner. It provides support to
 * change tree symbols and tree types on the fly. This cloner never
 * clones symbols, but its default implementation of getSymbolFor
 * requires that all symbols that are defined by the tree to be cloned
 * are present in the symbol substitution. This implies that if the
 * tree to clone contains definitions, it must first be traversed to
 * map all defined symbols. To do this, one can use TreeSymbolCloner.
 */
public class TreeCloner extends Transformer {

    //########################################################################
    // Private Fields

    /** The symbol substitution to apply to tree symbols */
    private final Map/*<Symbol,Symbol>*/ symbols;

    /** The type map to apply to tree types */
    private final Type.Map types;

    //########################################################################
    // Public Constructors

    /** Initializes a new instance. */
    public TreeCloner(Global global, SymbolSubstTypeMap types) {
        this(global, types.getSymbols(), types);
    }

    /** Initializes a new instance. */
    public TreeCloner(Global global, Map symbols, Type.Map types) {
        super(global, global.make, new StrictTreeCopier(global.make));
        this.symbols = symbols;
        this.types = types;
    }

    //########################################################################
    // Public Methods

    /**
     * Returns the symbol for the given cloned tree. The default
     * implementation returns the result of the application of the
     * symbol substitution to the given tree's symbol. If there is no
     * mapping for that symbol returns that same symbol.
     */
    public Symbol getSymbolFor(Tree tree) {
        Symbol oldsym = tree.symbol();
        Symbol newsym = (Symbol)symbols.get(oldsym);
        // cloned definitions must have a cloned symbol
        assert newsym != null || !tree.definesSymbol() : Debug.show(oldsym);
        return newsym != null ? newsym : oldsym;
    }

    /**
     * Returns the type for the given cloned tree. The default
     * implementation returns the result of the application of the
     * type map to the given tree's type.
     */
    public Type getTypeFor(Tree tree) {
        return types.apply(tree.getType());
    }

    /** Traverses the given tree and returns a deeply cloned one. */
    public Tree transform(Tree tree) {
        tree = super.transform(tree);
        if (tree.hasSymbol()) tree.setSymbol(getSymbolFor(tree));
        tree.setType(getTypeFor(tree));
        return tree;
    }

    //########################################################################
}
