/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: SubstTransformer.java,v 1.3 2002/04/19 10:55:15 schinz Exp $
// $Id$

package scalac.ast;

import scalac.*;
import scalac.ast.*;
import scalac.symtab.*;
import scalac.util.*;
import scalac.typechecker.*;
import Tree.*;

import java.util.*;

// TODO remove stacks of substitutions, since they never grow to more
// than one element.

/**
 * A transformer which performs symbol or type substitutions.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class SubstTransformer extends Transformer {
    protected Map/*<Symbol,Symbol>*/ symbolMap = new HashMap();
    protected SymbolMapApplier smApplier = new SymbolMapApplier(symbolMap);
    protected LinkedList/*<Map<Symbol,Symbol>>*/ ssStack = new LinkedList();

    protected Symbol[] typeMapFormals = Symbol.EMPTY_ARRAY;
    protected Type[] typeMapActuals = Type.EMPTY_ARRAY;
    protected LinkedList/*<Symbol[]>*/ tmfStack = new LinkedList();
    protected LinkedList/*<Symbol[]>*/ tmaStack = new LinkedList();

    protected Type.Map userTypeMap = null;

    final protected Type.Map typeMap =
        new Type.Map() {
            public Type apply(Type t) {
                return t.subst(typeMapFormals, typeMapActuals);
            }
        };

    // !!! remove (use transformer's copy, both are strict)
    protected final TreeCopier simpleCopy;

    public SubstTransformer(Global global, TreeFactory make) {
        super(global, make, new StrictTreeCopier(make));
        this.simpleCopy = new StrictTreeCopier(make);
    }

    public boolean mustSubstituteSymbol(Tree tree) {
        return true;
    }

    public void setTypeMap(Type.Map map) {
        assert userTypeMap == null;
        userTypeMap = map;
    }

    public void clearTypeMap() {
        assert userTypeMap != null;
        userTypeMap = null;
    }

    protected void updateSymbolSubst() {
        symbolMap.clear();
        Iterator ssIt = ssStack.iterator();
        while (ssIt.hasNext()) {
            Map/*<Symbol,Symbol>*/ map = (Map)ssIt.next();
            symbolMap.putAll(map);
        }
    }

    public void pushSymbolSubst(Map map) {
        assert ssStack.size() == 0;
        ssStack.addLast(map);
        updateSymbolSubst();
    }

    public void popSymbolSubst() {
        assert ssStack.size() == 1;
        ssStack.removeLast();
        updateSymbolSubst();
    }

    public void clearSymbolSubst() {
        ssStack.clear();
        updateSymbolSubst();
    }

    protected void updateTypeSubst() {
        assert tmfStack.size() == tmaStack.size();

        Map/*<Symbol,Type>*/ map = new HashMap();
        Iterator tmfIt = tmfStack.iterator();
        Iterator tmaIt = tmaStack.iterator();
        while (tmfIt.hasNext()) {
            assert tmaIt.hasNext();
            Symbol[] formals = (Symbol[]) tmfIt.next();
            Type[] actuals = (Type[]) tmaIt.next();
            assert formals.length == actuals.length;
            for (int i = 0; i < formals.length; ++i)
                map.put(formals[i], actuals[i]);
        }

        typeMapFormals = new Symbol[map.size()];
        typeMapActuals = new Type[map.size()];

        Set/*<Map.Entry<Symbol,Type>>*/ entries = map.entrySet();
        Iterator entriesIt = entries.iterator();
        int i = 0;
        while (entriesIt.hasNext()) {
            Map.Entry entry = (Map.Entry)entriesIt.next();
            typeMapFormals[i] = (Symbol)entry.getKey();
            typeMapActuals[i] = (Type)entry.getValue();
            ++i;
        }
    }

    public void pushTypeSubst(Symbol[] from, Type[] to) {
        assert tmfStack.size() == 0;

        assert from.length == to.length;
        tmfStack.addLast(from);
        tmaStack.addLast(to);
        updateTypeSubst();
    }

    public void popTypeSubst() {
        assert tmfStack.size() == 1;

        tmfStack.removeLast();
        tmaStack.removeLast();
        updateTypeSubst();
    }

    public void clearTypeSubst() {
        tmfStack.clear();
        tmaStack.clear();
        updateTypeSubst();
    }

    public Tree transform(Tree oldTree) {
        Tree newTree = super.transform(oldTree);
        if (oldTree.hasSymbol()) {
            Symbol oldSym = oldTree.symbol();

            if (mustSubstituteSymbol(oldTree)
                && symbolMap.containsKey(oldSym)) {
                newTree.setSymbol((Symbol)symbolMap.get(oldSym));
            } else
                newTree.setSymbol(oldTree.symbol());
        }

        newTree.type = transformType(oldTree.type);
        return syncTree(newTree);
    }

    // Update the tree so that:
    //   1. names reflect the ones in symbols,
    //   2. types reflect the ones in symbols.
    //   3. modifiers reflect the ones in symbols.
    public Tree syncTree(Tree tree) {
        Type newType = null;

        Symbol sym = null;
        if (tree.hasSymbol()) {
            sym = tree.symbol();
            newType = transformType(sym.nextInfo());
        }

        // !!! Do we really need to copy ? transformer's copier is strict so
        // !!! all trees should already be copies.
        switch (tree) {
        case ClassDef(_, // fix Emacs :
                      _,
                      Tree.TypeDef[] tparams,
                      Tree.ValDef[][] vparams,
                      Tree tpe,
                      Tree.Template impl) :
            return simpleCopy
                .ClassDef(tree, sym, tparams, vparams, tpe, impl);

        case ModuleDef(_, _, Tree tpe, Template impl):
            return simpleCopy.ModuleDef(tree,
                                        sym,
                                        gen.mkType(tpe.pos, newType),
                                        impl);

        case ValDef(int mods, Name name, Tree tpe, Tree rhs):
            return simpleCopy.ValDef(tree,
                                     sym,
                                     gen.mkType(tpe.pos, newType),
                                     rhs);

        case DefDef(_,          // fix for Emacs :
                    Name name,
                    Tree.TypeDef[] tparams,
                    Tree.ValDef[][] vparams,
                    Tree tpe,
                    Tree rhs):
            return simpleCopy.DefDef(tree,
                                     sym,
                                     tparams,
                                     vparams,
                                     gen.mkType(tpe.pos, newType.resultType()),
                                     rhs);

        case Select(Tree qualifier, _):
            return simpleCopy.Select(tree, sym, qualifier);

        case Ident(_):
            return simpleCopy.Ident(tree, sym);

            // TODO add a case for TypeDef?

        case Typed(Tree expr, Tree tpe): {
            Type newType2 = transformType(((Tree.Typed)tree).tpe.type);
            return simpleCopy.Typed(tree,
                                    expr,
                                    gen.mkType(tpe.pos, newType2));
        }

        default:
            return tree;
        }
    }

    //////////////////////////////////////////////////////////////////////

    protected Type transformType(Type tp) {
        Type tp1 = (userTypeMap != null ? userTypeMap.apply(tp) : tp);
        Type tp2 = smApplier.apply(tp1);
        Type tp3 = typeMap.apply(tp2);
        return tp3;
    }
}


/**
 * Superclass for tree copiers. Takes care of duplicating symbols and
 * types when needed.
 *
 * @author Michel Schinz
 * @version 1.0
 */
public class AttributedTreeCopier extends SubstTransformer {
    public AttributedTreeCopier(Global global, TreeFactory make) {
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

            Type oldType = oldSym.info();
            Type newType = transformType(oldType);

            newSym.setInfo(newType);
        }

        return newTree;
    }
}
