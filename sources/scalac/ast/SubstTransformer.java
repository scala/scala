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

    final protected Type.Map typeMap =
        new Type.Map() {
            public Type apply(Type t) {
                return t.subst(typeMapFormals, typeMapActuals);
            }
        };

    protected final TreeCopyFactory simpleCopy;

    public SubstTransformer(Global global, TreeFactory make) {
        super(global, make, new TCF(make));
        this.simpleCopy = new StrictTreeFactory(make);

        ((TCF)copy).setTransformer(this);
    }

    public boolean mustSubstituteSymbol(Tree tree) {
        return true;
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
        ssStack.addLast(map);
        updateSymbolSubst();
    }

    public void popSymbolSubst() {
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
        assert from.length == to.length;
        tmfStack.addLast(from);
        tmaStack.addLast(to);
        updateTypeSubst();
    }

    public void popTypeSubst() {
        tmfStack.removeLast();
        tmaStack.removeLast();
        updateTypeSubst();
    }

    public void clearTypeSubst() {
        tmfStack.clear();
        tmaStack.clear();
        updateTypeSubst();
    }

    public Tree transform(Tree tree) {
        Tree newTree = super.transform(tree);
        return syncTree(newTree);
    }

    // Update the tree so that:
    //   1. names reflect the ones in symbols,
    //   2. types reflect the ones in symbols.
    //   3. modifiers reflect the ones in symbols.
    public Tree syncTree(Tree tree) {
        Name newName = null;
        Type newType = null;
        int newMods = -1;

        if (tree.hasSymbol()) {
            Symbol sym = tree.symbol();

            newName = sym.name;
            newType = smApplier.apply(typeMap.apply(sym.nextInfo()));
            newMods = sym.flags;
        }

        switch (tree) {
        case ClassDef(_, // fix Emacs :
                      _,
                      Tree.TypeDef[] tparams,
                      Tree.ValDef[][] vparams,
                      Tree tpe,
                      Tree.Template impl) :
            return simpleCopy
                .ClassDef(tree, newMods, newName, tparams, vparams, tpe, impl);

        case ModuleDef(_, _, Tree tpe, Template impl):
            return simpleCopy.ModuleDef(tree,
                                        newMods,
                                        newName,
                                        gen.mkType(tpe.pos, newType),
                                        impl);

        case ValDef(int mods, Name name, Tree tpe, Tree rhs):
            return simpleCopy.ValDef(tree,
                                     newMods,
                                     newName,
                                     gen.mkType(tpe.pos, newType),
                                     rhs);

        case DefDef(_,          // fix for Emacs :
                    Name name,
                    Tree.TypeDef[] tparams,
                    Tree.ValDef[][] vparams,
                    Tree tpe,
                    Tree rhs):
            return simpleCopy.DefDef(tree,
                                     newMods,
                                     newName,
                                     tparams,
                                     vparams,
                                     gen.mkType(tpe.pos, newType.resultType()),
                                     rhs);

        case Select(Tree qualifier, _):
            return simpleCopy.Select(tree, qualifier, newName);

        case Ident(_):
            return simpleCopy.Ident(tree, newName);

            // TODO add a case for TypeDef?

        case Typed(Tree expr, Tree tpe): {
            Type newType2 =
                smApplier.apply(((Tree.Typed)tree).tpe.type);
            return simpleCopy.Typed(tree,
                                    expr,
                                    gen.mkType(tpe.pos, newType2));
        }

        default:
            return tree;
        }
    }

    //////////////////////////////////////////////////////////////////////

    public static class TCF extends StrictTreeFactory {
        protected SubstTransformer transformer;
        protected Map/*<Symbol,Symbol>*/ symbolMap;
        protected SymbolMapApplier smApplier;
        protected Type.Map typeMap;

        public TCF(TreeFactory make) {
            super(make);
        }

        public void setTransformer(SubstTransformer transformer) {
            this.transformer = transformer;
            this.symbolMap = transformer.symbolMap;
            this.smApplier = transformer.smApplier;
            this.typeMap = transformer.typeMap;
        }

        public void attribute(Tree newTree, Tree oldTree) {
            if (oldTree.hasSymbol()) {
                Symbol oldSym = oldTree.symbol();

                if (transformer.mustSubstituteSymbol(oldTree)
                    && symbolMap.containsKey(oldSym)) {
                    newTree.setSymbol((Symbol)symbolMap.get(oldSym));
                } else
                    newTree.setSymbol(oldTree.symbol());
            }

            newTree.type = smApplier.apply(typeMap.apply(oldTree.type));
        }
    }
}
