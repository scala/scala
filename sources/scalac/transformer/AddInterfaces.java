/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

// TODO find a good way to change symbol flags (PRIVATE, DEFERRED,
// INTERFACE). In particular find how to make sure that the
// modifications are not made too early, for example before the symbol
// is cloned.

package scalac.transformer;

import scalac.*;
import ch.epfl.lamp.util.*;
import scalac.ast.*;
import scalac.symtab.*;
import scalac.util.*;
import Tree.*;

import java.util.*;

/**
 * Add, for each class, an interface with the same name, to be used
 * later by mixin expansion. More specifically:
 *
 * - at the end of the name of every class, the string "$class" is
 *   added,
 *
 * - an interface with the original name of the class is created, and
 *   contains all directly bound members of the class (as abstract
 *   members),
 *
 * - the interface is added to the mixin base classes of the class.
 *
 * @author Michel Schinz
 * @version 1.0
 */

class AddInterfaces extends Transformer {
    protected AddInterfacesPhase phase;
    protected Definitions defs;

    public AddInterfaces(Global global, AddInterfacesPhase phase) {
        super(global, global.make);
        this.phase = phase;
        this.defs = global.definitions;
    }

    protected LinkedList/*<Pair<Symbol,Symbol>>*/ ownerSubstStack =
        new LinkedList();
    protected Pair/*<Symbol,Symbol>*/ ownerSubst = null;
    protected SymbolSubstTypeMap typeSubst = null;
    protected Type.SubstThisMap thisTypeSubst = null;

    protected LinkedList/*<List<Tree>>*/ bodyStack = new LinkedList();

    public Tree[] transform(Tree[] trees) {
        List newTrees = new ArrayList();

        bodyStack.addFirst(newTrees);

        for (int i = 0; i < trees.length; ++i)
            newTrees.add(transform(trees[i]));

        bodyStack.removeFirst();

        return (Tree[]) newTrees.toArray(new Tree[newTrees.size()]);
    }

    public Tree transform(Tree tree) {
        // Update tree type, to take into account the new (type)
        // symbols of enclosing classes / methods.
        Type newTp = tree.type();
        if (typeSubst != null) newTp = typeSubst.apply(newTp);
        if (thisTypeSubst != null) newTp = thisTypeSubst.apply(newTp);
        tree.setType(newTp);

        if (tree.definesSymbol() && !(tree instanceof ClassDef)) {
            // Update symbol's owner, if needed.
            Symbol sym = tree.symbol();
            if (ownerSubst != null && ownerSubst.fst == sym.owner())
                sym.setOwner((Symbol)ownerSubst.snd);

            // Update symbol's type. Do that only for symbols which do
            // not belong to a class, since the type of these (i.e.
            // class members) has been changed during cloning
            // operation.
            if (! (sym.owner().isClass() || thisTypeSubst == null))
                sym.updateInfo(typeSubst.apply(sym.info()));
        }

        switch (tree) {
        case ClassDef(_,_,_,_,_,_): {
            Symbol sym = tree.symbol();
            if (phase.needInterface(sym)) {
                ClassDef classDef = (ClassDef)tree;
                Symbol ifaceSym = sym;
                Symbol classSym = phase.getClassSymbol(ifaceSym);

                List/*<Tree>*/ enclosingBody = (List)bodyStack.getFirst();
                enclosingBody.add(makeInterface(classDef));

                typeSubst = phase.getClassSubst(classSym);
                Tree newTree = makeClass(classDef);
                typeSubst = null;

                return newTree;
            } else
                return super.transform(tree);
        }

        case DefDef(int mods,
                    Name name,
                    AbsTypeDef[] tparams,
                    ValDef[][] vparams,
                    Tree tpe,
                    Tree rhs): {
            Symbol sym = tree.symbol();
            Tree newTree;

            Symbol owner;
            if (sym.isConstructor())
                owner = sym.constructorClass();
            else
                owner = sym.owner();

            if (owner.isClass() && phase.needInterface(owner)) {
                Symbol classOwner = phase.getClassSymbol(owner);
                Map ownerMemberMap = phase.getClassMemberMap(classOwner);
                Symbol newSym = (Symbol)ownerMemberMap.get(sym);
                assert newSym != null
                    : Debug.show(sym) + " not in " + ownerMemberMap;

                global.nextPhase();
                typeSubst.insertSymbol(sym.typeParams(), newSym.typeParams());
                if (!sym.isConstructor())
                    typeSubst.insertSymbol(sym.valueParams(), newSym.valueParams());
                global.prevPhase();

                pushOwnerSubst(sym, newSym);

                newTree = gen.DefDef(newSym, transform(rhs));

                popOwnerSubst();

                typeSubst.removeSymbol(sym.valueParams());
                typeSubst.removeSymbol(sym.typeParams());
            } else
                newTree = super.transform(tree);
            return newTree;
        }

        case This(_): {
            // Use class symbol for references to "this".
            Symbol classThisSym = phase.getClassSymbol(tree.symbol());
            return gen.This(tree.pos, classThisSym);
        }

        case Select(Super(_, _), _): {
            // Use class member symbols for references to "super".

            Symbol sym = tree.symbol();
            Symbol classOwner = phase.getClassSymbol(sym.owner());
            Map ownerMemberMap = phase.getClassMemberMap(classOwner);
            if (ownerMemberMap != null && ownerMemberMap.containsKey(sym)) {
                Symbol newSym = (Symbol)ownerMemberMap.get(sym);
                return gen.Select(((Select)tree).qualifier, newSym);
            } else
                return super.transform(tree);
        }

        case Select(Tree qualifier, _): {
            Symbol sym = tree.symbol();
            if (sym.isConstructor()) {
                // If the constructor now refers to the interface
                // constructor, use the class constructor instead.
                Symbol clsSym = sym.constructorClass();
                if (phase.needInterface(clsSym)) {
                    Symbol realClsSym = phase.getClassSymbol(clsSym);
                    Map memMap = phase.getClassMemberMap(realClsSym);
                    assert memMap != null
                        : Debug.show(clsSym) + " " + Debug.show(realClsSym);
                    return gen.Select(qualifier, (Symbol)memMap.get(sym));
                } else
                    return super.transform(tree);
            } else {
                qualifier = transform(qualifier);
                Symbol owner = sym.owner();
                if (owner.isClass()
                    && owner.isJava() && !owner.isInterface()
                    && owner != defs.ANY_CLASS
                    && owner != defs.ANYREF_CLASS
                    && owner != defs.JAVA_OBJECT_CLASS) {
                    Type qualifierType = qualifier.type().bound();
                    if (phase.needInterface(qualifierType.symbol())) {
                        Type castType = qualifierType.baseType(owner);
                        qualifier =
                            gen.Apply(
                                gen.TypeApply(
                                    gen.Select(qualifier, defs.AS),
                                    new Tree[] {
                                        gen.mkType(tree.pos, castType)}),
                                Tree.EMPTY_ARRAY);
                    }
                }
                return copy.Select(tree, sym, qualifier);
            }
        }

        case Ident(_): {
            Symbol sym = tree.symbol();
            if (sym.isConstructor()) {
                // If the constructor now refers to the interface
                // constructor, use the class constructor instead.
                Symbol clsSym = sym.constructorClass();
                if (phase.needInterface(clsSym)) {
                    Symbol realClsSym = phase.getClassSymbol(clsSym);
                    Map memMap = phase.getClassMemberMap(realClsSym);
                    assert memMap != null
                        : Debug.show(clsSym) + " " + Debug.show(realClsSym);
                    assert memMap.containsKey(sym)
                        : Debug.show(sym) + " not in " + memMap;
                    return gen.Ident((Symbol)memMap.get(sym));
                } else
                    return super.transform(tree);
            } else if (typeSubst != null) {
                Symbol newSym = (Symbol)typeSubst.lookupSymbol(tree.symbol());
                if (newSym != null)
                    return gen.Ident(newSym);
                else
                    return super.transform(tree);
            } else {
                return super.transform(tree);
            }
        }

        case New(Template templ): {
            Tree.New newTree = (Tree.New)super.transform(tree);
            Symbol ifaceSym = newTree.type.unalias().symbol();
            if (phase.needInterface(ifaceSym)) {
                Map clsMap = new HashMap();
                Symbol classSym = phase.getClassSymbol(ifaceSym);
                clsMap.put(ifaceSym, classSym);
                clsMap.put(ifaceSym.primaryConstructor(), classSym.primaryConstructor());

                SymbolSubstTypeMap clsSubst =
                    new SymbolSubstTypeMap(clsMap, Collections.EMPTY_MAP);

                newTree.setType(clsSubst.apply(newTree.type));
                newTree.templ.setType(clsSubst.apply(newTree.templ.type));
                Tree.Apply parent = (Tree.Apply)newTree.templ.parents[0];
                parent.setType(clsSubst.apply(parent.type));
                parent.fun.setType(clsSubst.apply(parent.fun.type));
            }
            return newTree;
        }

        case Template(_,_): {
            Symbol sym = tree.symbol();
            if (sym != Symbol.NONE) {
                Symbol newDummySymbol = sym.cloneSymbol();
                pushOwnerSubst(sym, newDummySymbol);
                Tree newTemplate = super.transform(tree);
                popOwnerSubst();
                return newTemplate;
            } else
                return super.transform(tree);
        }

        default:
            return super.transform(tree);
        }
    }

    protected Tree makeInterface(ClassDef classDef) {
        Template classImpl = classDef.impl;
        Tree[] classBody = classImpl.body;
        TreeList ifaceBody = new TreeList();

        for (int i = 0; i < classBody.length; ++i) {
            Tree t = classBody[i];
            Symbol tSym = t.symbol();

            if (!t.definesSymbol() || !phase.memberGoesInInterface(tSym))
                continue;

            if (tSym.isClass())
                ifaceBody.append(transform(new Tree[] { t }));
            else if (tSym.isType())
                ifaceBody.append(t);
            else if (tSym.isMethod())
                ifaceBody.append(gen.DefDef(tSym, Tree.Empty));
            else
                throw Debug.abort("don't know what to do with this ", t);
        }

        return gen.ClassDef(classDef.symbol(), ifaceBody.toArray());
    }

    protected Tree makeClass(ClassDef classDef) {
        Symbol ifaceSym = classDef.symbol();
        Symbol classSym = phase.getClassSymbol(ifaceSym);

        TreeList newClassBody = new TreeList();
        Template classImpl = classDef.impl;
        Tree[] classBody = classImpl.body;

        Map classMemberMap = phase.getClassMemberMap(classSym);

        assert thisTypeSubst == null;
        thisTypeSubst = new Type.SubstThisMap(ifaceSym, classSym);

        for (int i = 0; i < classBody.length; ++i) {
            Tree t = classBody[i];
            Symbol tSym = t.symbol();

            if (t.definesSymbol() && !(classMemberMap.containsKey(tSym)
                                       || tSym.isConstructor()))
                continue;

            Tree newT = transform(t);

            if (t.definesSymbol() && classMemberMap.containsKey(tSym))
                newT.setSymbol((Symbol)classMemberMap.get(tSym));

            newClassBody.append(newT);
        }

        thisTypeSubst = null;

        Tree[][] oldParentArgs = extractParentArgs(classImpl.parents);
        Tree[][] parentArgs = new Tree[oldParentArgs.length + 1][];
        System.arraycopy(oldParentArgs, 0, parentArgs, 0, oldParentArgs.length);
        parentArgs[oldParentArgs.length] = Tree.EMPTY_ARRAY;
        global.nextPhase();
        Type[] newParents = classSym.parents();
        global.prevPhase();
        Tree[] newClassParents =
            gen.mkParentConstrs(classDef.pos, newParents, parentArgs);

        Symbol local = classDef.impl.symbol();
        local.setOwner(classSym);
        return gen.ClassDef(classSym, newClassParents, local, newClassBody.toArray());
    }

    protected Tree[][] extractParentArgs(Tree[] parents) {
        Tree[][] pArgs = new Tree[parents.length][];
        for (int i = 0; i < parents.length; ++i) {
            switch(parents[i]) {
            case Apply(_, Tree[] args): pArgs[i] = transform(args); break;
            default: throw Debug.abort("unexpected parent constr. ", parents[i]);
            }
        }
        return pArgs;
    }

    protected void pushOwnerSubst(Symbol from, Symbol to) {
        ownerSubstStack.addFirst(ownerSubst);
        ownerSubst = new Pair(from, to);
    }

    protected void popOwnerSubst() {
        ownerSubst = (Pair)ownerSubstStack.removeFirst();
    }

}
