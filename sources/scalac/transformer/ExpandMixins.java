/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: ExpandMixins.java,v 1.24 2002/11/11 16:08:50 schinz Exp $
// $Id$

package scalac.transformer;

import scalac.*;
import scalac.util.*;
import scalac.ast.*;
import scalac.symtab.*;
import java.util.*;
import java.util.Arrays;
import Tree.*;

/**
 * A transformer to expand mixins using code copying. We assume that
 * links to outer classes have been made explicit by a previous phase.
 *
 * @author Michel Schinz
 * @version 1.0
 */

// TODO do not copy hidden members which are not accessible via
// "super".

// TODO also substitute type parameters of classes in which the mixin
// is nested, if any. Do the same for the substitution of symbols in
// ThisTypes.

public class ExpandMixins extends Transformer {
    // Mapping from (class) symbols to their definition.
    protected final Map/*<Symbol,Tree>*/ classDefs;

    protected final Definitions defs;

    protected final AddInterfacesPhase addInterfaces;

    public ExpandMixins(Global global, ExpandMixinsPhase descr) {
        super(global);
        defs = global.definitions;

        classDefs = descr.classDefs;

        addInterfaces = global.PHASE.ADDINTERFACES;
    }

    public void apply() {
        ClassDefCollector collector = new ClassDefCollector(classDefs);

        for (int i = 0; i < global.units.length; i++) {
	    Unit unit = global.units[i];
	    for (int j = 0; j < unit.body.length; ++j)
		collector.traverse(unit.body[j]);
	}

        super.apply();
    }

    protected void typeSubst(Type type, ArrayList f, ArrayList a) {
        switch (type) {
        case TypeRef(Type pre, Symbol sym, Type[] args): {
            Symbol s = addInterfaces.getClassSymbol(sym);
            f.addAll(Arrays.asList(s.typeParams()));
            a.addAll(Arrays.asList(args));
            typeSubst(pre, f, a);
        } break;
        default:
            ;                   // nothing to do
        }
    }

    protected Object[] typeSubst(Type type) {
        ArrayList/*<Symbol[]>*/ f = new ArrayList();
        ArrayList/*<Type[]>*/ a = new ArrayList();
        typeSubst(type, f, a);
        return new Object[] {
            f.toArray(new Symbol[f.size()]), a.toArray(new Type[a.size()])
        };
    }

    protected void getArgsSection(Tree tree, List s) {
        switch(tree) {
        case Apply(Tree fun, Tree[] args):
            getArgsSection(fun, s);
            s.add(args);
            break;

        default:
            ;                   // nothing to do
        }
    }

    protected Tree[][] getArgsSection(Tree tree) {
        List s = new ArrayList();
        getArgsSection(tree, s);
        return (Tree[][])s.toArray(new Tree[s.size()][]);
    }

    protected Map/*<Template,Template>*/ expansions = new HashMap();

    protected Template getMixinExpandedTemplate(Template tree, Symbol owner) {
        if (! expansions.containsKey(tree))
            expansions.put(tree, expandMixins(tree, owner));
        return (Template)expansions.get(tree);
    }

    protected Template expandMixins(Template tree, final Symbol owner) {
        Type templType = owner.info();

        List/*<Tree>*/ newBody = new ArrayList();
	Scope newMembers = new Scope();

        Symbol newTemplSymbol = tree.symbol().cloneSymbol();

        // Start by copying the statement sequence.
        Tree[] body = tree.body;
        for (int i = 0; i < body.length; ++i) {
            Tree stat = body[i];
            newBody.add(transform(stat));

            if (stat.definesSymbol()) {
		Symbol sym = stat.symbol();
		newMembers.enterOrOverload(sym);
            }
        }

        Type[] baseTypes = owner.parents();
        global.log("baseTypes for " + Debug.show(owner)
                   + " = <" + ArrayApply.toString(baseTypes) + ">");

        // Then go over the mixins and mix them in.
        SymbolCloner symbolCloner =
            new SymbolCloner(global.freshNameCreator);
        for (int bcIndex = tree.parents.length - 1; bcIndex > 0; --bcIndex) {
            SymbolSubstTypeMap typeCloner = new SymbolSubstTypeMap();
            Tree bc = tree.parents[bcIndex];

            final Symbol bcSym = baseTypes[bcIndex].symbol();
            symbolCloner.owners.put(bcSym, owner);
            symbolCloner.owners.put(bcSym.constructor(), owner);

            if ((bcSym.flags & Modifiers.INTERFACE) != 0)
                continue;

            assert classDefs.containsKey(bcSym) : bcSym;
            ClassDef bcDef = (ClassDef)classDefs.get(bcSym);

            // Create substitution for mixin's type parameters.
            Object[] ts = typeSubst(baseTypes[bcIndex]);
            assert ts.length == 2;
            final Symbol[] tpFormals = (Symbol[])ts[0];
            final Type[] tpActuals = (Type[])ts[1];
            assert tpFormals.length == tpActuals.length;
            Type.Map typeMap = new Type.Map() {
                    public Type apply(Type t) {
                        Type t1 = t.asSeenFrom(owner.thisType(), bcSym);
                        Type t2 = t1.subst(tpFormals, tpActuals);
                        return t2;
                    }
                };
            typeCloner.insertType(tpFormals, tpActuals);

            // Create private fields for mixin's value parameters.
            Tree[][] actuals = getArgsSection(bc);
            assert bcDef.vparams.length == actuals.length;
            for (int s = 0; s < bcDef.vparams.length; ++s) {
                ValDef[] sectionF = bcDef.vparams[s];
                Tree[] sectionA = actuals[s];

                assert sectionF.length == sectionA.length;

                for (int p = 0; p < sectionF.length; ++p) {
                    // We do not need to copy the actual parameters,
                    // since they are removed from their original
                    // location anyway.
                    ValDef formal = sectionF[p];
                    Tree actual = sectionA[p];

                    Symbol memberSymbol =
                        symbolCloner.cloneSymbol(formal.symbol(), true);
                    Type memberType = typeMap.apply(formal.tpe.type());
                    memberSymbol.updateInfo(memberType);

                    Tree memberDef = gen.ValDef(memberSymbol, actual);
                    newBody.add(memberDef);
                    typeCloner.insertSymbol(formal.symbol(), memberSymbol);
                }
            }

            Template mixin = getMixinExpandedTemplate(bcDef.impl, bcSym);
            Type bcType = mixin.type();
            Tree[] mixinBody = mixin.body;
            Map newNames = new HashMap();

            // Pass 1: compute members to rename.
            for (int m = 0; m < mixinBody.length; ++m) {
                Tree member = mixinBody[m];
                if (!member.definesSymbol()) continue;
                Symbol symbol = member.symbol();
                Name newName = (Name)newNames.get(symbol.name);
                boolean shadowed = newName == null &&
                    newMembers.lookup(symbol.name) != Symbol.NONE;
                if (shadowed && symbol.isDeferred()) continue;
                Symbol clone = symbolCloner.cloneSymbol(symbol, shadowed);
                if (newName != null)
                    clone.name = newName;
                else
                    newNames.put(symbol.name, clone.name);
                typeCloner.insertSymbol(symbol, clone);
                newMembers.enterOrOverload(clone);
            }

            // Pass 2: copy members
            TreeSymbolCloner treeSymbolCloner =
                new TreeSymbolCloner(symbolCloner);
            TreeCloner treeCloner = new TreeCloner(
                global, symbolCloner.clones, typeCloner);
            for (int m = 0; m < mixinBody.length; ++m) {
                Tree member = mixinBody[m];
                if (symbolCloner.clones.containsKey(member.symbol())) {
                    treeSymbolCloner.traverse(member);
                    newBody.add(treeCloner.transform(member));
                }
            }
        }

	// Modify mixin base classes to refer to interfaces instead of
	// real classes.
	Type[] newBaseTypes = new Type[baseTypes.length];
	Tree[] newBaseClasses = new Tree[tree.parents.length];
        newBaseTypes[0] = baseTypes[0];
        newBaseClasses[0] = tree.parents[0];
	for (int i = 1; i < baseTypes.length; ++i) {
	    switch (baseTypes[i]) {
	    case TypeRef(Type pre, Symbol sym, Type[] args): {
		if (!Modifiers.Helper.isInterface(sym.flags))
                    sym = addInterfaces.getInterfaceSymbol(sym);

                newBaseClasses[i] =
                    gen.mkParentConstr(tree.pos, new Type.TypeRef(pre, sym, args));
		newBaseTypes[i] = new Type.TypeRef(pre, sym, args);
	    } break;

	    default:
		throw global.fail("invalid base class type", baseTypes[i]);
	    }
	}

        Tree[] fixedBody = (Tree[])newBody.toArray(new Tree[newBody.size()]);
        SuperFixer superFixer = new SuperFixer(
            global, owner, symbolCloner.clones);
        for (int i = 0; i < body.length; ++i) {
            fixedBody[i] = superFixer.transform(fixedBody[i]);
        }
        Template newTree = make.Template(tree.pos, newBaseClasses, fixedBody);
        newTree.setSymbol(newTemplSymbol);
	newTree.setType(Type.compoundType(newBaseTypes, newMembers, owner));

        return newTree;
    }

    public Tree transform(Tree tree) {
        switch (tree) {
        case ClassDef(_,
                      _,
                      TypeDef[] tparams,
                      ValDef[][] vparams,
                      Tree tpe,
                      Template impl):
            Symbol sym = tree.symbol();
            if (Modifiers.Helper.isInterface(sym.flags))
                return super.transform(tree);
            else {
                global.log("expanding " + Debug.show(tree.symbol()));
		Tree.ClassDef newClass = (Tree.ClassDef)
                    copy.ClassDef(tree,
                                  sym,
                                  super.transform(tparams),
                                  super.transform(vparams),
                                  super.transform(tpe),
                                  getMixinExpandedTemplate(impl, sym));
		newClass.symbol().updateInfo(newClass.impl.type);
		return newClass;
	    }

        default:
            return super.transform(tree);
        }
    }

    //########################################################################

    // Return a hash table associating class definitions to (class) symbols.
    protected static class ClassDefCollector extends Traverser {
        private Map map;

        public ClassDefCollector(Map map) {
            this.map = map;
        }

        public void traverse(Tree tree) {
            switch(tree) {
            case ClassDef(_, _, _, _, _, _):
                map.put(tree.symbol(), tree);
                break;

            default:
                ;               // nothing to do
            }
            super.traverse(tree);
        }
    }

    //########################################################################
    // Private Class - MyTreeCloner

    private static class SuperFixer extends Transformer {

        private final Symbol clasz;
        private final Map/*<Symbol,Symbol>*/ symbols;

        public SuperFixer(Global global, Symbol clasz, Map symbols) {
            super(global);
            this.clasz = clasz;
            this.symbols = symbols;
        }

        public Tree transform(Tree tree) {
            switch (tree) {
            case Select(Super(Tree qualifier), _):
                Symbol symbol = (Symbol)symbols.get(tree.symbol());
                if (symbol != null) {
                    Tree self = gen.This(((Select)tree).qualifier.pos, clasz);
                    return gen.Select(tree.pos, self, symbol);
                }
            }
            return super.transform(tree);
        }
    }

    //########################################################################
}
