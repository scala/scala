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

// [...] do not copy hidden members which are not accessible via
//       "super"
// [...] handle overloaded symbols

public class ExpandMixins extends Transformer {
    // Mapping from (class) symbols to their definition.
    protected final Map/*<Symbol,Tree>*/ classDefs;

    protected final FreshNameCreator freshNameCreator;
    protected final Map interfaceToClass;
    protected final Map classToInterface;

    protected final static int PRIVATE_FINAL = Modifiers.FINAL | Modifiers.PRIVATE;

    protected final TreeCopier treeCopier;
    protected final Definitions defs;

    public ExpandMixins(Global global, ExpandMixinsPhase descr) {
        super(global);
        defs = global.definitions;

        classToInterface = global.PHASE.ADDINTERFACES.classToInterface;
        interfaceToClass = global.PHASE.ADDINTERFACES.interfaceToClass;

        classDefs = descr.classDefs;

        freshNameCreator = global.freshNameCreator;

        treeCopier = new TreeCopier(global, global.make) {
                // Substitute symbols refering to this class only.
                public boolean mustSubstituteSymbol(Tree tree) {
                    switch (tree) {
                    case Ident(_):
                    case Select(This(_), _):
                        return true;

                    default:
                        return mustCopySymbol(tree);
                    }
                }
            };
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
            Symbol s;
            if (interfaceToClass.containsKey(sym))
                s = (Symbol)interfaceToClass.get(sym);
            else
                s = sym;

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

    protected Symbol renameSymbol(Map symbolMap, Symbol oldSymbol) {
        Name newName = freshNameCreator.newName(oldSymbol.name);
        if (oldSymbol.name.isTypeName()) newName = newName.toTypeName();
        else if (oldSymbol.name.isConstrName()) newName = newName.toConstrName();
        Symbol newSymbol = oldSymbol.cloneSymbol();
        newSymbol.name = newName;
        symbolMap.put(oldSymbol, newSymbol);

        return newSymbol;
    }

    protected Map/*<Template,Template>*/ expansions = new HashMap();

    protected Template getMixinExpandedTemplate(Template tree, Symbol owner) {
        if (! expansions.containsKey(tree))
            expansions.put(tree, expandMixins(tree, owner));
        return (Template)expansions.get(tree);
    }

    protected Template expandMixins(Template tree, Symbol owner) {
        Type templType = tree.type;

        List/*<Tree>*/ newBody = new ArrayList();
	Scope newMembers = new Scope();

        Map mixedInSymbols/*<Symbol,Symbol>*/ = new HashMap();

        Symbol newTemplSymbol = tree.symbol().cloneSymbol();

        // Start by copying the statement sequence.
        Tree[] body = tree.body;
        for (int i = 0; i < body.length; ++i) {
            Tree stat = body[i];
            newBody.add(transform(stat));

            if (stat.hasSymbol()) {
		Symbol sym = stat.symbol();
		newMembers.enter(sym);
            }
        }

        Type[] baseTypes = tree.type.parents();
        global.log("baseTypes = <" + ArrayApply.toString(baseTypes) + ">");

        // Then go over the mixins and mix them in.
        for (int bcIndex = tree.parents.length - 1; bcIndex > 0; --bcIndex) {
            Tree bc = tree.parents[bcIndex];

            Symbol bcSym = baseTypes[bcIndex].symbol();
            Type bcType = bcSym.type();

            if ((bcSym.flags & Modifiers.INTERFACE) != 0)
                continue;

            assert classDefs.containsKey(bcSym) : bcSym;
            ClassDef bcDef = (ClassDef)classDefs.get(bcSym);

            Map symbolMap/*<Symbol,Symbol>*/ = new HashMap();

            // Create substitution for mixin's type parameters.
            Object[] ts = typeSubst(baseTypes[bcIndex]);
            assert ts.length == 2;
            final Symbol[] tpFormals = (Symbol[])ts[0];
            final Type[] tpActuals = (Type[])ts[1];
            assert tpFormals.length == tpActuals.length;
            Type.Map typeMap = new Type.Map() {
                    public Type apply(Type t) {
                        return t.subst(tpFormals, tpActuals);
                    }
                };

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
                        renameSymbol(symbolMap, formal.symbol());
                    memberSymbol.setOwner(owner);
                    Type memberType = typeMap.apply(formal.tpe.type());
                    memberSymbol.updateInfo(memberType);

                    Tree memberDef = gen.ValDef(memberSymbol, actual);
                    newBody.add(memberDef);
                }
            }

            Template mixin = getMixinExpandedTemplate(bcDef.impl, bcSym);
            Tree[] mixinBody = mixin.body;
            Set/*<Tree>*/ leftOutMembers = new HashSet();

            // Pass 1: compute members to rename.
            for (int m = 0; m < mixinBody.length; ++m) {
                Tree member = mixinBody[m];

                if (!member.hasSymbol())
                    continue;

                Symbol memSym = member.symbol();
                Name memName = memSym.name;

                // Check if we have to import this member. To do this,
                // we lookup the member both in the template and in
                // the mixin, and if the result is the same, we import
                // the member (otherwise it means it's shadowed).

                Symbol memSymT = templType.lookupNonPrivate(memName);
                Symbol memSymM = bcType.lookupNonPrivate(memName);

                if (memSymT != memSymM) {
                    if ((memSym.flags & Modifiers.DEFERRED) != 0)
                        leftOutMembers.add(member);
                    else
                        renameSymbol(symbolMap, memSym);
                }
            }

            // Pass 2: copy members
            for (int m = 0; m < mixinBody.length; ++m) {
                Tree member = mixinBody[m];

                if (leftOutMembers.contains(member))
                    continue;

                treeCopier.pushSymbolSubst(symbolMap);
                treeCopier.pushTypeSubst(tpFormals, tpActuals);
                Tree newMember = treeCopier.copy(member);
                treeCopier.popTypeSubst();
                treeCopier.popSymbolSubst();

                newBody.add(newMember);

                if (newMember.hasSymbol()) {
		    Symbol sym = newMember.symbol();

                    sym.setOwner(owner);
                    newMembers.enter(sym);

                    mixedInSymbols.put(member.symbol(), newMember.symbol());
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
		if (!Modifiers.Helper.isInterface(sym.flags) && i > 0) {
                    assert classToInterface.containsKey(sym) : sym;
                    sym = (Symbol)classToInterface.get(sym);
                }

                newBaseClasses[i] =
                    gen.mkParentConstr(tree.pos, new Type.TypeRef(pre, sym, args));
		newBaseTypes[i] = new Type.TypeRef(pre, sym, args);
	    } break;

	    default:
		throw global.fail("invalid base class type", baseTypes[i]);
	    }
	}

        // Use correct symbols for mixed-in members.
        SymbolFixer symbolFixer = new SymbolFixer(global, mixedInSymbols);
        Tree[] fixedBody =
            symbolFixer.transform((Tree[])newBody.toArray(new Tree[newBody.size()]));
        Template newTree = make.Template(tree.pos, newBaseClasses, fixedBody);
        newTree.setSymbol(newTemplSymbol);
	newTree.setType(Type.compoundType(newBaseTypes, newMembers, owner));

        return newTree;
    }

    public Tree transform(Tree tree) {
        switch (tree) {
        case ClassDef(int mods,
                      Name name,
                      TypeDef[] tparams,
                      ValDef[][] vparams,
                      Tree tpe,
                      Template impl):
            if (Modifiers.Helper.isInterface(mods))
                return super.transform(tree);
            else {
                global.log("expanding " + name);
		Tree.ClassDef newClass = (Tree.ClassDef)
                    copy.ClassDef(tree,
                                  mods,
                                  name,
                                  super.transform(tparams),
                                  super.transform(vparams),
                                  super.transform(tpe),
                                  getMixinExpandedTemplate(impl, tree.symbol()));
		newClass.symbol().updateInfo(newClass.impl.type);
		return newClass;
	    }

        default:
            Tree newTree = super.transform(tree);

            switch (newTree) {
            case Apply(Select(Tree qualifier, Name selector), Tree[] args): {
                Tree fun = ((Tree.Apply)newTree).fun;
                Symbol funOwnerSym = fun.symbol().owner();
                Symbol qualSym = qualifier.type.symbol().moduleClass();
                if (! (qualifier instanceof Tree.Super
                       || qualSym.isSubClass(funOwnerSym))) {
		    global.log("inserting cast from " + qualSym + " to " + funOwnerSym);//debug
                    Type ownerTp = funOwnerSym.type();
                    Tree castQualifier =
                        gen.Apply(gen.TypeApply(gen.Select(qualifier, defs.AS),
                                                new Tree[] {
                                                    gen.mkType(qualifier.pos, ownerTp)
                                                }),
                                  Tree.EMPTY_ARRAY);
                    return copy.Apply(newTree,
                                      copy.Select(fun, castQualifier, selector),
                                      args);
                } else
                    return newTree;
            }
            default:
                return newTree;
            }
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

    protected static class SymbolFixer extends Transformer {
        protected final Map/*<Symbol,Symbol>*/ mixedInSymbols;

        public SymbolFixer(Global global, Map mixedInSymbols) {
            super(global);
            this.mixedInSymbols = mixedInSymbols;
        }

        public Tree transform(Tree tree) {
            switch (tree) {
            case Ident(_): {
                Symbol sym = tree.symbol();
                if (mixedInSymbols.containsKey(sym))
                    return gen.Ident((Symbol)mixedInSymbols.get(sym));
                else
                    return super.transform(tree);
            }

            case Select(Super(Tree tpe), Name selector): {
                Symbol sym = tree.symbol();
                if (mixedInSymbols.containsKey(sym))
                    return gen.Ident((Symbol)mixedInSymbols.get(sym));
                else
                    return super.transform(tree);
            }

            default:
                return super.transform(tree);
            }
        }
    }
}
