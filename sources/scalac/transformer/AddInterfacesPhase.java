/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

// TODO try to use setInfo instead of updateInfo for cloned symbols,
// to avoid the need to use nextPhase/nextInfo.

package scalac.transformer;

import scalac.*;
import scalac.symtab.*;
import scalac.checkers.*;
import scalac.util.Name;
import java.util.*;
import scalac.util.Debug;

public class AddInterfacesPhase extends Phase {

    /** Initializes this instance. */
    public AddInterfacesPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
    }

    /** Applies this phase to the given compilation units. */
    public void apply(Unit[] units) {
        for (int i = 0; i < units.length; i++)
            new AddInterfaces(global, this).apply(units[i]);
    }

    public Type transformInfo(Symbol sym, Type tp) {
        if (sym.isConstructor() || sym.owner().isConstructor()) {
            return tp;
        } else if (sym.isClass() && !sym.isJava()) {
            Definitions definitions = global.definitions;
            if (sym == definitions.ANY_CLASS) return tp;
            Type[] oldParents = tp.parents();
            assert oldParents.length > 0 : Debug.show(sym);
            for (int i = 1; i < oldParents.length; ++i) {
                Symbol oldSym = oldParents[i].symbol();
                assert !oldSym.isJava() || oldSym.isInterface() :
                    Debug.show(sym) + " <: " + Debug.show(oldSym);
            }

            boolean ifaceReqrd = needInterface(sym);
            Type[] newParents;
            Scope newMembers = new Scope();

            Scope.SymbolIterator oldMembersIt =
                new Scope.UnloadIterator(tp.members().iterator());
            while (oldMembersIt.hasNext()) {
                Symbol member = oldMembersIt.next();

                if (ifaceReqrd
                    && (member.isInitializer() || !memberGoesInInterface(member)))
                    continue;

                if (member.isPrivate()) {
                    member.name = uniqueName(member);
                    member.flags ^= Modifiers.PRIVATE;
                } else if (member.isProtected())
                    member.flags ^= Modifiers.PROTECTED;

                newMembers.enterOrOverload(member);
            }

            if (ifaceReqrd) {
                // Before this phase, the symbol is a class, but after
                // it will be an interface. Its type has to be changed
                // so that:
                //
                //   1. Java classes are removed from its parents,
                //
                //   2. only members which will end up in the
                //      interface are kept, and private ones are made
                //      public and renamed.
                sym.flags |= Modifiers.INTERFACE;
                newParents = oldParents;
            } else {
                // The symbol is the one of a class which doesn't need
                // an interface. We need to fix its parents to use
                // class symbols instead of interface symbols.
                newParents = new Type[oldParents.length];
                for (int i = 0; i < oldParents.length; ++i) {
                    switch (oldParents[i]) {
                    case TypeRef(Type pre, Symbol oldSym, Type[] args):
                        newParents[i] = !needInterface(oldSym)
                            ? oldParents[i]
                            : Type.typeRef(pre, getClassSymbol(oldSym), args);
                        break;
                    default:
                        throw Debug.abort("illegal case", oldParents[i]);
                    }
                }
            }

            return Type.compoundType(newParents, newMembers, sym);
        } else if (sym.isThisSym() && hasInterfaceSymbol(sym.owner())) {
            switch (tp) {
            case TypeRef(_, _, _):
                return sym.owner().nextType();
            case CompoundType(Type[] parents, Scope members):
                parents = Type.cloneArray(parents);
                parents[parents.length - 1] = sym.owner().nextType();
                return Type.compoundTypeWithOwner(sym.owner(), parents, members);
            default:
                throw Debug.abort("illegal case", tp +" -- "+ Debug.show(sym));
            }
        } else
            return tp;
    }

    public Checker[] postCheckers(Global global) {
        return new Checker[] {
            new CheckSymbols(global),
            new CheckTypes(global),
        };
    }

    protected boolean memberGoesInInterface(Symbol member) {
        return member.isType() || member.isMethod();
    }

    protected Type removeValueParams(Type tp) {
        switch (tp) {
        case MethodType(Symbol[] vparams, Type result):
            return new Type.MethodType(Symbol.EMPTY_ARRAY, result);
        case PolyType(Symbol[] tps, Type result):
            return new Type.PolyType(tps, removeValueParams(result));
        default:
            throw Debug.abort("illegal case", tp);
        }
    }

    protected final SymbolNameWriter uniqueNameWriter = new SymbolNameWriter()
        .setAllSeparators('$')
        .setRootSeparator('\0');
    protected Name uniqueName(Symbol sym) {
        Name name = Name.fromString(uniqueNameWriter.toString(sym));
        return sym.name.isTypeName() ? name.toTypeName() : name;
    }

    // Terminology: in the following code, the symbol which was used
    // until this phase for the class, and will now be used for the
    // interface is called the "interface symbol". The symbol created
    // by this phase for the class is called the "class symbol".

    /** True iff the given class symbol needs an interface. */
    protected boolean needInterface(Symbol classSym) {
        assert classSym.isClass()
            : Debug.toString(classSym) + " is not a class (kind " + classSym.kind + ")";
        return !(classSym.isJava()
                 || classSym.isModuleClass()
                 || classSym.isAnonymousClass()
                 || hasInterfaceSymbol(classSym)
                 || classSym == global.definitions.ANY_CLASS
                 || classSym == global.definitions.ANYREF_CLASS
                 || classSym == global.definitions.ALL_CLASS
                 || classSym == global.definitions.ALLREF_CLASS);
    }

    protected final static String CLASS_SUFFIX = "$class";
    protected Name className(Name ifaceName) {
        Name className = Name.fromString(ifaceName.toString() + CLASS_SUFFIX);
        if (ifaceName.isTypeName()) return className.toTypeName();
        else return className;
    }

    protected HashMap ifaceToClass = new HashMap();
    protected HashMap classToIFace = new HashMap();

    /** Return the class symbol corresponding to the given interface
     * symbol. If the class does not need an interface, return the
     * given symbol.
     */
    protected Symbol getClassSymbol(Symbol ifaceSym) {
        assert ifaceSym.isClass(): Debug.show(ifaceSym);
        if (!needInterface(ifaceSym))
            return ifaceSym;

        Symbol classSym = (Symbol)ifaceToClass.get(ifaceSym);
        if (classSym == null) {
            classSym = ifaceSym.cloneSymbol(ifaceSym.owner());
            classSym.name = className(ifaceSym.name);
            ifaceSym.flags &= ~Modifiers.FINAL;
            classSym.flags &= ~Modifiers.INTERFACE;

            Scope ifaceOwnerMembers = ifaceSym.owner().members();
            ifaceOwnerMembers.enter(classSym);

            Type.SubstThisMap thisTypeMap =
                new Type.SubstThisMap(ifaceSym, classSym);

            // Create class substitution map.
            SymbolSubstTypeMap classSubst = newClassSubst(classSym);
            Map classMemberMap = newClassMemberMap(classSym);

            Symbol[] ifaceTParams = ifaceSym.typeParams();
            Symbol[] classTParams = classSym.typeParams();
            classSubst.insertSymbol(ifaceTParams, classTParams);

            // Clone all members, entering them in the class scope.
            Scope classMembers = new Scope();
            Scope.SymbolIterator ifaceMembersIt =
                new Scope.UnloadIterator(ifaceSym.members().iterator());
            while (ifaceMembersIt.hasNext()) {
                Symbol ifaceMemberSym = ifaceMembersIt.next();

                if (ifaceMemberSym.isType()
                    || ifaceMemberSym.isDeferred())
                    continue;

                Symbol classMemberSym;
                if (memberGoesInInterface(ifaceMemberSym)) {
                    if (ifaceMemberSym.isPrivate()) {
                        ifaceMemberSym.name = uniqueName(ifaceMemberSym);
			ifaceMemberSym.flags |= Modifiers.FINAL;
                        ifaceMemberSym.flags ^= Modifiers.PRIVATE;
                    } else if (ifaceMemberSym.isProtected())
                        ifaceMemberSym.flags ^= Modifiers.PROTECTED;

                    classMemberSym = ifaceMemberSym.cloneSymbol(classSym);
                    ifaceMemberSym.flags &= ~Modifiers.FINAL;
                    classMemberSym.setInfo(
                        thisTypeMap.applyParams(
                            classSubst.applyParams(
                                classMemberSym.info().cloneType(
                                    ifaceMemberSym, classMemberSym))));

                    ifaceMemberSym.flags |= Modifiers.DEFERRED;
                } else {
                    // Member doesn't go in interface, we just make it
                    // owned by the class.
                    classMemberSym = ifaceMemberSym;
                    classMemberSym.setOwner(classSym);
                    classMemberSym.updateInfo(
                        thisTypeMap.apply(
                            classSubst.apply(
                                classMemberSym.info())));
                }

                classMemberMap.put(ifaceMemberSym, classMemberSym);
                classMembers.enterOrOverload(classMemberSym);
            }

            // Give correct type to the class symbol by using class
            // symbols for its parents, and by adding the interface
            // among them.
            Type[] oldClassParents = classSym.parents();
            int oldParentsCount = oldClassParents.length;
            Type[] newClassParents = new Type[oldParentsCount + 1];
            for (int i = 0; i < oldParentsCount; ++i) {
                switch (oldClassParents[i]) {
                case TypeRef(Type pre, Symbol sym, Type[] args):
                    Type newTp = Type.typeRef(pre, getClassSymbol(sym), args);
                    newClassParents[i] = classSubst.apply(newTp);
                    break;
                default:
                    throw Debug.abort("unexpected type for parent", oldClassParents[i]);
                }
            }
            newClassParents[oldParentsCount] = classSubst.apply(ifaceSym.type());
            // TODO setInfo cannot be used here because the type then
            // goes through transformInfo. Maybe setInfo should behave
            // like updateInfo.
            classSym.updateInfo(Type.compoundType(newClassParents,
                                                  classMembers,
                                                  classSym));
            ifaceToClass.put(ifaceSym, classSym);
            classToIFace.put(classSym, ifaceSym);
        }
        return classSym;
    }

    public boolean hasInterfaceSymbol(Symbol classSym) {
        return classToIFace.containsKey(classSym);
    }

    public Symbol getInterfaceSymbol(Symbol classSym) {
        return (Symbol)classToIFace.get(classSym);
    }

    HashMap/*<Symbol,SymbolSubstTypeMap>*/ classSubstitutions = new HashMap();
    protected SymbolSubstTypeMap newClassSubst(Symbol classSym) {
        SymbolSubstTypeMap subst = new SymbolSubstTypeMap();
        classSubstitutions.put(classSym, subst);
        return subst;
    }

    /** Return symbol substitution for the class (a mapping from the
     * interface's type and value parameters to the class' equivalent)
     */
    public SymbolSubstTypeMap getClassSubst(Symbol classSym) {
        SymbolSubstTypeMap classSubst =
            (SymbolSubstTypeMap)classSubstitutions.get(classSym);
        assert classSubst != null;
        return classSubst;
    }

    HashMap/*<Symbol, HashMap>*/ classMemberMaps = new HashMap();
    protected HashMap newClassMemberMap(Symbol classSym) {
        HashMap map = new HashMap();
        classMemberMaps.put(classSym, map);
        return map;
    }

    /** Return symbol substitution for the class (a mapping from the
     * interface's type and value parameters to the class' equivalent)
     */
    public Map getClassMemberMap(Symbol classSym) {
        return (Map)classMemberMaps.get(classSym);
    }

}
