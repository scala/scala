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

public class AddInterfacesPhase extends PhaseDescriptor {
    public String name () {
        return "addinterfaces";
    }

    public String description () {
        return "add one interface per class";
    }

    public String taskDescription() {
        return "added interfaces";
    }

    public void apply(Global global) {
        new AddInterfaces(global, this).apply();
    }

    public Type transformInfo(Symbol sym, Type tp) {
        if (sym.isConstructor()
            && needInterface(sym.primaryConstructorClass())) {
            // The symbol is a constructor of a class which needs
            // an interface. All its value arguments have to be
            // removed.
            return removeValueParams(tp);
        } else if (sym.isClass() && !sym.isPackage()) {
            if (needInterface(sym)) {
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

                Scope.SymbolIterator oldMembersIt =
                    new Scope.UnloadIterator(tp.members().iterator());
                Scope newMembers = new Scope();
                while (oldMembersIt.hasNext()) {
                    Symbol member = oldMembersIt.next();

                    if (!memberGoesInInterface(member))
                        continue;

                    if (member.isPrivate()) {
                        member.name = uniqueName(member);
                        member.flags ^= Modifiers.PRIVATE;
                    } else if (member.isProtected())
                        member.flags ^= Modifiers.PROTECTED;

                    newMembers.enterOrOverload(member);
                }

                LinkedList/*<Type>*/ newParentsLst = new LinkedList();
                Type[] oldParents = tp.parents();
                for (int i = 0; i < oldParents.length; ++i) {
                    if (! oldParents[i].symbol().isJava())
                        newParentsLst.addLast(oldParents[i]);
                }
                Type[] newParents;
                if (newParentsLst.size() == oldParents.length)
                    newParents = oldParents;
                else
                    newParents = (Type[])
                        newParentsLst.toArray(new Type[newParentsLst.size()]);
                return Type.compoundType(newParents, newMembers, sym);
            } else {
                // The symbol is the one of a class which doesn't need
                // an interface. We need to fix its parents to use
                // class symbols instead of interface symbols.
                LinkedList/*<Type>*/ newParentsLst = new LinkedList();
                Type[] oldParents = tp.parents();
                for (int i = 0; i < oldParents.length; ++i) {
                    Symbol oldSym = oldParents[i].unalias().symbol();
                    if (needInterface(oldSym))
                        newParentsLst.addLast(getClassSymbol(oldSym).type());
                    else
                        newParentsLst.addLast(oldParents[i]);
                }
                Type[] newParents = (Type[])
                    newParentsLst.toArray(new Type[newParentsLst.size()]);
                return Type.compoundType(newParents, tp.members(), sym);
            }
        } else
            return tp;
    }

    public Checker[] postCheckers(Global global) {
        return new Checker[] {
            new CheckSymbols(global),
            new CheckTypes(global),
            new CheckOwners(global),
	    new CheckNames(global)
        };
    }

    protected boolean memberGoesInInterface(Symbol member) {
        return member.isType()
            || (member.isMethod() && !member.isPrimaryConstructor());
    }

    protected boolean memberMovesToInterface(Symbol member) {
        return member.isType();
    }

    protected Type removeValueParams(Type tp) {
        switch (tp) {
        case MethodType(Symbol[] vparams, Type result):
            return new Type.MethodType(Symbol.EMPTY_ARRAY, result);
        case PolyType(Symbol[] tps, Type result):
            return new Type.PolyType(tps, removeValueParams(result));
        default:
            return tp;
        }
    }

    protected void uniqueName(Symbol sym, StringBuffer buf) {
        Symbol owner = sym.owner();

        if (owner != Symbol.NONE) {
            uniqueName(owner, buf);
            buf.append('$');
        }

        buf.append(sym.name.toString());
    }

    protected Name uniqueName(Symbol sym) {
        StringBuffer buf = new StringBuffer();
        uniqueName(sym, buf);
        Name newName = Name.fromString(buf.toString());
        if (sym.name.isTypeName()) return newName.toTypeName();
        else if (sym.name.isConstrName()) return newName.toConstrName();
        else return newName;
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
                 || hasInterfaceSymbol(classSym));
    }

    protected final static String CLASS_SUFFIX = "$class";
    protected Name className(Name ifaceName) {
        Name className = Name.fromString(ifaceName.toString() + CLASS_SUFFIX);
        if (ifaceName.isTypeName()) return className.toTypeName();
        else if (ifaceName.isConstrName()) return className.toConstrName();
        else return className;
    }

    /** Clone symbol and set its owner immediately. */
    protected Symbol cloneSymbol(Symbol original, Symbol newOwner) {
        Symbol clone = original.cloneSymbol();
        clone.setOwner(newOwner);
        return clone;
    }

    /** Clone the symbol itself, and if it represents a method, clone
     * its type and value arguments too, then update the symbol's type
     * to use these new symbols. Also applies the given substitution
     * to the cloned symbols' type. */
    protected Symbol deepCloneSymbol(Symbol original,
                                     Symbol newOwner,
                                     SymbolSubstTypeMap map) {
        Symbol clone = cloneSymbol(original, newOwner);
        if (clone.isMethod()) {
            Symbol[] tparams = clone.typeParams();
            Symbol[] newTParams = new Symbol[tparams.length];
            SymbolSubstTypeMap symMap = new SymbolSubstTypeMap(map);
            for (int i = 0; i < tparams.length; ++i) {
                newTParams[i] = cloneSymbol(tparams[i], clone);
                symMap.insertSymbol(tparams[i], newTParams[i]);
            }

            Symbol[] vparams = clone.valueParams();
            Symbol[] newVParams = new Symbol[vparams.length];
            for (int i = 0; i < vparams.length; ++i) {
                newVParams[i] = cloneSymbol(vparams[i], clone);
                newVParams[i].updateInfo(symMap.apply(newVParams[i].info()));
                symMap.insertSymbol(vparams[i], newVParams[i]);
            }

            clone.updateInfo(substParams(clone.info(), symMap));
        }
        return clone;
    }

    protected HashMap ifaceToClass = new HashMap();
    protected HashMap classToIFace = new HashMap();

    /** Return the class symbol corresponding to the given interface
     * symbol. If the class does not need an interface, return the
     * given symbol.
     */
    protected Symbol getClassSymbol(Symbol ifaceSym) {
        if (ifaceSym.isPrimaryConstructor())
            return getClassSymbol(ifaceSym.primaryConstructorClass())
                .constructor();

        if (!needInterface(ifaceSym))
            return ifaceSym;

        Symbol classSym = (Symbol)ifaceToClass.get(ifaceSym);
        if (classSym == null) {
            classSym = cloneSymbol(ifaceSym, ifaceSym.owner());
            classSym.name = className(ifaceSym.name);
            classSym.flags &= ~Modifiers.INTERFACE;

            Symbol ifaceConstrSym = ifaceSym.constructor();
            Symbol classConstrSym = classSym.constructor();
            classConstrSym.name = className(ifaceConstrSym.name);

            Scope ifaceOwnerMembers = ifaceSym.owner().members();
            ifaceOwnerMembers.enter(classSym);
            ifaceOwnerMembers.enter(classConstrSym);

            // Clone type and value parameters of constructor.
            Map classSubst = newClassSubst(classSym);
            Symbol[] tparams = classConstrSym.typeParams();
            for (int i = 0; i < tparams.length; ++i) {
                Symbol newParam = cloneSymbol(tparams[i], classConstrSym);
                classSubst.put(tparams[i], newParam);
            }

            SymbolSubstTypeMap paramsSubst =
                new SymbolSubstTypeMap(classSubst, Collections.EMPTY_MAP);
            // Play it safe and make sure that classSubst won't be
            // modified anymore.
            classSubst = Collections.unmodifiableMap(classSubst);

            ThisTypeMap thisTpMap =
                new ThisTypeMap(ifaceSym, new Type.ThisType(classSym));

            Symbol[] vparams = classConstrSym.valueParams();
            for (int i = 0; i < vparams.length; ++i) {
                vparams[i].setOwner(classConstrSym);
                vparams[i].updateInfo(paramsSubst.apply(vparams[i].info()));
            }

            // Clone all members, entering them in the class scope.
            Map classMembersMap = newClassMemberMap(classSym);
            Scope classMembers = new Scope();
            Scope.SymbolIterator ifaceMembersIt =
                new Scope.UnloadIterator(ifaceSym.members().iterator());
            while (ifaceMembersIt.hasNext()) {
                Symbol ifaceMemberSym = ifaceMembersIt.next();

                if (memberMovesToInterface(ifaceMemberSym)
                    || ifaceMemberSym.isPrimaryConstructor())
                    continue;

                Symbol classMemberSym;
                if (memberGoesInInterface(ifaceMemberSym)) {
                    if (ifaceMemberSym.isPrivate()) {
                        ifaceMemberSym.name = uniqueName(ifaceMemberSym);
                        ifaceMemberSym.flags ^= Modifiers.PRIVATE;
                    } else if (ifaceMemberSym.isProtected())
                        ifaceMemberSym.flags ^= Modifiers.PROTECTED;

                    classMemberSym =
                        deepCloneSymbol(ifaceMemberSym, classSym, paramsSubst);
                    ifaceMemberSym.flags |= Modifiers.DEFERRED;

                    classMemberSym.updateInfo(thisTpMap.apply(classMemberSym.info()));
                    classMembersMap.put(ifaceMemberSym, classMemberSym);
                } else {
                    // Member doesn't go in interface, we just make it
                    // owned by the class.
                    classMemberSym = ifaceMemberSym;
                    classMemberSym.setOwner(classSym);
                }
                Type nextMemberTp = classMemberSym.nextInfo();
                classMemberSym.updateInfo(paramsSubst.apply(nextMemberTp));

                classMembers.enterOrOverload(classMemberSym);
                if (classMemberSym.isClass())
                    classMembers.enterOrOverload(classMemberSym.constructor());
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
                    newClassParents[i] = paramsSubst.apply(newTp);
                    break;
                default:
                    throw Debug.abort("unexpected type for parent", oldClassParents[i]);
                }
            }
            newClassParents[oldParentsCount] = paramsSubst.apply(ifaceSym.type());
            // TODO setInfo cannot be used here because the type then
            // goes through transformInfo. Maybe setInfo should behave
            // like updateInfo.
            classSym.updateInfo(Type.compoundType(newClassParents,
                                                  classMembers,
                                                  classSym));
            classConstrSym.updateInfo(substResType(substParams(classConstrSym.info(),
                                                               paramsSubst),
                                                   ifaceSym,
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

    HashMap/*<Symbol,HashMap>*/ classSubstitutions = new HashMap();
    protected HashMap newClassSubst(Symbol classSym) {
        HashMap subst = new HashMap();
        classSubstitutions.put(classSym, subst);
        return subst;
    }

    /** Return symbol substitution for the class (a mapping from the
     * interface's type and value parameters to the class' equivalent)
     */
    public Map getClassSubst(Symbol classSym) {
        Map classSubst = (Map)classSubstitutions.get(classSym);
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

    /** Substitute type and value arguments in the given type. We
     * don't use Type.Map here, because it doesn't do what we want, in
     * particular it doesn't substitute type arguments of PolyTypes */
    protected Type substParams(Type t, SymbolSubstTypeMap map) {
        switch (t) {
        case MethodType(Symbol[] vparams, Type result): {
            Symbol[] newVParams = new Symbol[vparams.length];
            for (int i = 0; i < vparams.length; ++i) {
                newVParams[i] = map.lookupSymbol(vparams[i]);
                if (newVParams[i] == null) newVParams[i] = vparams[i];
            }
            return new Type.MethodType(newVParams, substParams(result, map));
        }

        case PolyType(Symbol[] tparams, Type result): {
            Symbol[] newTParams = new Symbol[tparams.length];
            for (int i = 0; i < tparams.length; ++i) {
                newTParams[i] = map.lookupSymbol(tparams[i]);
                if (newTParams[i] == null) newTParams[i] = tparams[i];
            }
            return new Type.PolyType(newTParams, substParams(result, map));
        }

        case TypeRef(Type pre, Symbol sym, Type[] args): {
            Symbol newSym = map.lookupSymbol(sym);
            if (newSym == null) newSym = sym;
            Type[] newArgs = new Type[args.length];
            for (int i = 0; i < args.length; ++i)
                newArgs[i] = substParams(args[i], map);
            return new Type.TypeRef(pre, newSym, newArgs);
        }

        case SingleType(_, _):
        case ThisType(_):
            return t;

        case CompoundType(_,_):
            // TODO see what to do here.
            Global.instance.log("WARNING: blindly cloning CompoundType");
            return t;

        default:
            throw Debug.abort("unexpected method type", t);
        }
    }

    protected Type substResType(Type t, Symbol ifaceSym, Symbol classSym) {
        switch (t) {
        case MethodType(Symbol[] vparams, Type result):
            return new Type.MethodType(vparams,
                                       substResType(result, ifaceSym, classSym));
        case PolyType(Symbol[] tparams, Type result):
            return new Type.PolyType(tparams,
                                     substResType(result, ifaceSym, classSym));
        case TypeRef(Type pre, Symbol sym, Type[] args):
            assert sym == ifaceSym;
            return new Type.TypeRef(pre, classSym, args);
        default:
            throw Debug.abort("unexpected constructor type");
        }
    }
}

class ThisTypeMap extends Type.Map {
    private Symbol sym;
    private Type tp;

    public ThisTypeMap(Symbol sym, Type tp) {
        this.sym = sym; this.tp = tp;
    }

    public Type apply(Type t) {
        switch (t) {
        case ThisType(Symbol s):
            return (s == sym) ? tp : t;
        default:
            return map(t);
        }
    }
}
