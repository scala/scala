/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: AddInterfaces.java,v 1.40 2002/11/08 11:56:47 schinz Exp $
// $Id$

package scalac.transformer;

import scalac.*;
import scalac.util.*;
import scalac.ast.*;
import scalac.symtab.*;
import Tree.*;

import java.util.*;

// TODO see why lambda-lifted functions end up in the interface

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

class AddInterfaces extends SubstTransformer {
    /** Mapping from class symbols to their interface symbol. */
    public final Map/*<Symbol,Symbol>*/ classToInterface;

    protected final Map/*<Symbol,Symbol>*/ ifaceToClass;
    protected final SymbolMapApplier ifaceToClassApplier;
    protected final Map/*<Symbol,Symbol>*/ ifaceMemberToClass;

    // Mapping from class symbol to its type parameters mapping.
    protected final Map/*<Symbol,Map<Symbol,Symbol>>*/ typeParamsMaps;

    // Mapping from a class member symbol to its value and type
    // parameters mapping.
    protected final Map/*<Symbol,Map<Symbol,Symbol>>*/ funParamsMaps;

    protected final Set/*<Symbol>*/ createdIFaces = new HashSet();

    public AddInterfaces(Global global, AddInterfacesPhase descr) {
        super(global, global.make);
        classToInterface = descr.classToInterface;
        ifaceToClass = descr.interfaceToClass;
        ifaceMemberToClass = descr.ifaceMemberToClass;

        ifaceToClassApplier = new SymbolMapApplier(ifaceToClass);

        typeParamsMaps = new HashMap();
        funParamsMaps = new HashMap();
    }

    public void apply() {
        // Phase 1: create all new symbols
        ClassSymCreator creator = new ClassSymCreator();

        for (int i = 0; i < global.units.length; ++i)
            creator.traverse(global.units[i].body);

        // Phase 2: transform the tree to add interfaces and use the
        // new symbols where needed.
        super.apply();
    }

    protected final static Tree.ValDef[][] EMPTY_PARAMS =
        new Tree.ValDef[][]{ new Tree.ValDef[0] };

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

    protected final static String CLASS_SUFFIX = "$class";

    protected boolean hasClassSuffix(Name name) {
        return name.toString().endsWith(CLASS_SUFFIX);
    }

    protected Name className(Name interfaceName) {
        assert !hasClassSuffix(interfaceName) : interfaceName;

        String interfaceStr = interfaceName.toString();
        Name className = Name.fromString(interfaceStr + CLASS_SUFFIX);
        if (interfaceName.isTypeName()) return className.toTypeName();
        else if (interfaceName.isConstrName()) return className.toConstrName();
        else return className;
    }

    // Modifiers added to interfaces
    // TODO we should put ABSTRACT_CLASS too but that doesn't work now.
    protected int INTERFACE_MODS = Modifiers.INTERFACE | Modifiers.STATIC;

    // Modifiers for which we do not create interfaces.
    protected int NO_INTERFACE_MODS =
        (Modifiers.MODUL | Modifiers.SYNTHETIC | Modifiers.JAVA);

    protected boolean needInterface(Symbol sym) {
        return (sym != Symbol.NONE)
            && ((sym.primaryConstructorClass().flags & NO_INTERFACE_MODS) == 0);
    }

    protected boolean memberGoesInInterface(Symbol member) {
        switch (member.kind) {
        case Kinds.TYPE: case Kinds.ALIAS:
            return true;
        case Kinds.CLASS:
            return needInterface(member);
        case Kinds.VAL:
            return member.isMethod() && !member.isPrimaryConstructor();
        default:
            throw Debug.abort("unknown kind: " + member.kind);
        }
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

    protected Tree mkAbstract(Tree tree) {
        Symbol symbol = tree.symbol();

        if (symbol.isMethod())
            return gen.DefDef(symbol, Tree.Empty);
        else switch (symbol.kind) {
	case Kinds.TYPE: case Kinds.ALIAS:
            return tree;
        case Kinds.CLASS:
            return classInterface((ClassDef)tree);
	default:
	    throw new ApplicationError("invalid symbol kind for me", symbol);
	}
    }

    protected Symbol getClassSym(Symbol ifaceSym) {
        assert !hasClassSuffix(ifaceSym.name) : ifaceSym.name;

        if (!needInterface(ifaceSym))
            return ifaceSym;
        else {
            assert ifaceToClass.containsKey(ifaceSym);
            return (Symbol)ifaceToClass.get(ifaceSym);
        }
    }

    protected Symbol[] vparams(Type tp) {
        switch (tp) {
        case MethodType(Symbol[] vparams, _):
            return vparams;
        case PolyType(_, Type result):
            return vparams(result);
        case OverloadedType(_, _):
            throw global.fail("can't get vparams of this type", tp);
        default:
            return Symbol.EMPTY_ARRAY;
        }
    }

    protected Type cloneSymbolsInMethodType(Type type,
                                            Symbol newOwner,
                                            SymbolMapApplier smApplier,
                                            Map symbolMap) {
        switch (type) {
        case NoType:
        case ThisType(_):
        case TypeRef(_, _, _):
        case SingleType(_, _):
        case CompoundType(_, _):
            return type;

        case MethodType(Symbol[] vparams, Type result): {
            Symbol[] newVParams = new Symbol[vparams.length];
            for (int i = 0; i < vparams.length; ++i) {
                newVParams[i] = vparams[i].cloneSymbol();
                newVParams[i].setOwner(newOwner);
                newVParams[i].setType(smApplier.apply(newVParams[i].info()));
                symbolMap.put(vparams[i], newVParams[i]);
            }
            return new Type.MethodType(newVParams,
                                       cloneSymbolsInMethodType(result,
                                                                newOwner,
                                                                smApplier,
                                                                symbolMap));
        }

        case PolyType(Symbol[] tparams, Type result): {
            Symbol[] newTParams = new Symbol[tparams.length];
            for (int i = 0; i < tparams.length; ++i) {
                newTParams[i] = tparams[i].cloneSymbol();
                newTParams[i].setOwner(newOwner);
                symbolMap.put(tparams[i], newTParams[i]);
            }
            return new Type.PolyType(newTParams,
                                     cloneSymbolsInMethodType(result,
                                                              newOwner,
                                                              smApplier,
                                                              symbolMap));
        }

        default:
            throw global.fail("unexpected method type: " + Debug.toString(type));
        }
    }

    protected static class ThisTypeSubst {
        Symbol fromSym;
        Type toType;
        ThisTypeSubst outer;
        public ThisTypeSubst(Symbol fromSym, Type toType, ThisTypeSubst outer) {
            this.fromSym = fromSym;
            this.toType = toType;
            this.outer = outer;
        }
        public Type apply(Type tp) {
            switch (tp) {
            case ThisType(Symbol sym):
                if (sym == fromSym)
                    return toType;
                else if (outer != null)
                    return outer.apply(tp);
                else
                    return tp;
            case TypeRef(Type pre, Symbol sym, Type[] args):
                return new Type.TypeRef(apply(pre), sym, apply(args));
            case SingleType(Type pre, Symbol sym):
                return Type.singleType(apply(pre), sym);
            case CompoundType(Type[] parts, Scope members):
                return Type.compoundType(apply(parts), members, tp.symbol());
            case MethodType(Symbol[] vparams, Type result):
                return new Type.MethodType(vparams, apply(result));
            case PolyType(Symbol[] tparams, Type result):
                return new Type.PolyType(tparams, apply(result));
            case OverloadedType(Symbol[] alts, Type[] alttypes):
                return new Type.OverloadedType(alts, apply(alttypes));
            case CovarType(Type t):
                return new Type.CovarType(apply(t));
            case NoType:
                return tp;
            default:
                throw Debug.abort("unknown type",tp);
            }
        }
        public Type[] apply(Type[] tps) {
            Type[] newTps = new Type[tps.length];
            for (int i = 0; i < newTps.length; ++i)
                newTps[i] = apply(tps[i]);
            return newTps;
        }
    }

    // Return the interface corresponding to the given class.
    protected Tree classInterface(ClassDef classDef) {
        Template impl = classDef.impl;
        Symbol ifaceSym = classDef.symbol();

        // Create tree for interface.
        List/*<Tree>*/ ifaceBody = new ArrayList();
        Tree[] body = impl.body;
        for (int i = 0; i < body.length; ++i) {
            Tree elem = body[i];
            if (elem.hasSymbol() && elem.symbol().owner() == ifaceSym) {
                if (memberGoesInInterface(elem.symbol()))
                    ifaceBody.add(mkAbstract(elem));
            }
        }

        Tree[] parentConstr =
            gen.mkParentConstrs(impl.pos, ifaceSym.nextInfo().parents());
        Template ifaceTmpl =
            make.Template(impl.pos,
                          parentConstr,
                          (Tree[])ifaceBody.toArray(new Tree[ifaceBody.size()]));
        ifaceTmpl.setSymbol(impl.symbol().cloneSymbol());
        ifaceTmpl.setType(ifaceSym.nextInfo());

        int ifaceMods = classDef.mods | INTERFACE_MODS;
        ClassDef interfaceDef = (ClassDef)make.ClassDef(classDef.pos,
                                                        ifaceMods,
                                                        classDef.name,
                                                        classDef.tparams,
                                                        EMPTY_PARAMS,
                                                        classDef.tpe,
                                                        ifaceTmpl);
        interfaceDef.setType(Type.NoType);
        interfaceDef.setSymbol(ifaceSym);

        createdIFaces.add(ifaceSym);

        return interfaceDef;
    }

    protected Type fixClassSymbols(Type type) {
        switch (type) {
        case Type.NoType:
        case ThisType(_):
            return type;
        case TypeRef(Type pre, Symbol sym, Type[] args):
            return new Type.TypeRef(fixClassSymbols(pre), getClassSym(sym), args);
        case SingleType(Type pre, Symbol sym):
            return Type.singleType(fixClassSymbols(pre), sym);
        case CompoundType(Type[] parts, Scope members):
            return Type.compoundType(fixClassSymbols(parts),
                                     members,
                                     getClassSym(type.symbol()));
        case MethodType(Symbol[] vparams, Type result):
            return new Type.MethodType(vparams, fixClassSymbols(result));
        case PolyType(Symbol[] tparams, Type result):
            return new Type.PolyType(tparams, fixClassSymbols(result));
        default:
            throw global.fail("unexpected type ",type);
        }
    }

    protected Type[] fixClassSymbols(Type[] types) {
        Type[] newTypes = new Type[types.length];
        for (int i = 0; i < types.length; ++i)
            newTypes[i] = fixClassSymbols(types[i]);
        return newTypes;
    }

    protected Tree fixClassSymbols(Tree tree) {
        switch (tree) {
        case Apply(Tree fun, Tree[] args):
            return copy.Apply(tree, fixClassSymbols(fun), args);
        case TypeApply(Tree fun, Tree[] args):
            return copy.TypeApply(tree, fixClassSymbols(fun), args);
        case Select(Tree qualifier, Name selector): {
            Symbol classSym = getClassSym(tree.symbol());
            return copy.Select(tree, qualifier, classSym.name).setSymbol(classSym);
        }
        case Ident(Name name): {
            Symbol classSym = getClassSym(tree.symbol());
            return copy.Ident(tree, classSym.name).setSymbol(classSym);
        }
        default:
            throw global.fail("unexpected tree",tree);
        }
    }

    protected LinkedList/*<List<Tree>>*/ bodyStack = new LinkedList();
    protected ThisTypeSubst thisTypeSubst = null;

    public Tree[] transform(Tree[] trees) {
        List newTrees = new ArrayList();

        bodyStack.addFirst(newTrees);

        for (int i = 0; i < trees.length; ++i)
            newTrees.add(transform(trees[i]));

        bodyStack.removeFirst();

        return (Tree[]) newTrees.toArray(new Tree[newTrees.size()]);
    }

    public TypeDef[] transform(TypeDef[] ts) {
        return super.transform(ts);
    }

    public Tree transform(Tree tree) {
        if (thisTypeSubst != null) {
            if (tree.definesSymbol()) {
                Symbol sym = tree.symbol();
                sym.updateInfo(thisTypeSubst.apply(sym.nextInfo()));
            }
            tree.setType(thisTypeSubst.apply(tree.type()));
        }

        switch (tree) {
        case ClassDef(int mods,
                      Name name,
                      TypeDef[] tparams,
                      ValDef[][] vparams,
                      Tree tpe,
                      Template impl) : {
            global.log("adding interface for " + tree.symbol()
                       + " (need one? " + needInterface(tree.symbol()) + ")");

            Symbol interfaceSym = tree.symbol();

            if (needInterface(interfaceSym)) {
                ClassDef classDef = (ClassDef) tree;

                // First insert interface for class in enclosing body...
                if (! createdIFaces.contains(interfaceSym)) {
                    Tree interfaceDef = classInterface(classDef);
                    List/*<Tree>*/ enclosingBody = (List)bodyStack.getFirst();
                    enclosingBody.add(interfaceDef);
                }

                // ...then transform the class.
                Symbol classSym = getClassSym(interfaceSym);

                assert typeParamsMaps.containsKey(classSym) : classSym;
                Map/*<Symbol,Symbol>*/ tparamsMap = (Map)typeParamsMaps.get(classSym);
                SymbolMapApplier tparamsSM = new SymbolMapApplier(tparamsMap);

                // Make the class implement the interface, and make sure
                // to use class symbols for base classes.
                Type interfaceBaseType = tparamsSM.apply(interfaceSym.type());
                Type[] newBaseTypes;

                // 1. modify type of class symbol
                Type newClassInfo;
                switch (classSym.nextInfo()) {
                case CompoundType(Type[] baseTypes, Scope members): {
                    newBaseTypes = new Type[baseTypes.length + 1];
                    newBaseTypes[0] = fixClassSymbols(baseTypes[0]);
                    newBaseTypes[1] = interfaceBaseType;
                    for (int i = 2; i < newBaseTypes.length; ++i)
                        newBaseTypes[i] = fixClassSymbols(baseTypes[i-1]);
                    newClassInfo = Type.compoundType(newBaseTypes, members, classSym);
                    classSym.updateInfo(newClassInfo);
                } break;

                default:
                    throw global.fail("invalid info() for class", classSym);
                }

                // 2. modify tree accordingly
                pushSymbolSubst(tparamsMap);
                thisTypeSubst = new ThisTypeSubst(interfaceSym,
                                                  classSym.thisType(),
                                                  thisTypeSubst);
                Tree[] parents = transform(impl.parents);

                Tree[] newParents = new Tree[parents.length + 1];
                newParents[0] = parents[0];
                newParents[1] = gen.mkParentConstr(impl.pos, interfaceBaseType);
                for (int i = 2; i < newParents.length; ++i)
                    newParents[i] = parents[i-1];

                // Use new member symbols for class members.
                Tree[] body = impl.body;
                for (int i = 0; i < body.length; ++i) {
                    Tree member = body[i];
                    if (member.hasSymbol()) {
                        Symbol sym = member.symbol();
                        if (sym.kind != Kinds.CLASS
                            && ifaceMemberToClass.containsKey(sym)) {
                            Symbol cSym = (Symbol)ifaceMemberToClass.get(sym);
                            member.setSymbol(cSym);
                        }
                    }
                }

                // Transform body
                List newBody = new LinkedList();
                for (int i = 0; i < body.length; ++i) {
                    switch (body[i]) {
                    case TypeDef(_, _, _):
                        break;
                    default:
                        newBody.add(transform(body[i]));
                    }
                }
                Template newImpl =
                    copy.Template(impl,
                                  newParents,
                                  (Tree[])newBody.toArray(new Tree[newBody.size()]));
                newImpl.setType(newClassInfo);

                Tree newTree =
                    copy.ClassDef(classDef,
                                  classSym.flags,
                                  classSym.name.toTypeName(),
                                  transform(tparams),
                                  transform(vparams),
                                  transform(tpe),
                                  newImpl)
                    .setSymbol(classSym);

                thisTypeSubst = thisTypeSubst.outer;
                popSymbolSubst();

                return newTree;
            } else {
                // No interface needed, we just adapt the class type
                // to use class symbols.
                Symbol classSym = interfaceSym;
                classSym.updateInfo(fixClassSymbols(classSym.info()));
                return super.transform(tree);
            }
        }

        case Template(Tree[] parents, Tree[] body): {
            return copy.Template(tree, transform(parents), transform(body))
                .setType(fixClassSymbols(tree.type));
        }

        case DefDef(_, _, _, _, _, _): {
            Symbol sym = tree.symbol();
            if (funParamsMaps.containsKey(sym)) {
                Map funParamsMap = (Map)funParamsMaps.get(sym);
                pushSymbolSubst(funParamsMap);
                Tree newTree = super.transform(tree);
                popSymbolSubst();
                return newTree;
            }
            return super.transform(tree);
        }

        case ValDef(_, _, _, _): {
            Symbol sym = tree.symbol();
            Symbol owner = sym.owner();
            if (!sym.isParameter() && ifaceMemberToClass.containsKey(owner)) {
                Symbol newOwner = (Symbol)ifaceMemberToClass.get(owner);
                sym.setOwner(newOwner);
                global.log("new owner for " + Debug.show(sym) + " => " + newOwner);
            }
            return super.transform(tree);
        }

        case This(Ident qualifier): {
            Symbol qualSym = qualifier.symbol();
            if (needInterface(qualSym))
                return gen.This(tree.pos, getClassSym(qualSym));
            else
                return super.transform(tree);
        }

        case Select(Super(_), Name selector): {
            // Use class member symbol for "super" references.
            Symbol sym = tree.symbol();
            if (needInterface(sym.classOwner())) {
                assert ifaceMemberToClass.containsKey(sym);
                Symbol classSym = (Symbol)ifaceMemberToClass.get(sym);
                return super.transform(tree).setSymbol(classSym);
            } else
                return super.transform(tree);
        }

        default: {
            Tree newTree = super.transform(tree);

            // Use class symbols for constructor calls.
            switch (newTree) {
            case New(Template templ):
                return copy.New(newTree, templ)
                    .setType(templ.parents[0].type);

            case Apply(TypeApply(Tree fun, Tree[] targs), Tree[] vargs): {
                Tree tFun = ((Tree.Apply)newTree).fun;
                if (fun.hasSymbol() && fun.symbol().isPrimaryConstructor())
                    return copy.Apply(newTree, tFun, vargs)
                        .setType(fixClassSymbols(newTree.type));
                else
                    return newTree;
            }

            case Apply(Tree fun, Tree[] args): {
                if (fun.hasSymbol() && fun.symbol().isPrimaryConstructor()) {
                    fun.setSymbol(getClassSym(fun.symbol()));
                    fun.setType(fixClassSymbols(fun.type));
                    return (copy.Apply(newTree, super.syncTree(fun), args))
                        .setType(fixClassSymbols(newTree.type));
                } else
                    return newTree;
            }

            case TypeApply(Tree fun, Tree[] args): {
                if (fun.hasSymbol() && fun.symbol().isPrimaryConstructor()) {
                    fun.setSymbol(getClassSym(fun.symbol()));
                    fun.setType(fixClassSymbols(fun.type));
                    return (copy.TypeApply(newTree, super.syncTree(fun), args))
                        .setType(fixClassSymbols(newTree.type));
                } else
                    return newTree;
            }
            default:
                return newTree;
            }
        }
        }
    }

    //////////////////////////////////////////////////////////////////////

    // Class
    protected class ClassSymCreator extends Traverser {
        // Mapping from interface type parameters to class type
        // parameters.
        final HashMap/*<Symbol,Symbol>*/ tparamsMap = new HashMap();

        protected Symbol cloneAndMaybeRenameSymbol(Symbol sym) {
            assert !sym.isPrimaryConstructor() : sym;

            Symbol clone = sym.cloneSymbol();
            if (clone.kind == Kinds.CLASS) {
                clone.name = className(clone.name);
                Symbol constrClone = clone.constructor();
                constrClone.name = className(constrClone.name);
            }
            return clone;
        }

        protected void makeClassSymbol(Symbol ifaceSym) {
            Symbol classSym = cloneAndMaybeRenameSymbol(ifaceSym);
            ifaceToClass.put(ifaceSym, classSym);
            ifaceToClass.put(ifaceSym.constructor(), classSym.constructor());
            ifaceSym.owner().members().enter(classSym);
        }

        public void traverse(Tree tree) {
            switch(tree) {
            case ClassDef(_, _, _, _, _, Template impl): {
                Symbol ifaceSym = tree.symbol();

                if (!needInterface(ifaceSym)) {
                    super.traverse(impl);
                    break;
                }

                // The class needs an interface. Create new symbols
                // for the class itself, its constructor, its type
                // parameters and its members. Then modify what was
                // the class symbol to turn it into an interface
                // symbol.

                // At the end of this part, one inconsistency remains:
                // the base types of the new class symbols still refer
                // to interface symbols. This is fixed later, when
                // symbols exist for *all* classes.

                Symbol ifaceConstrSym = ifaceSym.constructor();
                ifaceConstrSym.updateInfo(removeValueParams(ifaceConstrSym.info()));

                if (! ifaceToClass.containsKey(ifaceSym))
                    makeClassSymbol(ifaceSym);
                Symbol classSym = (Symbol)ifaceToClass.get(ifaceSym);
                Symbol classConstrSym = classSym.constructor();

                if (ifaceToClass.containsKey(classSym.owner())) {
                    Symbol newOwner = (Symbol)ifaceToClass.get(classSym.owner());
                    classSym.setOwner(newOwner);
                    classConstrSym.setOwner(newOwner);
                }

                Symbol[] ifaceTParams = ifaceSym.typeParams();
                if (ifaceTParams.length > 0) {
                    for (int i = 0; i < ifaceTParams.length; ++i) {
                        Symbol classTParam = ifaceTParams[i].cloneSymbol();
                        classTParam.setOwner(classConstrSym);
                        tparamsMap.put(ifaceTParams[i], classTParam);
                    }
                }
                assert !typeParamsMaps.containsKey(classSym);
                Map cloneMap = new HashMap();
                cloneMap.putAll(tparamsMap);
                typeParamsMaps.put(classSym, cloneMap);

                SymbolMapApplier tparamsSM = new SymbolMapApplier(cloneMap);

                classConstrSym.setInfo(tparamsSM.apply(classConstrSym.info()));
                Symbol[] vparams = vparams(classConstrSym.nextInfo());
                for (int i = 0; i < vparams.length; ++i) {
                    vparams[i].setOwner(classConstrSym);
                    vparams[i].updateInfo(tparamsSM.apply(vparams[i].info()));
                }

                Scope newIFaceMembers = new Scope();
                Scope classMembers = new Scope();
                Scope.SymbolIterator symIt =
                    new Scope.UnloadIterator(ifaceSym.members().iterator());
                while (symIt.hasNext()) {
                    Symbol ifaceMemberSym = symIt.next();

                    ifaceMemberSym.updateInfo(tparamsSM.apply(ifaceMemberSym.info()));

                    if (! memberGoesInInterface(ifaceMemberSym)) {
                        ifaceMemberSym.setOwner(classSym);
                        classMembers.enterOrOverload(ifaceMemberSym);
                        continue;
                    }

                    // When encountering a constructor of a nested
                    // class, clone its class to make sure the
                    // constructor is cloned correctly.
                    if (ifaceMemberSym.isPrimaryConstructor()
                        && !ifaceToClass.containsKey(ifaceMemberSym)) {
                        makeClassSymbol(ifaceMemberSym.primaryConstructorClass());
                    }

                    // Make private members public and give them a
                    // unique name.
                    if (Modifiers.Helper.isPrivate(ifaceMemberSym.flags)) {
                        ifaceMemberSym.name = uniqueName(ifaceMemberSym);
                        ifaceMemberSym.flags ^= Modifiers.PRIVATE;
                    }
                    ifaceMemberSym.flags &= ~Modifiers.PROTECTED;

                    // Type members are moved to the interface.
                    // Therefore, no symbol has to be created for
                    // their class equivalent.
                    if (ifaceMemberSym.kind == Kinds.TYPE
                        || ifaceMemberSym.kind == Kinds.ALIAS) {
                        newIFaceMembers.enterOrOverload(ifaceMemberSym);
                        continue;
                    }

                    if (Modifiers.Helper.isPrivate(ifaceMemberSym.flags)) {
                        ifaceMemberSym.name = uniqueName(ifaceMemberSym);
                        ifaceMemberSym.flags ^= Modifiers.PRIVATE;
                    }
                    ifaceMemberSym.flags &= ~Modifiers.PROTECTED;

                    Symbol classMemberSym;
                    if (ifaceToClass.containsKey(ifaceMemberSym))
                        classMemberSym = (Symbol)ifaceToClass.get(ifaceMemberSym);
                    else
                        classMemberSym = cloneAndMaybeRenameSymbol(ifaceMemberSym);

                    ifaceMemberSym.updateInfo(tparamsSM.apply(ifaceMemberSym.info()));
                    classMemberSym.setInfo(tparamsSM.apply(classMemberSym.info()));

                    Symbol[] vpms = vparams(ifaceMemberSym.nextInfo());
                    for (int p = 0; p < vpms.length; ++p)
                        vpms[p].updateInfo(tparamsSM.apply(vpms[p].info()));

                    // Clone parameter symbols for methods.
                    if (classMemberSym.isMethod()) {
                        Map funSymMap = new HashMap();
                        Type newInfo = cloneSymbolsInMethodType(classMemberSym.info(),
                                                                classMemberSym,
                                                                tparamsSM,
                                                                funSymMap);
                        if (! funSymMap.isEmpty())
                            funParamsMaps.put(classMemberSym, funSymMap);
                    }

                    classMemberSym.setOwner(classSym);

                    if (ifaceMemberSym.kind == Kinds.CLASS) {
                        ifaceToClass.put(ifaceMemberSym, classMemberSym);
                        ifaceToClass.put(ifaceMemberSym.constructor(),
                                         classMemberSym.constructor());
                    } else {
                        ifaceMemberSym.flags |= Modifiers.DEFERRED;
                        ifaceMemberToClass.put(ifaceMemberSym, classMemberSym);
                    }
                    newIFaceMembers.enterOrOverload(ifaceMemberSym);
                    if (!ifaceMemberToClass.containsKey(ifaceMemberSym)
                        && ifaceMemberSym.kind != Kinds.CLASS)
                        ifaceMemberToClass.put(ifaceMemberSym, classMemberSym);
                    classMembers.enterOrOverload(classMemberSym);
                }

                switch (classSym.info()) {
                case CompoundType(Type[] parts, Scope members):
                    classSym.setInfo(Type.compoundType(tparamsSM.apply(parts),
                                                       classMembers,
                                                       classSym));
                    break;

                default:
                    global.fail("unexpected type for class", ifaceSym.info());
                }

                Type cConstrType = classConstrSym.info();
                classConstrSym.updateInfo(cConstrType.subst(new Symbol[]{ifaceSym},
                                                            new Symbol[]{classSym}));

                ifaceSym.flags |= INTERFACE_MODS;

                classToInterface.put(classSym, ifaceSym);
                super.traverse(impl);

                if (ifaceTParams.length > 0)
                    for (int i = 0; i < ifaceTParams.length; ++i)
                        tparamsMap.remove(ifaceTParams[i]);

                // Remove Java classes from interface base classes.
                switch (ifaceSym.info()) {
                case CompoundType(Type[] basetypes, Scope members):
                    ArrayList newBT_L = new ArrayList(basetypes.length);
                    for (int i = 0; i < basetypes.length; ++i)
                        if (! basetypes[i].symbol().isJava())
                            newBT_L.add(basetypes[i]);
                    Type[] newBT;
                    if (newBT_L.size() != basetypes.length)
                        newBT = (Type[]) newBT_L.toArray(new Type[newBT_L.size()]);
                    else
                        newBT = basetypes;
                    ifaceSym.updateInfo(Type.compoundType(newBT,
                                                          newIFaceMembers,
                                                          ifaceSym));
                    break;

                default:
                    Debug.abort("unexpected type for class", ifaceSym.info());
                }
            } break;

            default:
                super.traverse(tree);
            }
        }
    }
}
