/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.transformer;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.CompilationUnit;
import scalac.symtab.Definitions;
import scalac.symtab.Scope;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolNameWriter;
import scalac.symtab.Type;
import scalac.symtab.Modifiers;
import scalac.atree.AConstant;
import scalac.ast.Transformer;
import scalac.ast.GenTransformer;
import scalac.ast.Tree;
import scalac.ast.TreeList;
import scalac.backend.Primitives;

import scalac.util.Name;
import scalac.util.Names;
import scalac.util.Debug;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Arrays;
import java.util.Collections;

/**
 * Turn types into values by applying the following transformations:
 *
 * - For all type member T of all classes, add an accessor method
 *   T$type returnining the type as a value (the accessor is abstract
 *   if the type member is abstract).
 *
 * - For all polymorphic methods/constructors, add a value parameter
 *   for each type parameter.
 *
 * - Add a method getType to every class, to obtain its type as a
 *   value.
 *
 * - Transform all type expressions into value expressions: type
 *   application is turned into value application, type selection into
 *   value selection, and so on.
 *
 * @author Michel Schinz
 * @version 1.0
 */

// TODO use a constant instead of generating empty arrays all the
// time.

public class TypesAsValuesPhase extends Phase {
    private final TV_Transformer transformer;

    /**
     * The list of members to add to a given class (either type
     * accessors or instantiation methods).
     */
    private final HashMap/*<Symbol,NewMember[]>*/ membersToAdd =
        new HashMap();

    /** The list of parameters to add to a given method. */
    private final HashMap/*<Symbol,List<Symbol>>*/ paramsToAdd =
        new HashMap();

    /** The accessor method corresponding to a given type (or class) member. */
    private final HashMap/*<Symbol, Symbol>*/ typeAccessor =
        new HashMap();

    /** The instanciation method corresponding to a given class. */
    private final HashMap/*<Symbol,Symbol>*/ instantiator =
        new HashMap();

    /** The class constructor corresponding to a given class. */
    private final HashMap/*<Symbol,Symbol>*/ classInitialiser =
        new HashMap();

    private final HashMap/*<Symbol,Symbol>*/ tConstructor =
        new HashMap();

    private final Definitions defs = global.definitions;
    private final Primitives prims = global.primitives;

    private final Type.MethodType typeAccessorType =
        new Type.MethodType(new Symbol[]{}, defs.TYPE_TYPE());

    private final Symbol ARRAY_CONSTRUCTOR =
        defs.ARRAY_CLASS.primaryConstructor();

    private final TEnv EENV = new TEnv();

    private final Map/*<Symbol, Symbol>*/ basicTypes;

    private HashMap/*<Symbol, Ancestor[][]>*/ displayCache = new HashMap();

    public TypesAsValuesPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
        transformer = new TV_Transformer(global);

        basicTypes = new HashMap();
        basicTypes.put(defs.DOUBLE_CLASS,  defs.RTT_DOUBLE());
        basicTypes.put(defs.FLOAT_CLASS,   defs.RTT_FLOAT());
        basicTypes.put(defs.LONG_CLASS,    defs.RTT_LONG());
        basicTypes.put(defs.INT_CLASS,     defs.RTT_INT());
        basicTypes.put(defs.SHORT_CLASS,   defs.RTT_SHORT());
        basicTypes.put(defs.CHAR_CLASS,    defs.RTT_CHAR());
        basicTypes.put(defs.BYTE_CLASS,    defs.RTT_BYTE());
        basicTypes.put(defs.BOOLEAN_CLASS, defs.RTT_BOOLEAN());

        membersToAdd.put(defs.ARRAY_CLASS, new NewMember[0]);
        paramsToAdd.put(ARRAY_CONSTRUCTOR, new Symbol[0]);

        displayCache.put(defs.OBJECT_CLASS,
                         new Ancestor[][] {
                             new Ancestor[] {
                                 new Ancestor(defs.OBJECT_CLASS, -1, -1)
                             }
                         });
    }

    /**
     * Return the symbol of the accessor for the given type symbol.
     */
    private Symbol getAccessorSym(Symbol typeSym) {
        assert typeSym.isType();
        Symbol accessorSym = (Symbol)typeAccessor.get(typeSym);
        if (accessorSym == null) {
            accessorSym = typeSym.owner().newVariable(typeSym.pos,
                                                      typeSym.flags,
                                                      Names.TYPE(typeSym));
            accessorSym.setInfo(defs.TYPE_TYPE());
            typeAccessor.put(typeSym, accessorSym);
        }
        return accessorSym;
    }

    private Symbol getInstMethSym(Symbol classSym) {
        Symbol imSym = (Symbol)instantiator.get(classSym);
        if (imSym == null) {
            int pos = classSym.pos;
            Name imName = Names.INSTANTIATE(classSym);

            imSym = isNestedClass(classSym)
                ? classSym.owner().newMethod(pos, 0, imName)
                : classSym.newStaticMethod(pos, 0, imName);

            // TODO special case for monomorphic instantiations
            Symbol[] argTypes;
            if (true || classSym.typeParams().length > 0) {
                Symbol typesP =
                    imSym.newVParam(pos, 0, Name.fromString("types"));
                typesP.setInfo(defs.ARRAY_TYPE(defs.TYPE_TYPE()));
                argTypes = new Symbol[]{ typesP };
            } else
                argTypes = Symbol.EMPTY_ARRAY;

            imSym.setInfo(new Type.MethodType(argTypes,
                                              defs.SCALACLASSTYPE_TYPE()));

            instantiator.put(classSym, imSym);
        }
        return imSym;
    }

    private Symbol getTConstructorSym(Symbol classSym) {
        Symbol tcSym = (Symbol)tConstructor.get(classSym);
        if (tcSym == null) {
            int pos = classSym.pos;
            Name tcName = Names.TYPECONSTRUCTOR(classSym);

            tcSym = isNestedClass(classSym)
                ? classSym.owner().newField(pos, 0, tcName)
                : classSym.newStaticField(pos, 0, tcName);
            tcSym.setInfo(defs.TYPECONSTRUCTOR_TYPE());

            tConstructor.put(classSym, tcSym);
        }
        return tcSym;
    }

    private Symbol getClassInitSym(Symbol classSym) {
        Symbol ciSym = (Symbol)classInitialiser.get(classSym);
        if (ciSym == null) {
            int pos = classSym.pos;

            ciSym = classSym.newStaticMethod(pos, 0, Names.CLASS_CONSTRUCTOR);
            ciSym.setInfo(new Type.MethodType(Symbol.EMPTY_ARRAY,
                                              defs.UNIT_TYPE()));

            classInitialiser.put(classSym, ciSym);
        }
        return ciSym;
    }

    private NewMember[] membersToAdd(Symbol classSym) {
        NewMember[] toAdd = (NewMember[])membersToAdd.get(classSym);
        if (toAdd == null) {
            ArrayList toAddL = new ArrayList();
            Scope.SymbolIterator membersIt = classSym.members().iterator();
            while (membersIt.hasNext()) {
                Symbol member = membersIt.next();
                if (member.isClass()) {
                    Symbol tcSym = getTConstructorSym(member);
                    toAddL.add(NewMember.TypeConstructor(member, tcSym));
                    Symbol imSym = getInstMethSym(member);
                    toAddL.add(NewMember.Instantiator(member, imSym));
                } else if (member.isType()) {
                    Symbol accSym = getInstMethSym(member);
                    toAddL.add(NewMember.TypeAccessor(member, accSym));
                }
            }

            if (!isNestedClass(classSym)) {
                Symbol tcSym = getTConstructorSym(classSym);
                toAddL.add(NewMember.TypeConstructor(classSym, tcSym));
                Symbol ciSym = getClassInitSym(classSym);
                toAddL.add(NewMember.ClassInitialiser(classSym, ciSym, tcSym));
                Symbol imSym = getInstMethSym(classSym);
                toAddL.add(NewMember.Instantiator(classSym, imSym));
            }

            toAdd = (NewMember[])toAddL.toArray(new NewMember[toAddL.size()]);
            membersToAdd.put(classSym, toAdd);
        }
        return toAdd;
    }

    private Symbol[] paramsToAdd(Symbol methSym) {
        Symbol[] toAdd = (Symbol[])paramsToAdd.get(methSym);
        if (toAdd == null) {
            Symbol[] tparams = methSym.typeParams();

            ArrayList toAddL = new ArrayList();
            for (int i = 0; i < tparams.length; ++i)
                toAddL.add(getAccessorSym(tparams[i]));

            toAdd = (Symbol[])toAddL.toArray(new Symbol[toAddL.size()]);
            paramsToAdd.put(methSym, toAdd);
        }

        return toAdd;
    }

    public Type transformInfo(Symbol symbol, Type type) {
        if (symbol.isClass()) {
            NewMember[] toAdd = membersToAdd(symbol);

            if (toAdd.length == 0)
                return type;
            else {
                Scope newMembers = new Scope(symbol.members());

                for (int i = 0; i < toAdd.length; ++i)
                    newMembers.enterOrOverload(toAdd[i].symbolToAdd());

                return Type.compoundType(type.parents(), newMembers, symbol);
            }
        } else if (type.typeParams().length > 0 && !isPrimitive(symbol)) {
            // Polymorphic method/constructor:
            // - add a value parameter for every type parameter.
            switch (type) {
            case PolyType(Symbol[] tparams, // :
                          Type.MethodType(Symbol[] vparams, Type result)):
                List newVParams =
                    new LinkedList(Arrays.asList(paramsToAdd(symbol)));
                newVParams.addAll(Arrays.asList(vparams));
                Symbol[] newVParamsA = (Symbol[])
                    newVParams.toArray(new Symbol[newVParams.size()]);

                return new Type.PolyType(tparams,
                                         new Type.MethodType(newVParamsA,
                                                             result));

            default:
                throw Debug.abort("unexpected type: ", type);
            }
        } else
            return type;
    }

    private boolean isPrimitive(Symbol sym) {
        return sym == defs.ANY_IS
            || sym == defs.ANY_AS;
    }

    private boolean isNestedClass(Symbol classSym) {
        return !classSym.owner().isPackageClass();
    }

    public void apply(CompilationUnit[] units) {
        transformer.apply(units);
    }

    private class TV_Transformer extends GenTransformer {
        private Symbol currentOwner;

        public TV_Transformer(Global global) {
            super(global);
        }

        public Tree transform(Tree tree) {
            try {
                return transform0(tree);
            } catch (Error e) {
                System.out.println("tree: " + tree);
                throw e;
            }
        }

        public Tree transform0(Tree tree) {
            switch (tree) {
            case ClassDef(int mods, //:
                          Name name,
                          Tree.AbsTypeDef[] tparams,
                          Tree.ValDef[][] vparams,
                          Tree tpe,
                          Tree.Template impl):
                Symbol sym = tree.symbol();

//                 if (impl.symbol().isNone())
//                     throw new Error("no symbol for " + tree);

                TreeList newBody =
                    new TreeList(transform(impl.body, impl.symbol()));
                // Add members (accessors and instantiators)
                NewMember[] toAdd = membersToAdd(sym);
                for (int i = 0; i < toAdd.length; ++i) {
                    switch (toAdd[i]) {
                    case TypeAccessor(Symbol memSym, Symbol accSym):
                        newBody.append(typeAccessorBody(memSym, accSym));
                        break;
                    case TypeConstructor(Symbol memSym, Symbol tcSym):
                        newBody.append(tConstructorVal(memSym, tcSym));
                        break;
                    case Instantiator(Symbol memSym, Symbol insSym):
                        newBody.append(instantiatorBody(memSym, insSym));
                        break;
                    case ClassInitialiser(Symbol memSym,
                                          Symbol ciSym,
                                          Symbol tcSym):
                        newBody.append(classInitialiser(memSym, ciSym, tcSym));
                        break;
                    }
                }

                Symbol pConst = sym.primaryConstructor();

                return gen.ClassDef(sym,
                                    transform(impl.parents, pConst),
                                    impl.symbol(),
                                    newBody.toArray());

            case DefDef(_, _, _, _, _, Tree rhs):
                Symbol symbol = getSymbolFor(tree);

                // TODO maybe use "overrides" method instead of name
                // to identify the "getType" method.
                if (symbol.name == Names.getType) {
                    // Correct the body of the getType method which,
                    // until now, was a placeholder (introduced by
                    // RefCheck).
                    rhs = scalaClassType(symbol.pos,
                                         symbol.owner().type(),
                                         symbol,
                                         EENV);
                }

                return gen.DefDef(symbol, transform(rhs, symbol));

            case ValDef(_, _, Tree tpe, Literal(AConstant.ZERO)):
                // transform default values:
                //   val x: T = _
                // becomes
                //   val x: T = asValue(T).defaultValue()
                Symbol symbol = getSymbolFor(tree);
                Tree defaultValue =
                    gen.mkRef(tree.pos,
                              typeAsValue(tree.pos,
                                          tpe.type,
                                          currentOwner,
                                          EENV),
                              defs.TYPE_DEFAULTVALUE());
                Tree rhs = gen.mkApply__(tree.pos, defaultValue);
                return gen.ValDef(symbol, rhs);

            case ValDef(_, _, _, Tree rhs):
                Symbol symbol = getSymbolFor(tree);
                return gen.ValDef(symbol, transform(rhs, symbol));

            case New(Apply(TypeApply(Tree fun, Tree[] targs), Tree[] vargs)):
                if (fun.symbol() == ARRAY_CONSTRUCTOR && false) {
                    // Transform array creations:
                    //   new Array[T](size)
                    // becomes
                    //   asValue(T).newArray[T](size)
                    assert targs.length == 1;
                    assert vargs.length == 1;
                    Tree newArrayfun = gen.mkRef(tree.pos,
                                                 typeAsValue(targs[0].pos,
                                                             targs[0].type,
                                                             currentOwner,
                                                             EENV),
                                                 defs.TYPE_NEWARRAY());
                    return gen.mkApplyTV(newArrayfun, targs, vargs);
                } else
                    return super.transform(tree);

            case Apply(TypeApply(Tree fun, Tree[] targs), Tree[] vargs):
                Symbol funSym = fun.symbol();

                if (funSym == defs.ANY_IS) {
                    assert targs.length == 1 && vargs.length == 0;
                    Type type = targs[0].type;
                    Tree expr = transform(qualifierOf(fun));
                    return isTrivialType(type)
                        ? super.transform(tree)
                        : genInstanceTest(tree.pos, expr, type);
                } else if (funSym == defs.ANY_AS) {
                    // Transform instance tests:
                    //   e.asInstanceOf[T]
                    // becomes:
                    //   asValue(T).checkCastability(e).asInstanceOf[T]
                    // unless T is a "simple" type for which a Java
                    // instance test is sufficient, in which case the
                    // expression is left as is.
                    assert targs.length == 1 && vargs.length == 0;
                    Type type = targs[0].type;
                    Tree expr = transform(qualifierOf(fun));
                    return isTrivialType(type)
                        ? super.transform(tree)
                        : genTypeCast(tree.pos, expr, type);
                } else {
                    // Transform applications to pass types as values:
                    //   f[T1, ...](v1, ...)
                    // becomes
                    //   f[T1, ...](asValue(T1), ..., v1, ...)
                    if (funSym == ARRAY_CONSTRUCTOR)
                        return super.transform(tree);
                    else {
                        Tree[] newVArgs = transform(vargs);
                        Tree[] finalVArgs =
                            new Tree[newVArgs.length + targs.length];
                        for (int i = 0; i < targs.length; ++i)
                            finalVArgs[i] = typeAsValue(targs[i].pos,
                                                        targs[i].type,
                                                        currentOwner,
                                                        EENV);
                        System.arraycopy(newVArgs, 0,
                                         finalVArgs, targs.length,
                                         newVArgs.length);
                        return gen.mkApplyTV(tree.pos,
                                             transform(fun),
                                             targs,
                                             finalVArgs);
                    }
                }

            default:
                return super.transform(tree);
            }
        }

        private Tree transform(Tree tree, Symbol currentOwner) {
            Symbol bkpOwner = this.currentOwner;
            this.currentOwner = currentOwner;
            Tree newTree = transform(tree);
            this.currentOwner = bkpOwner;
            return newTree;
        }

        private Tree[] transform(Tree[] trees, Symbol currentOwner) {
            Symbol bkpOwner = this.currentOwner;
            this.currentOwner = currentOwner;
            Tree[] newTrees = transform(trees);
            this.currentOwner = bkpOwner;
            return newTrees;
        }

        private int level(Symbol sym) {
            Symbol superClass = sym.parents()[0].symbol();
            assert superClass != Symbol.NONE : sym;
            if (superClass == defs.ANY_CLASS)
                return 0;
            else
                return 1 + level(superClass);
        }

        /**
         * Return a method giving access to the given type, as a
         * value.
         */
        private Tree.DefDef typeAccessorBody(Symbol typSym, Symbol accSym) {
            Tree rhs;
            if (typSym.isAbstractType())
                rhs = Tree.Empty;
            else if (typSym.isClass())
                rhs = scalaClassType(typSym.pos, typSym.type(), accSym, EENV);
            else
                rhs = typeAsValue(typSym.pos, typSym.type(), accSym, EENV);
            return gen.DefDef(accSym, rhs);
        }

        private Tree tConstructorVal(Symbol clsSym, Symbol tcSym) {
            return gen.ValDef(tcSym,
                              tcSym.isStatic()
                              ? Tree.Empty
                              : tConstructorRHS(tcSym.pos, clsSym, tcSym));
        }

        private Tree classInitialiser(Symbol clsSym,
                                      Symbol ciSym,
                                      Symbol tcSym) {
            if (tcSym.isStatic()) {
                int pos = tcSym.pos;
                Tree rhs = tConstructorRHS(pos, clsSym, ciSym);
                Tree assign = gen.Assign(pos, gen.Ident(pos, tcSym), rhs);

                return gen.DefDef(ciSym, assign);
            } else
                return Tree.Empty;
        }

        private Tree tConstructorRHS(int pos, Symbol clsSym, Symbol owner) {
            int zCount = 0, mCount = 0, pCount = 0;
            Symbol[] tparams = clsSym.typeParams();

            for (int i = 0; i < tparams.length; ++i) {
                if ((tparams[i].flags & Modifiers.COVARIANT) != 0)
                    ++pCount;
                else if ((tparams[i].flags & Modifiers.CONTRAVARIANT) != 0)
                    ++mCount;
                else
                    ++zCount;
            }

            int[] displayCode = getDisplayCode(computeDisplay(clsSym));

            Pair attrRef = refinements(clsSym.info());
            int[] refinementCode =
                refinementCode((ArrayList)attrRef.fst, (HashMap)attrRef.snd);

            Tree[] tcArgs = new Tree[] {
                gen.mkIntLit(pos, level(clsSym)),
                gen.mkStringLit(pos, prims.getJREClassName(clsSym)),
                isNestedClass(clsSym)
                ? gen.This(pos, clsSym.owner())
                : gen.mkNullLit(pos),
                gen.mkIntLit(pos, zCount),
                gen.mkIntLit(pos, mCount),
                gen.mkIntLit(pos, pCount),
                mkNewIntLitArray(pos, displayCode, owner),
                mkNewIntLitArray(pos, refinementCode, owner)
            };

            Symbol tcConst = defs.TYPECONSTRUCTOR_CLASS.primaryConstructor();
            Tree tcCall =
                gen.mkApply_V(pos, gen.mkGlobalRef(pos, tcConst), tcArgs);
            return gen.New(pos, tcCall);
        }

        private Tree mkNewIntLitArray(int pos, int[] values, Symbol owner) {
            Tree[] intLits = new Tree[values.length];
            for (int i = 0; i < values.length; ++i)
                intLits[i] = gen.mkIntLit(pos, values[i]);
            return gen.mkNewArray(pos, defs.INT_TYPE(), intLits, owner);
        }

        /**
         * Return a method to instantiate the given type.
         */
        private Tree.DefDef instantiatorBody(Symbol clsSym, Symbol insSym) {
            // TODO fix flags for all symbols below
            final int pos = clsSym.pos;
            final Symbol[] vparams = insSym.valueParams();

            Tree[] body = new Tree[2];

            // Generate call to "getInstantiation" method of
            // constructor.
            Tree getInstFun =
                gen.Select(pos,
                           gen.mkLocalRef(pos, getTConstructorSym(clsSym)),
                           defs.TYPECONSTRUCTOR_GETINSTANTIATION());

            Tree[] getInstArgs = new Tree[]{ gen.mkLocalRef(pos, vparams[0]) };

            Symbol instVal =
                insSym.newVariable(pos, 0, Name.fromString("inst"));
            instVal.setInfo(defs.SCALACLASSTYPE_TYPE());

            Tree instValDef =
                gen.ValDef(instVal,
                           gen.mkApply_V(pos, getInstFun, getInstArgs));

            // Generate test to see if a call to "instantiate" is
            // necessary.
            Tree cond =
                gen.mkApply_V(pos,
                              gen.Select(pos,
                                         gen.mkLocalRef(pos, instVal),
                                         defs.ANY_BANGEQ),
                              new Tree[] { gen.mkNullLit(pos) });
            Tree thenP = gen.mkLocalRef(pos, instVal);

            final HashMap varMap = new HashMap();
            Symbol[] tparams = clsSym.typeParams();
            for (int i = 0; i < tparams.length; ++i)
                varMap.put(tparams[i], new Integer(i));

            // Type environment mapping the type parameters of the
            // class to their corresponding element in the "types"
            // array passed to this instantiator.
            TEnv tEnv = new TEnv() {
                    public boolean definesVar(Symbol sym) {
                        return varMap.containsKey(sym);
                    }

                    public Tree treeForVar(Symbol sym) {
                        int idx = ((Integer)varMap.get(sym)).intValue();
                        Tree array = gen.mkLocalRef(pos, vparams[0]);
                        return gen.mkArrayGet(pos, array, idx);
                    }
                };

            Type[] parents = clsSym.parents();
            TreeList parentTypes = new TreeList();
            for (int i = 0; i < parents.length; ++i) {
                Type parent = parents[i];
                if (!parent.symbol().isJava()) {
                    Tree parentType =
                        typeAsValue(pos, parent, insSym, tEnv);
                    parentTypes.append(parentType);
                }
            }
            Tree parentsArray = gen.mkNewArray(pos,
                                               defs.SCALACLASSTYPE_TYPE(),
                                               parentTypes.toArray(),
                                               insSym);
            Tree refArray = mkRefinementArray(pos,
                                              refinements(clsSym.members()),
                                              insSym,
                                              tEnv);

            Tree instFun =
                gen.Select(pos,
                           gen.mkLocalRef(pos, getTConstructorSym(clsSym)),
                           defs.TYPECONSTRUCTOR_INSTANTIATE());
            Tree[] instArgs = new Tree[] {
                gen.mkLocalRef(pos, vparams[0]),
                parentsArray,
                refArray
            };
            Tree elseP = gen.mkApply_V(pos, instFun, instArgs);

            Tree ifExpr =
                gen.If(pos, cond, thenP, elseP, defs.SCALACLASSTYPE_TYPE());

            return gen.DefDef(insSym, gen.mkBlock(pos, instValDef, ifExpr));
        }

        private Tree mkRefinementArray(int pos,
                                       ArrayList refinements,
                                       Symbol owner,
                                       TEnv env) {
            TreeList refs = new TreeList();
            Symbol refClass = defs.REFINEMENT_CLASS;

            Iterator refIt = refinements.iterator();
            while (refIt.hasNext()) {
                Refinement ref = (Refinement)refIt.next();
                Tree[] args = {
                    gen.mkIntLit(pos, ref.hash),
                    typeAsValue(pos, ref.type, owner, env)
                };
                Tree constr = gen.mkPrimaryConstructorGlobalRef(pos, refClass);
                refs.append(gen.New(pos, gen.mkApply_V(constr, args)));
            }

            Type refType = defs.REFINEMENT_TYPE();
            return gen.mkNewArray(pos, refType, refs.toArray(), owner);
        }

        /**
         * Generate code to test if the given expression is an
         * instance of the given type.
         */
        private Tree genInstanceTest(int pos, Tree expr, Type tp) {
            Tree tpVal = typeAsValue(pos, tp, currentOwner, EENV);
            Tree fun = gen.Select(pos, tpVal, defs.TYPE_ISINSTANCE());
            return gen.mkApply_V(pos, fun, new Tree[] { expr });
        }

        /**
         * Generate code to cast the given value to the given type.
         */
        private Tree genTypeCast(int pos, Tree expr, Type tp) {
            Tree tpVal = typeAsValue(pos, tp, currentOwner, EENV);
            Tree fun = gen.Select(pos, tpVal, defs.TYPE_CHECKCASTABILITY());
            Tree checkCastCall = gen.mkApply_V(pos, fun, new Tree[] { expr });
            return gen.mkAsInstanceOf(pos, checkCastCall, tp);
        }

        /**
         * Return true iff the given type is "trivial", that is if it
         * is representable as a Java type without loss of
         * information.
         */
        private boolean isTrivialType(Type tp) {
            switch (tp) {
            case ConstantType(Type base, _):
                return isTrivialType(base);
            case TypeRef(_, Symbol sym, Type[] args):
                return sym.isStatic() && args.length == 0;
            case SingleType(_, _):
            case ThisType(_):   // TODO check
            case CompoundType(_, _): // TODO check
                return false;
            default:
                throw Debug.abort("unexpected type", tp);
            }
        }

        /**
         * Transform a type into a tree representing it.
         */
        private Tree typeAsValue(int pos, Type tp, Symbol owner, TEnv env) {
            switch (tp) {
            case ConstantType(Type base, _):
                return typeAsValue(pos, base, owner, env);

            case TypeRef(Type pre, Symbol sym, Type[] args):
                if (env.definesVar(sym)) {
                    assert args.length == 0;
                    return env.treeForVar(sym);
                } else if (basicTypes.containsKey(sym)) {
                    return gen.mkGlobalRef(pos, (Symbol)basicTypes.get(sym));
                } else if (sym.isJava()) {
                    assert args.length == 0
                        : Debug.show(sym) + " " + args.length;
                    return javaType(pos, sym);
                } else if (!sym.owner().isMethod()) {
                    // Reference to a "global" type.
                    if (owner == null)
                        throw new Error("null owner for " + Debug.show(tp));
                    return scalaClassType(pos, tp, owner, env);
                } else {
                    assert !isValuePrefix(pre) : tp;
                    return gen.mkLocalRef(pos, getAccessorSym(sym));
                }

            case SingleType(Type pre, Symbol sym):
                return singleType(pos, pre, sym);

            case CompoundType(Type[] parts, Scope members):
                return compoundType(pos, parts, members, owner, env);

            case MethodType(Symbol[] vparams, Type result):
                return methodType(pos, vparams, result, owner, env);

            default:
                throw global.fail("unexpected type: ", tp);
            }
        }

        private Tree javaType(int pos, Symbol sym) {
            Tree constr =
                gen.mkPrimaryConstructorGlobalRef(pos,
                                                  defs.JAVACLASSTYPE_CLASS);
            Tree nameLit = gen.mkStringLit(pos, prims.getJREClassName(sym));
            Tree[] args = new Tree[] { nameLit };
            return gen.New(pos, gen.mkApply_V(constr, args));
        }

        private Tree singleType(int pos, Type pre, Symbol sym) {
            Tree constr =
                gen.mkPrimaryConstructorGlobalRef(pos, defs.SINGLETYPE_CLASS);
            Tree[] args = new Tree[] { gen.mkRef(pos, pre, sym) };
            return gen.New(pos, gen.mkApply_V(constr, args));
        }

        private Tree compoundType(int pos,
                                  Type[] parts,
                                  Scope members,
                                  Symbol owner,
                                  TEnv env) {
            Tree[] partsT = new Tree[parts.length];
            for (int i = 0; i < parts.length; ++i)
                partsT[i] = typeAsValue(pos, parts[i], owner, env);

            Tree refinementArray =
                mkRefinementArray(pos,
                                  (ArrayList)refinements(parts, members).fst,
                                  owner,
                                  env);

            Tree[] constrArgs = new Tree[] {
                gen.mkNewArray(pos, defs.CLASSTYPE_TYPE(), partsT, owner),
                refinementArray
            };
            Tree constr =
                gen.mkPrimaryConstructorGlobalRef(pos,
                                                  defs.COMPOUNDTYPE_CLASS);
            return gen.New(pos, gen.mkApply_V(constr, constrArgs));
        }

        private Tree scalaClassType(int pos, Type tp, Symbol owner, TEnv env) {
            switch (tp) {
            case TypeRef(Type pre, Symbol sym, Type[] args):
                Symbol insSym = getInstMethSym(sym);
                Tree preFun = isNestedClass(sym)
                    ? gen.Select(pos, prefixAsValue(pos, pre), insSym)
                    : gen.Ident(pos, insSym);

                // TODO special case for monomorphic cases
                Tree[] insArgs;
                if (true || args.length > 0) {
                    Tree[] elems = new Tree[args.length];
                    int[] perm = typeParamsPermutation(sym.typeParams());
                    for (int i = 0; i < args.length; ++i)
                        elems[i] = typeAsValue(pos, args[perm[i]], owner, env);
                    insArgs = new Tree[] {
                        gen.mkNewArray(pos, defs.TYPE_TYPE(), elems, owner)
                    };
                } else
                    insArgs = Tree.EMPTY_ARRAY;

                return gen.mkApply_V(pos, preFun, insArgs);

            default:
                throw Debug.abort("unexpected type: ", tp);
            }
        }

        private Tree methodType(int pos,
                                Symbol[] args,
                                Type result,
                                Symbol owner,
                                TEnv env) {
            Tree[] argTypes = new Tree[args.length];
            for (int i = 0; i < argTypes.length; ++i)
                argTypes[i] = typeAsValue(pos, args[i].info(), owner, env);
            Tree[] constrArgs = new Tree[] {
                gen.mkNewArray(pos, defs.TYPE_TYPE(), argTypes, owner),
                typeAsValue(pos, result, owner, env)
            };
            Tree constr =
                gen.mkPrimaryConstructorGlobalRef(pos, defs.METHODTYPE_CLASS);

            return gen.New(pos, gen.mkApply_V(constr, constrArgs));
        }

        private final int VARIANT =
            Modifiers.COVARIANT | Modifiers.CONTRAVARIANT;

        /**
         * Compute the (unique) permutation which puts all invariant
         * type parameters first, followed by the contravariant ones,
         * then the covariants, preserving the relative ordering of
         * arguments with same variance.
         */
        private int[] typeParamsPermutation(Symbol[] params) {
            int[] tparamsPerm = new int[params.length];
            int permIdx = 0;

            for (int i = 0; i < params.length; ++i)
                if ((params[i].flags & VARIANT) == 0)
                    tparamsPerm[permIdx++] = i;
            for (int i = 0; i < params.length; ++i)
                if ((params[i].flags & Modifiers.CONTRAVARIANT) != 0)
                    tparamsPerm[permIdx++] = i;
            for (int i = 0; i < params.length; ++i)
                if ((params[i].flags & Modifiers.COVARIANT) != 0)
                    tparamsPerm[permIdx++] = i;
            assert permIdx == tparamsPerm.length;

            return tparamsPerm;
        }

        /**
         * Extract qualifier from a tree, which must be a Select node.
         */
        private Tree qualifierOf(Tree tree) {
            switch (tree) {
            case Select(Tree qualifier, _): return qualifier;
            default: throw Debug.abort("cannot extract qualifier from ", tree);
            }
        }

        private boolean isValuePrefix(Type pre) {
            switch (pre) {
            case ThisType(Symbol clazz):
                return !(clazz.isPackage() || clazz.isNone());
            case NoPrefix:
                return false;
            default:
                return true;
            }
        }

        /**
         * Transform a prefix into a tree representing it.
         */
        private Tree prefixAsValue(int pos, Type pre) {
            assert isValuePrefix(pre);
            switch (pre) {
            case ThisType(Symbol clazz):
                return gen.This(pos, clazz);
            case SingleType(Type prefix, Symbol member):
                return gen.mkApply__(pos,
                                     gen.mkRef(pos,
                                               prefixAsValue(pos, prefix),
                                               member));
            default:
                throw Debug.abort("unexpected prefix", pre);
            }
        }

        private Ancestor[][] computeDisplay0(Symbol classSym) {
            Type[] parentTypes = classSym.parents();

            Ancestor[][] superDisplay =
                computeDisplay(parentTypes[0].symbol());
            int level = superDisplay.length;
            ArrayList/*<Ancestor>*/[] display = new ArrayList[level + 1];

            // Start with the display of the super-class
            for (int l = 0; l < level; ++l) {
                Ancestor[] superRow = superDisplay[l];
                ArrayList newRow = new ArrayList();
                for (int i = 0; i < superRow.length; ++i)
                    newRow.add(new Ancestor(superRow[i].symbol, 0, i));
                display[l] = newRow;
            }

            display[level] = new ArrayList();
            display[level].add(new Ancestor(classSym, -1, -1));

            // Go over parents from left to right and add missing
            // ancestors to the display, remembering where they come
            // from.
            int parentIndex = 0;
            for (int p = 1; p < parentTypes.length; ++p) {
                Symbol parentSymbol = parentTypes[p].symbol();
                assert parentSymbol != Symbol.NONE;

                if (parentSymbol.isJava())
                    continue;

                ++parentIndex;
                Ancestor[][] parentDisplay = computeDisplay(parentSymbol);
                assert parentDisplay.length <= display.length;

                for (int l = 0; l < parentDisplay.length; ++l) {
                    ArrayList/*<Ancestor>*/ myRow = display[l];
                    Ancestor[] parentRow = parentDisplay[l];
                    for (int i = 0; i < parentRow.length; ++i) {
                        Symbol sym = parentRow[i].symbol;

                        Iterator myRowIt = myRow.iterator();
                        boolean alreadyExists = false;
                        while (!alreadyExists && myRowIt.hasNext()) {
                            Ancestor myAncestor = (Ancestor)myRowIt.next();
                            if (myAncestor.symbol == sym)
                                alreadyExists = true;
                        }

                        if (!alreadyExists)
                            myRow.add(new Ancestor(sym, parentIndex, i));
                    }
                }
            }

            Ancestor[][] finalDisplay = new Ancestor[level + 1][];
            for (int i = 0; i < finalDisplay.length; ++i) {
                finalDisplay[i] = (Ancestor[])
                    display[i].toArray(new Ancestor[display[i].size()]);
            }

            return finalDisplay;
        }

        private Ancestor[][] computeDisplay(Symbol classSym) {
            Ancestor[][] display = (Ancestor[][])displayCache.get(classSym);
            if (display == null) {
                display = computeDisplay0(classSym);
                displayCache.put(classSym, display);
            }
            return display;
        }

        private int[] getDisplayCode(Ancestor[][] display) {
            ArrayList/*<List<Ancestor>>*/ prunedRows = new ArrayList();

            int totalSize = 0;
            for (int l = 0; l < display.length; ++l) {
                Ancestor[] row = display[l];
                ArrayList/*<Ancestor>*/ prunedRow = new ArrayList(row.length);
                for (int i = 0; i < row.length; ++i) {
                    if (row[i].parentIndex > 0)
                        prunedRow.add(row[i]);
                }

                prunedRows.add(prunedRow);
                totalSize += 1 + 2 * prunedRow.size();
            }

            int[] res = new int[totalSize];
            int i = 0;
            Iterator rowsIt = prunedRows.iterator();
            while (rowsIt.hasNext()) {
                ArrayList row = (ArrayList)rowsIt.next();
                res[i++] = row.size();
                Iterator ancIt = row.iterator();
                while (ancIt.hasNext()) {
                    Ancestor anc = (Ancestor)ancIt.next();
                    res[i++] = anc.parentIndex;
                    res[i++] = anc.position;
                }
            }
            assert i == totalSize;
            return res;
        }
    }

    /*Pair<ArrayList<Refinement>, HashMap<Refinement,Origin>>*/
    private Pair refinements(Type[] parents, Scope members) {
        HashMap attribution = new HashMap();

        ArrayList/*<Refinement>*/ ref = refinements(members);
        attributeRefinements(attribution, ref, -1);

        for (int i = parents.length; i > 0; --i) {
            ArrayList parentRef =
                (ArrayList)(refinements(parents[i - 1]).fst);
            attributeRefinements(attribution, parentRef, i);
            ref = mergeRefinements(parentRef, ref);
        }

        return new Pair(ref, attribution);
    }

    /*Pair<ArrayList<Refinement>, HashMap<Refinement,Origin>>*/
    private Pair refinements(Type tp) {
        return refinements(tp.parents(), tp.members());
    }

    /** Return the refinements introduced by the given members */
    private ArrayList/*<Refinement>*/ refinements(Scope members) {
        ArrayList/*<Refinement>*/ myRefs = new ArrayList();
        Scope.SymbolIterator membersIt = members.iterator();
        while (membersIt.hasNext()) {
            Symbol mem = membersIt.next();
            Symbol origMem = originalSymbol(mem);
            if (origMem != Symbol.NONE && !mem.info().isSameAs(origMem.info()))
                myRefs.add(new Refinement(origMem, mem.info()));
        }
        Collections.sort(myRefs);
        return myRefs;
    }

    private ArrayList mergeRefinements(ArrayList rs1, ArrayList rs2) {
        ArrayList/*<Refinement>*/ res =
            new ArrayList(rs1.size() + rs2.size());

        int i1 = 0, i2 = 0;
        while (i1 < rs1.size() && i2 < rs2.size()) {
            Refinement r1 = (Refinement)rs1.get(i1);
            Refinement r2 = (Refinement)rs2.get(i2);
            int cmp = r1.compareTo(r2);
            if (cmp < 0) {
                res.add(r1);
                ++i1;
            } else if (cmp > 0) {
                res.add(r2);
                ++i2;
            } else {
                assert r2.type.isSubType(r1.type) : r2 + " !<: " + r1;
                res.add(r2);
                ++i1;
                ++i2;
            }
        }
        while (i1 < rs1.size())
            res.add(rs1.get(i1++));
        while (i2 < rs2.size())
            res.add(rs2.get(i2++));

        return res;
    }

    private void attributeRefinements(HashMap attribution,
                                      ArrayList refs,
                                      int parentIndex) {
        for (int i = 0; i < refs.size(); ++i)
            attribution.put(refs.get(i), new Origin(parentIndex, i));
    }

    private int[] refinementCode(ArrayList refinement, HashMap attribution) {
        int[] code = new int[1 + 2 * refinement.size()];
        int pc = 0;

        code[pc++] = refinement.size();
        Iterator refIt = refinement.iterator();
        while (refIt.hasNext()) {
            Origin orig = (Origin)attribution.get(refIt.next());
            code[pc++] = orig.parentIndex;
            code[pc++] = orig.position;
        }
        assert pc == code.length;

        return code;
    }

//     private Symbol originalSymbol(Symbol sym) {
//         Symbol orig = originalSymbol0(sym);
//         if (orig != Symbol.NONE)
//         System.out.println("orig for " + Debug.show(sym) + ": " + Debug.show(orig));
//         return orig;
//     }

    private Symbol originalSymbol(Symbol sym) {
        Type[] closure = sym.owner().closure();
        for (int i = closure.length; i > 0; --i) {
            Type parent = closure[i - 1];
            Symbol maybeOrig = sym.overriddenSymbol(parent, false);
            if (maybeOrig != Symbol.NONE)
                return maybeOrig;
        }
        return Symbol.NONE;
    }

    //////////////////////////////////////////////////////////////////////

    private static class TEnv {
        public boolean definesVar(Symbol sym) {
            return false;
        }
        public Tree treeForVar(Symbol sym) {
            throw Debug.abort("no tree for variable " + sym);
        }
    }

    private static class NewMember {
        public Symbol symbolToAdd() {
            switch (this) {
            case TypeAccessor(_, Symbol accSym):
                return accSym;
            case TypeConstructor(_, Symbol tcSym):
                return tcSym;
            case Instantiator(_, Symbol insSym):
                return insSym;
            case ClassInitialiser(_, Symbol ciSym, _):
                return ciSym;
            default:
                throw Debug.abort("unexpected case");
            }
        }

        public case TypeAccessor(Symbol memSym, Symbol accSym);
        public case TypeConstructor(Symbol memSym, Symbol tcSym);
        public case Instantiator(Symbol memSym, Symbol insSym);
        public case ClassInitialiser(Symbol memSym, Symbol ciSym, Symbol tcSym);
    }

    private static class Ancestor {
        public final Symbol symbol;
        public final int parentIndex;
        public final int position;

        public Ancestor(Symbol symbol, int parentIndex, int position) {
            this.symbol = symbol;
            this.parentIndex = parentIndex;
            this.position = position;
        }
    }

    private static class Origin {
        public final int parentIndex;
        public final int position;
        public Origin(int parentIndex, int position) {
            this.parentIndex = parentIndex;
            this.position = position;
        }
    }

    private static class Pair {
        public final Object fst, snd;
        public Pair(Object fst, Object snd) {
            this.fst = fst;
            this.snd = snd;
        }
    }

    private class Refinement implements Comparable {
        public final Symbol member;
        public final int hash;
        public final Type type;

        private final SymbolNameWriter symWriter = new SymbolNameWriter();

        public Refinement(Symbol member, Type type) {
            this.member = member;
            this.hash = memberHash(member);
            this.type = type;
        }

        public boolean equals(Object thatO) {
            return (thatO instanceof Refinement) && (compareTo(thatO) == 0);
        }

        public int compareTo(Object thatO) {
            Refinement that = (Refinement)thatO;
            if (this.hash < that.hash)
                return -1;
            else if (this.hash > that.hash)
                return 1;
            else
                return 0;
        }

        public String toString() {
            return "<" + hash + " " + member + " : " + type + ">";
        }

        private int memberHash(Symbol sym) {
            // TODO use FNV hash
            return symWriter.toString(sym).hashCode();
        }
    }
}
