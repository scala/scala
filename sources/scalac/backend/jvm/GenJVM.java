/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $OldId: GenJVM.java,v 1.29 2003/02/05 09:32:17 schinz Exp $
// $Id$

// TODO: create arrays with ANEWARRAY & friends

// TODO: (maybe) add InnerClass attributes to .class files

package scalac.backend.jvm;

import scalac.*;
import scalac.backend.*;
import scalac.util.*;
import scalac.ast.*;
import scalac.symtab.*;
import scalac.symtab.classfile.ClassfileConstants;
import scalac.transformer.*;

import org.apache.bcel.*;
import org.apache.bcel.generic.*;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.Type;

import java.util.*;
import java.io.*;

/**
 * Backend generating JVM byte-codes.
 *
 * @version 1.0
 * @author Michel Schinz
 */

public class GenJVM extends Phase {
    public GenJVM(Global global, PhaseDescriptor descr) {
	super(global, descr);
    }

    public void apply(Unit unit) {
        JVMGenerator gen = new JVMGenerator(global);
        gen.translate(unit);
    }
}

class JVMGenerator {
    protected final static String JAVA_LANG_OBJECT = "java.lang.Object";
    protected final static String JAVA_LANG_STRING = "java.lang.String";
    protected final static String JAVA_LANG_STRINGBUFFER = "java.lang.StringBuffer";
    protected final static String SCALA_RUNTIME_RUNTIME = "scala.runtime.RunTime";
    protected final static String SCALA_UNIT = "scala.Unit";

    protected final static String MODULE_INSTANCE_FIELD_NAME = "MODULE$";
    protected final static String VOID_NO_ARGS_SIG =
        Type.getMethodSignature(Type.VOID, Type.NO_ARGS);
    protected final static String[] EMPTY_STRING_ARRAY = new String[0];

    protected final static String UNIT_SIG =
        (new ObjectType(SCALA_UNIT)).getSignature();

    protected final static String CONSTRUCTOR_STRING =
        Constants.CONSTRUCTOR_NAME;
    protected final static Name CONSTRUCTOR_NAME =
        Name.fromString(CONSTRUCTOR_STRING).toConstrName();

    protected Unit unit = null;
    protected String sourceFileName = null;

    // Shortcut names for interfaces.
    protected final static InstructionConstants ic = null;
    protected final static Constants cst = null;

    protected final Global global;
    protected final Definitions defs;
    protected final Primitives prims;

    public JVMGenerator(Global global) {
        this.global = global;
        this.defs = global.definitions;
        this.prims = global.primitives;
        initTypeMap();
        initArithPrimMap();
    }

    public void translate(Unit unit) {
        this.unit = unit;
        sourceFileName = unit.source.toString();
        for (int i = 0; i < unit.body.length; ++i)
            gen(unit.body[i]);
        sourceFileName = null;
        this.unit = null;
    }

    // Context
    protected ClassGen currClass = null;
    protected String currClassName = null;
    protected ConstantPoolGen currPool = null;
    protected MethodGen currMethod = null;
    protected InstructionList currIL = null;
    protected Map currLocals = null;

    static class InstrContext {
        public case Empty;
        public case New;
        public case Assign;
        public case If(InstructionHandle target, boolean when);
    }

    protected void gen(Tree tree) {
        gen(tree, cst.T_VOID, InstrContext.Empty);
    }

    protected void gen(Tree tree, byte expectedType) {
        gen(tree, expectedType, InstrContext.Empty);
    }

    protected void gen(Tree tree, InstrContext ctx) {
        gen(tree, cst.T_VOID, ctx);
    }

    protected void gen(Tree[] trees) {
        for (int i = 0; i < trees.length; ++i)
            gen(trees[i]);
    }

    protected void gen(Tree[] trees, byte expectedType) {
        for (int i = 0; i < trees.length; ++i)
            gen(trees[i], expectedType);
    }

    protected void gen(Tree[] trees, InstrContext ctx) {
        for (int i = 0; i < trees.length; ++i)
            gen(trees[i], ctx);
    }

    protected void gen(Tree[] trees, byte expectedType, InstrContext ctx) {
        for (int i = 0; i < trees.length; ++i)
            gen(trees[i], expectedType, ctx);
    }

    protected void gen(Tree tree, byte expectedType, InstrContext ctx) {
        Symbol sym = tree.symbol();
        byte generatedType = cst.T_VOID;

        switch (tree) {
        case PackageDef(_, Tree.Template impl):
            gen(impl);
            break;

        case ClassDef(_, _, _, _, _, Tree.Template impl) : {
            Tree.ClassDef classDef = (Tree.ClassDef)tree;

            enterClass(sym);

            addValueClassMembers(classDef);
            if (Modifiers.Helper.isModClass(sym.flags))
                addModuleInstanceField();

            gen(impl);
            leaveClass(sym);
        } break;

        case Template(_, Tree[] body):
            gen(body);
            break;

        case ValDef(_, Name name, _, Tree rhs): {
            if (currMethod == null)
                break;          // ignore ValDefs in classes, handled elsewhere

            Type valType = typeStoJ(sym.info());
            LocalVariableGen lGen = currMethod.addLocalVariable(name.toString(),
                                                                valType,
                                                                currIL.getEnd(),
                                                                null);
            int index = lGen.getIndex();

            if (rhs != Tree.Empty)
                gen(rhs, valType.getType());
            else {
                switch (valType.getType()) {
                case cst.T_BOOLEAN:
                case cst.T_BYTE:
                case cst.T_CHAR:
                case cst.T_SHORT:
                case cst.T_INT:
                    currIL.append(new PUSH(currPool, 0)); break;
                case cst.T_LONG:
                    currIL.append(new PUSH(currPool, 0L)); break;
                case cst.T_FLOAT:
                    currIL.append(new PUSH(currPool, 0F)); break;
                case cst.T_DOUBLE:
                    currIL.append(new PUSH(currPool, 0D)); break;
                default:
                    currIL.append(ic.ACONST_NULL); break;
                }
            }
            currIL.append(new Generic_STORE(index, valType));

            currLocals.put(sym, new Integer(index));
        } break;

        case DefDef(_, _, _, _, _, Tree rhs): {
            enterMethod((Tree.DefDef)tree);
            if (! Modifiers.Helper.isAbstract(sym.flags)) {
                Type retType = currMethod.getReturnType();
                gen(rhs, retType.getType());
                currIL.append(new Generic_RETURN(retType));
            }
            leaveMethod();
        } break;

        case LabelDef(_, _):
            global.fail("not implemented yet " + tree);
            break;

        case Block(Tree[] stats): {
            int statsNum = stats.length;
            for (int i = 0; i < statsNum - 1; ++i)
                gen(stats[i], cst.T_VOID);
            if (statsNum == 0)
                maybeLoadUnit(expectedType);
            else
                gen(stats[stats.length - 1], expectedType, ctx);
            generatedType = expectedType;
        } break;

        case Typed(Tree expr, _):
            gen(expr, expectedType, ctx);
            generatedType = expectedType;
            break;

        case New(Tree.Template templ): {
            assert templ.body.length == 0;
            assert templ.parents.length == 1;

            String className = javaName(tree.type.symbol());
            currIL.append(new NEW(currPool.addClass(className)));
            currIL.append(ic.DUP);
            gen(templ.parents[0], InstrContext.New);

            generatedType = cst.T_OBJECT;
        } break;

        case Apply(TypeApply(Tree fun, Tree[] args), _): {
            Type type = typeStoJ(args[0].type);
            int typeIndex;
            if (type instanceof ObjectType)
                typeIndex = currPool.addClass((ObjectType)type);
            else if (type instanceof ArrayType)
                typeIndex = currPool.addArrayClass((ArrayType)type);
            else
                throw global.fail("unexpected type " + type);

            genLoadQualifier(fun);

            if (fun.symbol() == defs.IS) {
                currIL.append(new INSTANCEOF(typeIndex));
                generatedType = cst.T_BOOLEAN;
            } else if (fun.symbol() == defs.AS) {
                currIL.append(new CHECKCAST(typeIndex));
                generatedType = type.getType();
            } else
                global.fail("unexpected type application");
        } break;

        case Apply(Tree fun, Tree[] args): {
            if (isPrimitive(fun.symbol())) {
                Tree.Select selectFun = (Tree.Select)fun;
                Primitive prim = prims.getPrimitive(fun.symbol());

                if (prim == Primitive.CONCAT) {
                    genStringConcatenation(liftStringConcatenations(tree));
                    generatedType = cst.T_OBJECT;
                } else {
                    Tree[] allArgs = new Tree[args.length + 1];
                    allArgs[0] = unbox(selectFun.qualifier);
                    System.arraycopy(args, 0, allArgs, 1, args.length);
                    generatedType = genPrimitive(prim,
                                                 allArgs,
                                                 typeStoJ(tree.type).getType(),
                                                 expectedType,
                                                 ctx);
                }
            } else {
                Symbol funSym = fun.symbol();
                Type[] argTypes = argTypesStoJ(funSym.info());
                Type retType = retTypeStoJ(funSym.info());
                boolean isStatic = isStaticMember(funSym);
                if (!isStatic && ctx != InstrContext.New)
                    genLoadQualifier(fun);
                for (int i = 0; i < args.length; ++i)
                    gen(args[i], argTypes[i].getType());

                String className = javaName(funSym.owner());
                String methodName = funSym.name.toString();
                String methodSig = Type.getMethodSignature(retType, argTypes);

                if (funSym.owner().isInterface()) {
                    int methodIndex =
                        currPool.addInterfaceMethodref(className, methodName, methodSig);
                    int argsSize = 1;

                    for (int i = 0; i < args.length; ++i)
                        argsSize += argTypes[i].getSize();
                    currIL.append(new INVOKEINTERFACE(methodIndex, argsSize));
                } else {
                    int methodIndex =
                        currPool.addMethodref(className, methodName, methodSig);
                    boolean isSpecial;
                    if (funSym.name == CONSTRUCTOR_NAME)
                        isSpecial = true;
                    else {
                        switch (fun) {
                        case Select(Super(_), _): isSpecial = true; break;
                        default: isSpecial = false; break;
                        }
                    }
                    if (isSpecial)
                        currIL.append(new INVOKESPECIAL(methodIndex));
                    else if (isStatic)
                        currIL.append(new INVOKESTATIC(methodIndex));
                    else
                        currIL.append(new INVOKEVIRTUAL(methodIndex));
                }

                generatedType = retType.getType();
            }
        } break;

        case Ident(Name name): {
            Type type = typeStoJ(sym.info());
            if (sym.isModule())
                generatedType = genLoadModule(sym);
            else if (sym == defs.NULL) {
                currIL.append(ic.ACONST_NULL);
                generatedType = expectedType;
            } else if (sym.owner().isClass()) {
                currIL.append(ic.THIS);
                int fieldIdx = currPool.addFieldref(currClassName,
                                                    name.toString(),
                                                    type.getSignature());
                if (ctx == InstrContext.Assign) {
                    currIL.append(new PUTFIELD(fieldIdx));
                    generatedType = cst.T_VOID;
                } else {
                    currIL.append(new GETFIELD(fieldIdx));
                    generatedType = type.getType();
                }
            } else {
                assert currLocals.containsKey(sym)
                    : Debug.show(sym) + " not in " + currLocals;
                int pos = ((Integer)currLocals.get(sym)).intValue();
                if (ctx == InstrContext.Assign) {
                    currIL.append(new Generic_STORE(pos, type));
                    generatedType = cst.T_VOID;
                } else {
                    currIL.append(new Generic_LOAD(pos, type));
                    generatedType = type.getType();
                }
            }
        } break;

        case Select(Tree qualifier, Name selector): {
            if (sym.isModule())
                generatedType = genLoadModule(sym);
            else {
                Type fieldType = typeStoJ(sym.info());
                int fieldIdx = currPool.addFieldref(javaName(sym.owner()),
                                                    selector.toString(),
                                                    fieldType.getSignature());
                if (isStaticMember(sym)) {
                    if (ctx == InstrContext.Assign) {
                        currIL.append(new PUTSTATIC(fieldIdx));
                        generatedType = cst.T_VOID;
                    } else {
                        currIL.append(new GETSTATIC(fieldIdx));
                        generatedType = fieldType.getType();
                    }
                } else {
                    genLoadQualifier(tree);
                    if (ctx == InstrContext.Assign) {
                        currIL.append(new PUTFIELD(fieldIdx));
                        generatedType = cst.T_VOID;
                    } else {
                        currIL.append(new GETFIELD(fieldIdx));
                        generatedType = fieldType.getType();
                    }
                }
            }
        } break;

        case Assign(Tree lhs, Tree rhs): {
            gen(lhs, InstrContext.Assign);
            InstructionHandle storeHandle = currIL.getEnd();
            gen(rhs, typeStoJ(lhs.symbol().info()).getType());
            // Work around BCEL bug (see below)
            currIL.move(storeHandle, currIL.append(ic.NOP).getPrev());
        } break;

        case If(Tree cond, Tree thenp, Tree elsep): {
            byte finalType = typeStoJ(tree.type).getType();

            InstructionHandle fakeElseH = currIL.append(ic.NOP);
            gen(cond, cst.T_VOID, new InstrContext.If(fakeElseH, false));
            InstructionHandle thenH = currIL.append(ic.NOP);
            gen(thenp, finalType);
            BranchInstruction gotoAfter = new GOTO(null);
            currIL.append(gotoAfter);
            InstructionHandle elseH = currIL.append(ic.NOP);
            if (elsep == Tree.Empty)
                maybeLoadUnit(finalType);
            else
                gen(elsep, finalType);
            gotoAfter.setTarget(currIL.append(ic.NOP));
            // We cannot move the instructions sooner because BCEL has
            // a bug which makes it impossible to move instructions at
            // the end of the list.
            currIL.move(fakeElseH, elseH);
            generatedType = finalType;
        } break;

        case This(_):
            currIL.append(ic.THIS);
            generatedType = cst.T_OBJECT;
            break;

        case Literal(Object value):
            if (value instanceof Integer) {
                generatedType = cst.T_INT;
                currIL.append(new PUSH(currPool, (Integer)value));
            } else if (value instanceof Long) {
                generatedType = cst.T_LONG;
                currIL.append(new PUSH(currPool, (Long)value));
            } else if (value instanceof Float) {
                generatedType = cst.T_FLOAT;
                currIL.append(new PUSH(currPool, (Float)value));
            } else if (value instanceof Double) {
                generatedType = cst.T_DOUBLE;
                currIL.append(new PUSH(currPool, (Double)value));
            } else if (value instanceof Character) {
                generatedType = cst.T_CHAR;
                currIL.append(new PUSH(currPool, (Character)value));
            } else if (value instanceof String) {
                generatedType = cst.T_OBJECT;
                currIL.append(new PUSH(currPool, (String)value));
            } else
                throw global.fail("unknown literal " + value);
            break;

        case Empty:
        case TypeDef(_, _, _):
        case TypeApply(_, _):
        case FunType(_, _):
        case CompoundType(_, _):
        case CovariantType(_):
        case AppliedType(_,_):
            break;

        case Tuple(_):
        case Super(_):
        case ModuleDef(_,_,_,_):
        case PatDef(_,_,_):
        case Import(_, _):
        case CaseDef(_, _, _):
        case Visitor(_):
        case Function(_, _):
            throw global.fail("unexpected node", tree);
        case Bad():
            throw global.fail("bad tree");
        default:
            throw global.fail("unknown node", tree);
        }

        // Pop unneeded result from stack, or widen it if needed.
        if (expectedType == cst.T_VOID && generatedType != cst.T_VOID) {
            if (generatedType == cst.T_LONG || generatedType == cst.T_DOUBLE)
                currIL.append(ic.POP2);
            else {
                switch (ctx) {
                case If(InstructionHandle target, boolean when):
                    assert generatedType == cst.T_BOOLEAN : generatedType;
                    currIL.append(when ? new IFNE(target) : new IFEQ(target));
                    break;
                default:
                    currIL.append(ic.POP);
                }
            }
        } else if (! (expectedType == cst.T_VOID
                      || generatedType == expectedType
                      || (generatedType == cst.T_ARRAY
                          && expectedType == cst.T_OBJECT)))
            genWidenConversion(generatedType, expectedType);
    }

    protected Tree unbox(Tree tree) {
        switch (tree) {
        case Apply(Tree fun, Tree[] args):
            if (prims.getPrimitive(fun.symbol()) == Primitive.BOX) {
                assert args.length == 1;
                return args[0];
            } else
                return tree;
        case Block(Tree[] stats):
            if (stats.length == 2
                && prims.getPrimitive(stats[1].symbol()) == Primitive.BOX) {
                return stats[0];
            } else
                return tree;
        default:
            return tree;
        }
    }

    // Add field containing module instance, and code to
    // initialize it, to current class.
    protected void addModuleInstanceField() {
        Type currClassType = new ObjectType(currClassName);
        FieldGen instanceField =
            new FieldGen(cst.ACC_PUBLIC
                         | cst.ACC_FINAL
                         | cst.ACC_STATIC,
                         currClassType,
                         MODULE_INSTANCE_FIELD_NAME,
                         currPool);
        currClass.addField(instanceField.getField());

        InstructionList initIL = new InstructionList();

        int constrRef = currPool.addMethodref(currClassName,
                                              CONSTRUCTOR_STRING,
                                              VOID_NO_ARGS_SIG);
        int fieldRef = currPool.addFieldref(currClassName,
                                            MODULE_INSTANCE_FIELD_NAME,
                                            currClassType.getSignature());

        initIL.append(new NEW(currPool.addClass(currClassName)));
        initIL.append(ic.DUP);
        initIL.append(new INVOKESPECIAL(constrRef));
        initIL.append(new PUTSTATIC(fieldRef));
        initIL.append(ic.RETURN);

        MethodGen initMethod =
            new MethodGen(cst.ACC_PUBLIC | cst.ACC_STATIC,
                          Type.VOID, Type.NO_ARGS, Strings.NONE,
                          "<clinit>",
                          currClassName,
                          initIL,
                          currPool);
        initMethod.setMaxStack();
        currClass.addMethod(initMethod.getMethod());
    }

    // Add value members (i.e. fields) to current class.
    protected void addValueClassMembers(Tree.ClassDef cDef) {
        Symbol cSym = cDef.symbol();
        Scope.SymbolIterator memberIt = cSym.members().iterator();
        while (memberIt.hasNext()) {
            Symbol member = memberIt.next();
            if (member.isTerm() && !member.isMethod()) {
                FieldGen fGen = new FieldGen(modifiersStoJ(member.flags),
                                             typeStoJ(member.info()),
                                             member.name.toString(),
                                             currPool);
                currClass.addField(fGen.getField());
            }
        }
    }

    protected void maybeLoadUnit(byte expectedType) {
        if (expectedType == cst.T_OBJECT) {
            int unitFieldRef = currPool.addFieldref(SCALA_RUNTIME_RUNTIME,
                                                    "UNIT_VAL",
                                                    UNIT_SIG);
            currIL.append(new GETSTATIC(unitFieldRef));
        }
    }

    protected byte genLoadModule(Symbol sym) {
        int moduleInstIdx =
            currPool.addFieldref(javaName(sym),
                                 MODULE_INSTANCE_FIELD_NAME,
                                 typeStoJ(sym.info()).getSignature());
        currIL.append(new GETSTATIC(moduleInstIdx));
        return cst.T_OBJECT;
    }

    protected void genLoadQualifier(Tree tree) {
        switch (tree) {
        case Select(Super(_), _):
        case Ident(_):
            currIL.append(ic.THIS);
            break;
        case Select(Tree qualifier, _):
            gen(qualifier, cst.T_OBJECT);
            break;
        default:
            throw global.fail("unknown qualifier");
        }
    }

    protected boolean isStaticMember(Symbol sym) {
        return (sym.name != CONSTRUCTOR_NAME)
            && sym.owner().isModuleClass()
            && sym.owner().isJava();
    }

    protected boolean isPrimitive(Symbol sym) {
        if (prims.isPrimitive(sym)) {
            switch (prims.getPrimitive(sym)) {
            case POS : case NEG :
            case ADD : case SUB : case MUL : case DIV : case MOD :
            case NOT : case OR : case XOR : case AND :
            case LSL : case LSR : case ASR :
            case EQ : case NE : case LT : case LE : case GE : case GT :
            case ZNOT : case ZOR : case ZAND :
            case NEW_ZARRAY : case NEW_BARRAY : case NEW_SARRAY :
            case NEW_CARRAY : case NEW_IARRAY : case NEW_LARRAY :
            case NEW_FARRAY : case NEW_DARRAY :
            case ZARRAY_GET : case BARRAY_GET : case SARRAY_GET :
            case CARRAY_GET : case IARRAY_GET : case LARRAY_GET :
            case FARRAY_GET : case DARRAY_GET : case OARRAY_GET :
            case ZARRAY_SET : case BARRAY_SET : case SARRAY_SET :
            case CARRAY_SET : case IARRAY_SET : case LARRAY_SET :
            case FARRAY_SET : case DARRAY_SET : case OARRAY_SET :
            case LENGTH :
            case IS : case AS :
            case CONCAT :
            case THROW :
            case AS_UVALUE :
                return true;

            case AS_ZVALUE : case AS_BVALUE : case AS_SVALUE :
            case AS_CVALUE : case AS_IVALUE : case AS_LVALUE :
            case AS_FVALUE : case AS_DVALUE :
            case AS_ZARRAY : case AS_BARRAY : case AS_SARRAY :
            case AS_CARRAY : case AS_IARRAY : case AS_LARRAY :
            case AS_FARRAY : case AS_DARRAY : case AS_OARRAY :
            case NEW_OARRAY :
            case EQUALS  :
            case HASHCODE :
            case TOSTRING :
            case BOX :
            case APPLY : case UPDATE :
                return false;
            default:
                throw Debug.abort("unknown primitive", sym);
            }
        } else
            return false;
    }

    protected byte genPrimitive(Primitive prim,
                                Tree[] args,
                                byte resType,
                                byte expectedType,
                                InstrContext ctx) {
        switch (prim) {
        case POS: case NEG:
        case ADD: case SUB: case MUL: case DIV: case MOD:
        case NOT: case OR : case XOR: case AND:
        case LSL: case LSR: case ASR:
            return genArithPrim(prim, args, resType, expectedType, ctx);
        case EQ: case NE: case LT: case LE: case GE: case GT:
        case ZNOT: case ZOR: case ZAND:
            return genCompOrLogicalPrim(prim, args, resType, expectedType, ctx);
        case THROW:
            assert args.length == 1;
            return genThrow(args[0]);
        case NEW_ZARRAY :
        case NEW_BARRAY :
        case NEW_SARRAY :
        case NEW_CARRAY :
        case NEW_IARRAY :
        case NEW_LARRAY :
        case NEW_FARRAY :
        case NEW_DARRAY :
            return genArrayCreate(prim, args[1]);
//         case NEW_OARRAY :
//             return genArrayCreate(prim, args[1], args[2]);
        case ZARRAY_SET : case BARRAY_SET : case SARRAY_SET :
        case CARRAY_SET : case IARRAY_SET : case LARRAY_SET :
        case FARRAY_SET : case DARRAY_SET : case OARRAY_SET :
            assert args.length == 4;
            return genArrayUpdate(args[1], args[2], args[3]);
        case ZARRAY_GET : case BARRAY_GET : case SARRAY_GET :
        case CARRAY_GET : case IARRAY_GET : case LARRAY_GET :
        case FARRAY_GET : case DARRAY_GET : case OARRAY_GET :
            assert args.length == 3;
            return genArrayAccess(args[1], args[2]);
        case LENGTH:
            assert args.length == 1;
            return genArrayLength(args[0]);
        case AS_UVALUE :
            assert args.length == 1;
            gen(args[0], cst.T_VOID);
            return cst.T_VOID;
        default:
            throw Debug.abort("unknown primitive ", prim);
        }
    }

    protected Map/*<Primitive, Instruction>*/ arithPrimMap;
    protected void addPrim(Primitive prim,
                           Instruction z,
                           Instruction i,
                           Instruction l,
                           Instruction f,
                           Instruction d) {
        arithPrimMap.put(prim, new Instruction[] { z, i, l, f, d });
    }

    protected void initArithPrimMap() {
        arithPrimMap = new HashMap();
        /*                      boolean  int ...    long       float     double */
        addPrim(Primitive.ADD , null,    ic.IADD  , ic.LADD  , ic.FADD , ic.DADD);
        addPrim(Primitive.SUB , null,    ic.ISUB  , ic.LSUB  , ic.FSUB , ic.DSUB);
        addPrim(Primitive.MUL , null,    ic.IMUL  , ic.LMUL  , ic.FMUL , ic.DMUL);
        addPrim(Primitive.DIV , null,    ic.IDIV  , ic.LDIV  , ic.FDIV , ic.DDIV);
        addPrim(Primitive.MOD , null,    ic.IREM  , ic.LREM  , ic.FREM , ic.DREM);
        addPrim(Primitive.AND , ic.IAND, ic.IAND  , ic.LAND  , null    , null);
        addPrim(Primitive.OR  , ic.IOR,  ic.IOR   , ic.LOR   , null    , null);
        addPrim(Primitive.XOR , ic.IXOR, ic.IXOR  , ic.LXOR  , null    , null);
        addPrim(Primitive.LSL , null,    ic.ISHL  , ic.LSHL  , null    , null);
        addPrim(Primitive.LSR , null,    ic.IUSHR , ic.LUSHR , null    , null);
        addPrim(Primitive.ASR , null,    ic.ISHR  , ic.LSHR  , null    , null);
        addPrim(Primitive.POS , null,    null     , null     , null    , null);
        addPrim(Primitive.NEG , null,    ic.INEG  , ic.LNEG  , ic.FNEG , ic.DNEG);
    }

    protected byte genArithPrim(Primitive prim,
                                Tree[] args,
                                byte resType,
                                byte expectedType,
                                InstrContext ctx) {
        int arity = args.length;
        int resTypeIdx = getTypeIndex(resType);

        for (int i = 0; i < arity; ++i)
            gen(args[i], resType);

        if (prim == Primitive.NOT) {
            assert resType == cst.T_INT || resType == cst.T_LONG;
            boolean isLong = (resType == cst.T_LONG);
            if (isLong) {
                currIL.append(new PUSH(currPool, -1L));
                currIL.append(ic.LXOR);
            } else {
                currIL.append(new PUSH(currPool, -1));
                currIL.append(ic.IXOR);
            }
        } else {
            assert arithPrimMap.containsKey(prim);
            Instruction primInst = ((Instruction[])arithPrimMap.get(prim))[resTypeIdx];
            if (primInst != null)
                currIL.append(primInst);
        }
        return resType;
    }

    protected byte genCompOrLogicalPrim(Primitive prim,
                                        Tree[] args,
                                        byte resType,
                                        byte expectedType,
                                        InstrContext ctx) {
        // Ensure that all comparisons happen in the context of an
        // "if".
        InstructionHandle target;
        boolean when;
        InstructionList epilogue = new InstructionList();
        byte realResType;
        switch (ctx) {
        case InstrContext.If(InstructionHandle t, boolean w):
            target = t; when = w; realResType = cst.T_VOID; break;
        default:
            epilogue.append(ic.ICONST_1);
            BranchInstruction gotoAfter = new GOTO(null);
            epilogue.append(gotoAfter);
            target = epilogue.append(ic.ICONST_0);
            gotoAfter.setTarget(epilogue.append(ic.NOP));
            when = false;
            realResType = cst.T_BOOLEAN;
            break;
        }

        if (prim == Primitive.ZNOT
            || prim == Primitive.ZOR
            || prim == Primitive.ZAND)
            genLogicalPrim(prim, args, resType, expectedType, target, when);
        else
            genCompPrim(prim, args, resType, expectedType, target, when);

        currIL.append(epilogue);
        return realResType;
    }

    protected byte getMaxType(Tree[] trees) {
        byte maxType = cst.T_BOOLEAN;
        int maxTypeIdx = getTypeIndex(maxType);

        for (int i = 0; i < trees.length; ++i) {
            byte argType = typeStoJ(trees[i].type).getType();
            if (getTypeIndex(argType) > maxTypeIdx) {
                maxType = argType;
                maxTypeIdx = getTypeIndex(maxType);
            }
        }
        return maxType;
    }

    protected static int tempCounter = 1;
    protected void genCompPrim(Primitive prim,
                               Tree[] args,
                               byte resType,
                               byte expectedType,
                               InstructionHandle target,
                               boolean when) {
        byte maxType = getMaxType(args);
        int maxTypeIdx = getTypeIndex(maxType);
        int intTypeIdx = getTypeIndex(Type.INT);
        boolean intCompareWithZero = false;
        for (int i = 0; i < args.length; ++i) {
            boolean isIntZero = false;
            switch (args[i]) {
            case Literal(Object val):
                if ((maxTypeIdx <= intTypeIdx) && ((Number)val).intValue() == 0) {
                    isIntZero = true;
                    if (i == 0) prim = prim.swap();
                }
            }
            if (intCompareWithZero || !isIntZero)
                gen(args[i], maxType);
            intCompareWithZero |= isIntZero;
        }

        if (maxType == cst.T_OBJECT) {
            assert prim == Primitive.EQ || prim == Primitive.NE;

            LocalVariableGen lGen =
                currMethod.addLocalVariable("temp" + tempCounter++,
                                            Type.OBJECT,
                                            currIL.getEnd(),
                                            null);
            currIL.append(new ASTORE(lGen.getIndex()));
            currIL.append(ic.DUP);
            BranchInstruction ifNonNull = new IFNONNULL(null);
            currIL.append(ifNonNull);
            currIL.append(ic.POP);
            currIL.append(new ALOAD(lGen.getIndex()));
            if (when ^ (prim != Primitive.EQ))
                currIL.append(new IFNULL(target));
            else
                currIL.append(new IFNONNULL(target));
            BranchInstruction gotoAfter = new GOTO(null);
            currIL.append(gotoAfter);
            InstructionHandle nonNullHandle =
                currIL.append(new ALOAD(lGen.getIndex()));
            ifNonNull.setTarget(nonNullHandle);
            lGen.setEnd(nonNullHandle);
            String equalsSig =
                Type.getMethodSignature(Type.BOOLEAN,
                                        new Type[] { Type.OBJECT });
            int equalsIndex =
                currPool.addMethodref(JAVA_LANG_OBJECT, "equals", equalsSig);
            currIL.append(new INVOKEVIRTUAL(equalsIndex));
            if (when ^ (prim != Primitive.EQ))
                currIL.append(new IFNE(target));
            else
                currIL.append(new IFEQ(target));
            gotoAfter.setTarget(currIL.append(ic.NOP));
        } else if (maxTypeIdx <= intTypeIdx && !intCompareWithZero) {
            switch (maybeNegatedPrim(prim, !when)) {
            case LT: currIL.append(new IF_ICMPLT(target)); break;
            case LE: currIL.append(new IF_ICMPLE(target)); break;
            case EQ: currIL.append(new IF_ICMPEQ(target)); break;
            case NE: currIL.append(new IF_ICMPNE(target)); break;
            case GE: currIL.append(new IF_ICMPGE(target)); break;
            case GT: currIL.append(new IF_ICMPGT(target)); break;
            default: throw global.fail("unknown primitive " + prim);
            }
        } else {
            switch (maxType) {
            case cst.T_LONG:   currIL.append(ic.LCMP);  break;
            case cst.T_FLOAT:  currIL.append(ic.FCMPG); break;
            case cst.T_DOUBLE: currIL.append(ic.DCMPG); break;
            default:
                ;               // do nothing (int comparison with 0)
            }
            switch (maybeNegatedPrim(prim, !when)) {
            case LT: currIL.append(new IFLT(target)); break;
            case LE: currIL.append(new IFLE(target)); break;
            case EQ: currIL.append(new IFEQ(target)); break;
            case NE: currIL.append(new IFNE(target)); break;
            case GE: currIL.append(new IFGE(target)); break;
            case GT: currIL.append(new IFGT(target)); break;
            default: throw global.fail("unknown primitive " + prim);
            }
        }
    }

    protected Primitive maybeNegatedPrim(Primitive prim, boolean negate) {
        return negate ? prim.negate() : prim;
    }

    protected void genLogicalPrim(Primitive prim,
                                  Tree[] args,
                                  byte resType,
                                  byte expectedType,
                                  InstructionHandle target,
                                  boolean when) {
        if (prim == Primitive.ZNOT)
            gen(args[0], new InstrContext.If(target, !when));
        else {
            InstructionHandle fakeAfterH = currIL.append(ic.NOP);
            if (when ^ (prim == Primitive.ZAND)) {
                // x || y jump if true  or  x && y jump if false
                gen(args[0], new InstrContext.If(target, when));
                gen(args[1], new InstrContext.If(target, when));
            } else {
                // x || y jump if false  or  x && y jump if true
                gen(args[0], new InstrContext.If(fakeAfterH, !when));
                gen(args[1], new InstrContext.If(target, when));
                currIL.move(fakeAfterH, currIL.append(ic.NOP).getPrev());
            }
        }
    }

    protected byte genThrow(Tree arg) {
        gen(arg, cst.T_OBJECT);
        currIL.append(new CHECKCAST(currPool.addClass("java.lang.Throwable")));
        currIL.append(ic.ATHROW);
        return cst.T_OBJECT;
    }

    protected byte genArrayCreate(Primitive prim, Tree size) {
        gen(size, cst.T_INT);
        byte type;
        switch (prim) {
        case NEW_ZARRAY : type = cst.T_BOOLEAN; break;
        case NEW_BARRAY : type = cst.T_BYTE;    break;
        case NEW_SARRAY : type = cst.T_SHORT;   break;
        case NEW_CARRAY : type = cst.T_CHAR;    break;
        case NEW_IARRAY : type = cst.T_INT;     break;
        case NEW_LARRAY : type = cst.T_LONG;    break;
        case NEW_FARRAY : type = cst.T_FLOAT;   break;
        case NEW_DARRAY : type = cst.T_DOUBLE;  break;
        default: throw Debug.abort("unexpected primitive", prim);
        }
        currIL.append(new NEWARRAY(type));
        return cst.T_ARRAY;
    }

    protected byte genArrayUpdate(Tree array, Tree index, Tree value) {
        ArrayType arrayType = (ArrayType)typeStoJ(array.type);
        Type elemType = arrayType.getElementType();
        gen(array, cst.T_ARRAY);
        gen(index, cst.T_INT);
        if (elemType instanceof BasicType)
            value = unbox(value);
        gen(value, elemType.getType());
        currIL.append(new Generic_ASTORE(elemType));
        return cst.T_VOID;
    }

    protected byte genArrayAccess(Tree array, Tree index) {
        ArrayType arrayType = (ArrayType)typeStoJ(array.type);
        Type elemType = arrayType.getElementType();
        gen(array, cst.T_ARRAY);
        gen(index, cst.T_INT);
        currIL.append(new Generic_ALOAD(elemType));
        return elemType.getType();
    }

    protected byte genArrayLength(Tree array) {
        gen(array, cst.T_ARRAY);
        currIL.append(ic.ARRAYLENGTH);
        return cst.T_INT;
    }

    protected Tree[] liftStringConcatenations(Tree tree) {
        LinkedList accu = new LinkedList();
        liftStringConcatenations(tree, accu);
        return (Tree[])accu.toArray(new Tree[accu.size()]);
    }

    protected void liftStringConcatenations(Tree tree, LinkedList accu) {
        switch (tree) {
        case Apply(Select(Tree qualifier, Name selector), Tree[] args): {
            Symbol funSym = ((Tree.Apply)tree).fun.symbol();
            if  (prims.isPrimitive(funSym)
                 && prims.getPrimitive(funSym) == Primitive.CONCAT) {
                liftStringConcatenations(qualifier, accu);
                liftStringConcatenations(args[0], accu);
            } else
                accu.addLast(tree);
            } break;
        default:
            accu.addLast(tree);
        }
    }

    protected void genStringConcatenation(Tree[] elements) {
        ObjectType strBufType = new ObjectType (JAVA_LANG_STRINGBUFFER);

        int constRef = currPool.addMethodref(JAVA_LANG_STRINGBUFFER,
                                             "<init>",
                                             VOID_NO_ARGS_SIG);
        currIL.append (new NEW (currPool.addClass (JAVA_LANG_STRINGBUFFER)));
        currIL.append (ic.DUP);
        currIL.append (new INVOKESPECIAL (constRef));

        for (int i = 0; i < elements.length; ++i) {
            Type elemType = typeStoJ(elements[i].type);
            if (!elemType.equals(Type.STRING)
                && elemType.getType() == cst.T_OBJECT)
                elemType = Type.OBJECT;
            String appendSig =
                Type.getMethodSignature(strBufType, new Type[] { elemType });
            int appendRef =
                currPool.addMethodref(JAVA_LANG_STRINGBUFFER, "append", appendSig);
            gen(elements[i], elemType.getType());
            currIL.append(new INVOKEVIRTUAL(appendRef));
        }

        String toStringSig = Type.getMethodSignature (Type.STRING, Type.NO_ARGS);
        final int toStringRef =
            currPool.addMethodref (JAVA_LANG_STRINGBUFFER, "toString", toStringSig);
        currIL.append (new INVOKEVIRTUAL (toStringRef));
    }

    protected int getTypeIndex(Type tp) {
        return getTypeIndex(tp.getType());
    }

    protected int getTypeIndex(byte tp) {
        switch (tp) {
        case cst.T_BOOLEAN: return 0;
        case cst.T_BYTE:
        case cst.T_CHAR:
        case cst.T_SHORT:
        case cst.T_INT:     return 1;
        case cst.T_LONG:    return 2;
        case cst.T_FLOAT:   return 3;
        case cst.T_DOUBLE:  return 4;
        case cst.T_ARRAY:
        case cst.T_OBJECT:  return 5;
        default: return -1;
        }
    }

    protected Instruction[][] WIDENING_CONVERSION_TABLE = {
        /*               bool  int    long     float    double */
        /* boolean */  { null, null , null   , null   , null   },
        /* int ... */  { null, null , ic.I2L , ic.I2F , ic.I2D },
        /* long */     { null, null , null   , ic.L2F , ic.L2D },
        /* float */    { null, null , null   , null   , ic.F2D },
        /* double */   { null, null , null   , null   , null   }
    };

    protected void genWidenConversion(byte origType, byte finalType) {
        int origIdx = getTypeIndex(origType);
        int finalIdx = getTypeIndex(finalType);

        assert (origIdx <= 4 && finalIdx <= 4)
            : cst.TYPE_NAMES[origType] + " -> " + cst.TYPE_NAMES[finalType];
        Instruction instr = WIDENING_CONVERSION_TABLE[origIdx][finalIdx];
        if (instr != null)
            currIL.append(instr);
    }

    protected void dumpModuleMainClass(ClassGen modClassGen) {
        String moduleName = modClassGen.getClassName();
        String mainClassName = moduleName.substring(0, moduleName.length() - 1);
        ClassGen mainClassGen = new ClassGen(mainClassName,
                                             JAVA_LANG_OBJECT,
                                             sourceFileName,
                                             cst.ACC_SUPER
                                             | cst.ACC_PUBLIC
                                             | cst.ACC_FINAL,
                                             Strings.NONE);
        ConstantPoolGen mainPool = mainClassGen.getConstantPool();

        Method[] methods = modClassGen.getMethods();
        for (int i = 0; i < methods.length; ++i) {
            Method m = methods[i];
            if (m.isProtected() || m.isPrivate() || m.isStatic()
                || m.getName().equals("<init>"))
                continue;

            MethodGen mGen = new MethodGen(m, moduleName, mainPool);

            Type[] argTypes = mGen.getArgumentTypes();
            Type retType = mGen.getReturnType();

            InstructionList mainIL = new InstructionList();
            MethodGen mainMGen = new MethodGen(m.getAccessFlags() | cst.ACC_STATIC,
                                               retType,
                                               argTypes, mGen.getArgumentNames(),
                                               mGen.getName(),
                                               mainClassName,
                                               mainIL,
                                               mainPool);
            int moduleFieldIndex =
                mainPool.addFieldref(moduleName,
                                     MODULE_INSTANCE_FIELD_NAME,
                                     (new ObjectType(moduleName)).getSignature());
            int mIndex = mainPool.addMethodref(mGen);

            mainIL.append(new GETSTATIC(moduleFieldIndex));
            int pos = 0;
            for (int j = 0; j < argTypes.length; ++j) {
                mainIL.append(new Generic_LOAD(pos, argTypes[j]));
                pos += argTypes[j].getSize();
            }
            mainIL.append(new INVOKEVIRTUAL(mIndex));
            mainIL.append(new Generic_RETURN(retType));
            mainMGen.setMaxStack();
            mainClassGen.addMethod(mainMGen.getMethod());
        }

        addScalaAttr(mainClassGen);
        JavaClass mainClass = mainClassGen.getJavaClass();
        try {
            mainClass.dump(javaFileName(mainClassName));
        } catch (java.io.IOException e) {
            throw global.fail(e.getMessage());
        }
    }


    protected void addScalaAttr(ClassGen classGen) {
        ConstantPoolGen poolGen = classGen.getConstantPool();

        int scalaNameIndex =
            poolGen.addUtf8(ClassfileConstants.SCALA_N.toString());
        Unknown scalaAttr = new Unknown(scalaNameIndex,
                                        0,
                                        null,
                                        poolGen.getConstantPool());

        classGen.addAttribute(scalaAttr);
    }

    // Context manipulation

    protected LinkedList/*<ClassGen>*/ classStack = new LinkedList();

    protected void enterClass(Symbol cSym) {
        String javaName = javaName(cSym);

        scalac.symtab.Type[] baseTps = cSym.info().parents();

        String superClassName;
        String[] interfaceNames;
        if (cSym.isInterface()) {
            superClassName = JAVA_LANG_OBJECT;
            if (baseTps.length == 1 && baseTps[0] == defs.ANY_TYPE)
                interfaceNames = EMPTY_STRING_ARRAY;
            else {
                interfaceNames = new String[baseTps.length];
                for (int i = 0; i < baseTps.length; ++i) {
                    Symbol baseSym = baseTps[i].symbol();
                    assert baseSym.isInterface() : cSym + " implements " + baseSym;
                    interfaceNames[i] = javaName(baseSym);
                }
            }
        } else {
            superClassName = javaName(baseTps[0].symbol());
            interfaceNames = new String[Math.max(0, baseTps.length - 1)];
            for (int i = 1; i < baseTps.length; ++i) {
                Symbol baseSym = baseTps[i].symbol();
                assert baseSym.isInterface() : cSym + " implements " + baseSym;
                interfaceNames[i-1] = javaName(baseSym);
            }
        }

        ClassGen cGen = new ClassGen(javaName,
                                     superClassName,
                                     sourceFileName,
                                     modifiersStoJ(cSym.flags) | cst.ACC_SUPER,
                                     interfaceNames);
        classStack.addFirst(cGen);
        updateClassContext();
    }

    protected void leaveClass(Symbol cSym) {
        if (Modifiers.Helper.isModClass(cSym.flags))
            dumpModuleMainClass(currClass);

        addScalaAttr(currClass);
        JavaClass cls = currClass.getJavaClass();
        try {
            cls.dump(javaFileName(cls.getClassName()));
        } catch (java.io.IOException e) {
            throw global.fail(e.getMessage());
        }
        classStack.removeFirst();
        updateClassContext();
    }

    protected void updateClassContext() {
        if (classStack.isEmpty()) {
            currClass = null;
            currClassName = null;
            currPool = null;
        } else {
            ClassGen cGen = (ClassGen)classStack.getFirst();
            currClass = cGen;
            currClassName = currClass.getClassName();
            currPool = currClass.getConstantPool();
        }
    }

    protected void enterMethod(Tree.DefDef dDef) {
        Symbol dSym = dDef.symbol();

        global.log("entering method " + Debug.toString(dSym)
                   + " (type: " + Debug.toString(dSym.info()) + ")");

        Map locals;
        if (currLocals == null)
            locals = new HashMap();
        else
            locals = new HashMap(currLocals);

        Tree.ValDef[] args = dDef.vparams[0];
        int argsNum = args.length;

        Type[] argTypes = new Type[argsNum];
        String[] argNames = new String[argsNum];
        for (int i = 0, pos = 1; i < argsNum; ++i) {
            argTypes[i] = typeStoJ(args[i].symbol().info());
            argNames[i] = args[i].name.toString();
            locals.put(args[i].symbol(), new Integer(pos));
            pos += argTypes[i].getSize();
        }

        MethodGen mGen = new MethodGen(modifiersStoJ(dDef.mods),
                                       retTypeStoJ(dSym.info()),
                                       argTypes,
                                       argNames,
                                       dDef.name.toString(),
                                       currClassName,
                                       new InstructionList(),
                                       currPool);

        currMethod = mGen;
        currLocals = locals;
        currIL = currMethod.getInstructionList();
    }

    protected void leaveMethod() {
        global.log(" leaving method");

        currMethod.setMaxStack();
        currMethod.removeNOPs();
        currClass.addMethod(currMethod.getMethod());

        currMethod = null;
        currLocals = null;
        currIL = null;
    }

    protected int modifiersStoJ(int flags) {
        int jFlags = 0;

        if (Modifiers.Helper.isPrivate(flags))
            jFlags |= cst.ACC_PRIVATE;
        else if (Modifiers.Helper.isProtected(flags))
            jFlags |= cst.ACC_PROTECTED;
        else
            jFlags |= cst.ACC_PUBLIC;

        if (Modifiers.Helper.isAbstract(flags))
            jFlags |= cst.ACC_ABSTRACT;
        if (Modifiers.Helper.isInterface(flags))
            jFlags |= cst.ACC_INTERFACE;

        if (Modifiers.Helper.isFinal(flags)
            && !(Modifiers.Helper.isAbstract(flags) || Modifiers.Helper.isInterface(flags)))
            jFlags |= cst.ACC_FINAL;

        return jFlags;
    }

    protected boolean isUnboxedType(scalac.symtab.Type tp) {
        switch (tp) {
        case UnboxedType(_):
        case UnboxedArrayType(_): return true;
        default:                  return false;
        }
    }

    protected HashMap typeMap/*<Symbol,Type>*/ = new HashMap();
    protected void initTypeMap() {
        typeMap.put(defs.ANY_CLASS,    Type.OBJECT);
        typeMap.put(defs.ANYREF_CLASS, Type.OBJECT);
    }
    protected Type typeStoJ(scalac.symtab.Type tp) {
        switch (tp) {
        case UnboxedType(TypeTags.BYTE):
            return Type.BYTE;
        case UnboxedType(TypeTags.CHAR):
            return Type.CHAR;
        case UnboxedType(TypeTags.SHORT):
            return Type.SHORT;
        case UnboxedType(TypeTags.INT):
            return Type.INT;
        case UnboxedType(TypeTags.LONG):
            return Type.LONG;
        case UnboxedType(TypeTags.FLOAT):
            return Type.FLOAT;
        case UnboxedType(TypeTags.DOUBLE):
            return Type.DOUBLE;
        case UnboxedType(TypeTags.BOOLEAN):
            return Type.BOOLEAN;
        case UnboxedType(TypeTags.UNIT):
            return Type.VOID;
        case UnboxedType(TypeTags.STRING):
            return Type.STRING;
        case UnboxedArrayType(scalac.symtab.Type elementType):
            return new ArrayType(typeStoJ(elementType), 1);
        default: {
            Symbol sym = tp.symbol();
            if (sym == Symbol.NONE)
                throw global.fail("invalid type ", tp);
            else if (typeMap.containsKey(sym))
                return (Type)typeMap.get(sym);
            else {
                Type jTp = new ObjectType(javaName(sym));
                typeMap.put(sym, jTp);
                return jTp;
            }
        }
        }
    }

    protected Type[] argTypesStoJ(scalac.symtab.Type tp) {
        switch (tp) {
        case MethodType(Symbol[] vparams, _):
            Type[] argTypes = new Type[vparams.length];
            for (int i = 0; i < vparams.length; ++i)
                argTypes[i] = typeStoJ(vparams[i].info());
            return argTypes;
        default:
            throw global.fail("invalid method type", tp);
        }
    }

    protected Type retTypeStoJ(scalac.symtab.Type tp) {
        switch (tp) {
        case MethodType(_, _):
            return typeStoJ(tp.resultType());
        default:
            throw global.fail("invalid method type", tp);
        }
    }

    protected String javaName(Symbol sym) {
        if (sym == defs.ANY_CLASS || sym == defs.ANYREF_CLASS)
            return JAVA_LANG_OBJECT;
        else {
            StringBuffer buf = new StringBuffer(sym.name.toString());
            if ((sym.isModule() || sym.isModuleClass()) && !sym.isJava())
                buf.append('$');
            for (sym = sym.owner();
                 !(sym.isAnonymousClass() || sym.isPackage());
                 sym = sym.owner()) {
                buf.insert(0, '$');
                buf.insert(0, sym.name);
            }
            if (!sym.isAnonymousClass()) {
                buf.insert(0, '.');
                buf.insert(0, sym.fullName());
            }
            return buf.toString();
        }
    }

    protected String javaFileName(String className) {
        StringTokenizer tokens = new StringTokenizer(className, ".");
        File file = new File(global.outpath);
        while (tokens.hasMoreElements())
            file = new File(file, tokens.nextToken());

        return file.getPath() + ".class";
    }

    // Generic instructions
    static class Generic_RETURN implements CompoundInstruction {
        private ReturnInstruction inst;

        public Generic_RETURN(Type type) {
            switch (type.getType()) {
            case cst.T_VOID:    inst = ic.RETURN; break;
            case cst.T_BOOLEAN:
            case cst.T_BYTE:
            case cst.T_CHAR:
            case cst.T_SHORT:
            case cst.T_INT:     inst = ic.IRETURN; break;
            case cst.T_LONG:    inst = ic.LRETURN; break;
            case cst.T_FLOAT:   inst = ic.FRETURN; break;
            case cst.T_DOUBLE:  inst = ic.DRETURN; break;
            case cst.T_ARRAY:
            case cst.T_OBJECT:  inst = ic.ARETURN; break;
            default: throw Debug.abort("unexpected type " + type.getType());
            }
        }

        public InstructionList getInstructionList() {
            return new InstructionList(inst);
        }
    }

    static class Generic_LOAD implements CompoundInstruction {
        private LoadInstruction inst;

        public Generic_LOAD(int pos, Type type) {
            switch (type.getType()) {
            case cst.T_BOOLEAN:
            case cst.T_BYTE:
            case cst.T_CHAR:
            case cst.T_SHORT:
            case cst.T_INT:    inst = new ILOAD(pos); break;
            case cst.T_LONG:   inst = new LLOAD(pos); break;
            case cst.T_FLOAT:  inst = new FLOAD(pos); break;
            case cst.T_DOUBLE: inst = new DLOAD(pos); break;
            case cst.T_ARRAY:
            case cst.T_OBJECT: inst = new ALOAD(pos); break;
            default: throw Debug.abort("unexpected type");
            }
        }

        public InstructionList getInstructionList() {
            return new InstructionList(inst);
        }
    }

    static class Generic_STORE implements CompoundInstruction {
        private StoreInstruction inst;

        public Generic_STORE(int pos, Type type) {
            switch (type.getType()) {
            case cst.T_BOOLEAN:
            case cst.T_BYTE:
            case cst.T_CHAR:
            case cst.T_SHORT:
            case cst.T_INT:    inst = new ISTORE(pos); break;
            case cst.T_LONG:   inst = new LSTORE(pos); break;
            case cst.T_FLOAT:  inst = new FSTORE(pos); break;
            case cst.T_DOUBLE: inst = new DSTORE(pos); break;
            case cst.T_ARRAY:
            case cst.T_OBJECT: inst = new ASTORE(pos); break;
            default: throw Debug.abort("unexpected type");
            }
        }

        public InstructionList getInstructionList() {
            return new InstructionList(inst);
        }
    }

    static class Generic_ALOAD implements CompoundInstruction {
        private ArrayInstruction inst;

        public Generic_ALOAD(Type type) {
            switch (type.getType()) {
            case cst.T_BOOLEAN:
            case cst.T_BYTE:    inst = ic.BALOAD; break;
            case cst.T_CHAR:    inst = ic.CALOAD; break;
            case cst.T_SHORT:   inst = ic.SALOAD; break;
            case cst.T_INT:     inst = ic.IALOAD; break;
            case cst.T_LONG:    inst = ic.LALOAD; break;
            case cst.T_FLOAT:   inst = ic.FALOAD; break;
            case cst.T_DOUBLE:  inst = ic.DALOAD; break;
            case cst.T_ARRAY:
            case cst.T_OBJECT:  inst = ic.AALOAD; break;
            default: throw Debug.abort("unexpected type");
            }
        }

        public InstructionList getInstructionList() {
            return new InstructionList(inst);
        }
    }

    static class Generic_ASTORE implements CompoundInstruction {
        private ArrayInstruction inst;

        public Generic_ASTORE(Type type) {
            switch (type.getType()) {
            case cst.T_BOOLEAN:
            case cst.T_BYTE:    inst = ic.BASTORE; break;
            case cst.T_CHAR:    inst = ic.CASTORE; break;
            case cst.T_SHORT:   inst = ic.SASTORE; break;
            case cst.T_INT:     inst = ic.IASTORE; break;
            case cst.T_LONG:    inst = ic.LASTORE; break;
            case cst.T_FLOAT:   inst = ic.FASTORE; break;
            case cst.T_DOUBLE:  inst = ic.DASTORE; break;
            case cst.T_ARRAY:
            case cst.T_OBJECT:  inst = ic.AASTORE; break;
            default: throw Debug.abort("unexpected type");
            }
        }

        public InstructionList getInstructionList() {
            return new InstructionList(inst);
        }
    }
}
