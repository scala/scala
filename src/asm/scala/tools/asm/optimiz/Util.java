/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.asm.optimiz;

import scala.tools.asm.Opcodes;
import scala.tools.asm.Type;
import scala.tools.asm.Label;
import scala.tools.asm.ClassWriter;
import scala.tools.asm.MethodWriter;

import scala.tools.asm.tree.AbstractInsnNode;
import scala.tools.asm.tree.InsnNode;
import scala.tools.asm.tree.LdcInsnNode;
import scala.tools.asm.tree.LabelNode;
import scala.tools.asm.tree.MethodInsnNode;
import scala.tools.asm.tree.ClassNode;
import scala.tools.asm.tree.MethodNode;
import scala.tools.asm.tree.FieldNode;
import scala.tools.asm.tree.VarInsnNode;
import scala.tools.asm.tree.InsnList;
import scala.tools.asm.tree.LocalVariableNode;
import scala.tools.asm.tree.TryCatchBlockNode;
import scala.tools.asm.tree.JumpInsnNode;

import scala.tools.asm.util.Textifier;

import scala.tools.asm.tree.analysis.Analyzer;
import scala.tools.asm.tree.analysis.BasicInterpreter;
import scala.tools.asm.tree.analysis.BasicValue;
import scala.tools.asm.tree.analysis.AnalyzerException;

import java.util.*;

/**
 *  Utilities.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
public class Util {

    // ------------------------------------------------------------------------
    // utilities to classify instructions
    // ------------------------------------------------------------------------

    public static boolean isLOAD(final AbstractInsnNode insn) {
        final int opc = insn.getOpcode();
        return (opc >= Opcodes.ILOAD  && opc <= Opcodes.ALOAD);
    }

    public static boolean isRETURN(final AbstractInsnNode insn) {
        final int opc = insn.getOpcode();
        return (opc >= Opcodes.IRETURN && opc <= Opcodes.RETURN);
    }

    public static boolean isPrimitiveConstant(final AbstractInsnNode insn) {
        final int opc = insn.getOpcode();
        boolean result = (opc >= Opcodes.ACONST_NULL && opc <= Opcodes.SIPUSH);
        result |= (opc == Opcodes.LDC && !isStringConstant(insn) && !isTypeConstant(insn));
        return result;
    }

    public static boolean isStringConstant(final AbstractInsnNode insn) {
        if(insn.getOpcode() != Opcodes.LDC) {
             return false;
        }
        final LdcInsnNode ldc = (LdcInsnNode)insn;
        return (ldc.cst instanceof String);
    }

    public static boolean isTypeConstant(final AbstractInsnNode insn) {
        if(insn.getOpcode() != Opcodes.LDC) {
             return false;
        }
        final LdcInsnNode ldc = (LdcInsnNode)insn;
        return (ldc.cst instanceof Type);
    }

    public static boolean isSTORE(final AbstractInsnNode insn) {
        final int opc = insn.getOpcode();
        return (opc >= Opcodes.ISTORE && opc <= Opcodes.ASTORE);
    }

    public static boolean isDROP(final AbstractInsnNode insn) {
        final int opc = insn.getOpcode();
        return (opc == Opcodes.POP || opc == Opcodes.POP2);
    }

    public static boolean isExecutable(final AbstractInsnNode insn) {
        final int t = insn.getType();
        final boolean nonExec = (t == AbstractInsnNode.FRAME || t == AbstractInsnNode.LABEL || t == AbstractInsnNode.LINE);
        return !nonExec;
    }

    public static boolean isLabel(final AbstractInsnNode insn) {
        return (insn.getType() == AbstractInsnNode.LABEL);
    }

    public static InsnNode getDrop(final int size) {
        final int opc  = (size == 1) ? Opcodes.POP : Opcodes.POP2;
        return new InsnNode(opc);
    }

    /**
     *  Does the argument push a value to the stack, and pops nothing, without any side-effects?
     *  Ie the argument pushes a local-var, or a non-class constant.
     *
     *  TODO how about NEW for JDK classes without side-effecting static constructor?
     *
     *  TODO also related (although it doesn't push anything),
     *      <init> for JDK classes without side-effecting static constructor AND
     *             the instance constructor in question is known to be side-effects free.
     */
    public static boolean hasPushEffectOnly(final AbstractInsnNode producer) {
        if(Util.isLOAD(producer)) return true;
        // we leave out LDC <type> on purpose.
        if(Util.isPrimitiveConstant(producer) || Util.isStringConstant(producer)) return true;
        // taking DUP to be push-effect-only leads to trouble.
        return false;
    }

    /**
     *  Is the argument a conditional jump?
     */
    public static boolean isCondJump(final AbstractInsnNode insn) {
        switch (insn.getOpcode()) {
            case Opcodes.IFEQ:
            case Opcodes.IFNE:
            case Opcodes.IFLT:
            case Opcodes.IFGE:
            case Opcodes.IFGT:
            case Opcodes.IFLE:
            case Opcodes.IF_ICMPEQ:
            case Opcodes.IF_ICMPNE:
            case Opcodes.IF_ICMPLT:
            case Opcodes.IF_ICMPGE:
            case Opcodes.IF_ICMPGT:
            case Opcodes.IF_ICMPLE:
            case Opcodes.IF_ACMPEQ:
            case Opcodes.IF_ACMPNE:
            case Opcodes.IFNULL:
            case Opcodes.IFNONNULL:
                return true;
            default:
                return false;
        }
    }

    /**
     *  Is the argument an unconditional jump?
     */
    public static boolean isUncondJump(final AbstractInsnNode insn) {
        if(insn.getType() != AbstractInsnNode.JUMP_INSN) return false;
        switch (insn.getOpcode()) {
            case Opcodes.GOTO:
            case Opcodes.LOOKUPSWITCH:
            case Opcodes.TABLESWITCH:
            case Opcodes.JSR:
                return true;
            default:
                return false;
        }
    }

    public static boolean isGOTO(final AbstractInsnNode insn) {
        return (insn.getOpcode() == Opcodes.GOTO);
    }

    public static boolean isJSR(final AbstractInsnNode insn) {
        return (insn.getOpcode() == Opcodes.JSR);
    }

    /**
     *  Reports whether two LabelNodes denote in fact the same jump destination.
     */
    public static boolean denoteSameLocal(final AbstractInsnNode x, final AbstractInsnNode y) {
        assert x != null;
        assert y != null;

        if(x.getType() == AbstractInsnNode.VAR_INSN && y.getType() == AbstractInsnNode.VAR_INSN) {
            VarInsnNode vx = (VarInsnNode)x;
            VarInsnNode vy = (VarInsnNode)y;
            return vx.var == vy.var;
        }
        return false;
    }

    // ------------------------------------------------------------------------
    // instruction lookup
    // ------------------------------------------------------------------------

    /**
     *  Returns the first executable instruction (if any) occuring IN THE PROGRAM TEXT after the argument, null otherwise.
     */
    public static AbstractInsnNode execInsnAfter(final AbstractInsnNode insn) {
        AbstractInsnNode current = insn;
        while(true) {
            current = current.getNext();
            if(current == null || isExecutable(current)) return current;
        }
    }

    /**
     *  Returns the first
     *    (a) executable instruction (if any); or
     *    (b) LabelNode
     *  occuring IN THE PROGRAM TEXT after the argument, null otherwise.
     */
    public static AbstractInsnNode execInsnOrLabelAfter(final AbstractInsnNode insn) {
        AbstractInsnNode current = insn;
        while(true) {
            current = current.getNext();
            if(current == null || isExecutable(current) || isLabel(current)) return current;
        }
    }

    /**
     *  Returns the executable instruction (if any) labelled by the argument, null otherwise.
     */
    public static AbstractInsnNode insnLabelledBy(final LabelNode label) {
        assert label != null;
        AbstractInsnNode labelled = label;
        while (labelled != null && labelled.getOpcode() < 0) {
            labelled = labelled.getNext();
        }
        return labelled;
    }

    /**
     *  Reports whether two LabelNodes denote in fact the same jump destination.
     */
    public static boolean denoteSameTarget(final LabelNode x, final LabelNode y) {
        assert x != null;
        assert y != null;

        AbstractInsnNode xTarget = insnLabelledBy(x);
        if(xTarget == null) return false;
        AbstractInsnNode yTarget = insnLabelledBy(y);
        return xTarget == yTarget;
    }

    // ------------------------------------------------------------------------
    // boxing and unboxing
    // ------------------------------------------------------------------------

    public static boolean isPrivateMethod(final MethodNode m) {
        return (m.access & Opcodes.ACC_PRIVATE) != 0;
    }

    public static void makePrivateMethod(final MethodNode m) {
        m.access |=  Opcodes.ACC_PRIVATE;
        m.access &= ~Opcodes.ACC_PUBLIC;
        m.access &= ~Opcodes.ACC_PROTECTED;
    }

    public static boolean isPublicMethod(final MethodNode m) {
        return (m.access & Opcodes.ACC_PUBLIC) != 0;
    }

    public static void makePublicMethod(final MethodNode m) {
        m.access &= ~Opcodes.ACC_PRIVATE;
        m.access |=  Opcodes.ACC_PUBLIC;
        m.access &= ~Opcodes.ACC_PROTECTED;
    }

    public static boolean isProtectedMethod(final MethodNode m) {
        return (m.access & Opcodes.ACC_PROTECTED) != 0;
    }

    public static void makeProtectedMethod(final MethodNode m) {
        m.access &= ~Opcodes.ACC_PRIVATE;
        m.access &= ~Opcodes.ACC_PUBLIC;
        m.access |=  Opcodes.ACC_PROTECTED;
    }

    public static boolean isAbstractMethod(final MethodNode m) {
        return (m.access & Opcodes.ACC_ABSTRACT) != 0;
    }

    public static boolean isInstanceMethod(final MethodNode m) {
        return (m.access & Opcodes.ACC_STATIC) == 0;
    }

    public static boolean isStaticMethod(final MethodNode m) {
        return (m.access & Opcodes.ACC_STATIC) != 0;
    }

    public static void makeStaticMethod(final MethodNode m) {
        m.access |= Opcodes.ACC_STATIC;
    }

    public static boolean isSynchronizedMethod(final MethodNode m) {
        return (m.access & Opcodes.ACC_SYNCHRONIZED) != 0;
    }

    public static boolean isConstructor(final MethodNode m) {
        return (m.name.equals("<init>") || m.name.equals("<clinit>"));
    }

    public static boolean hasBytecodeInstructions(final MethodNode m) {
        return (m.access & (Opcodes.ACC_ABSTRACT | Opcodes.ACC_NATIVE)) == 0;
    }

    public static boolean isInstanceField(final FieldNode f) {
        return (f.access & Opcodes.ACC_STATIC) == 0;
    }

    /**
     *  INVOKEDYNAMIC and INVOKESTATIC don't qualify as `isInstanceCallsite()`
     * */
    public static boolean isInstanceCallsite(final MethodInsnNode callsite) {
        switch (callsite.getOpcode()) {
            case Opcodes.INVOKEVIRTUAL:
            case Opcodes.INVOKESPECIAL:
            case Opcodes.INVOKEINTERFACE:
                return true;
            default:
                return false;
        }
    }

    public static boolean isJavaBox(final AbstractInsnNode insn) {
        return (insn.getType() == AbstractInsnNode.METHOD_INSN) && isJavaBoxCall((MethodInsnNode) insn);
    }

    public static boolean isJavaBoxCall(final MethodInsnNode mi) {

        if(!"valueOf".equals(mi.name)) return false;

        Type[] argTs = Type.getArgumentTypes(mi.desc);
        if(argTs.length != 1) return false;

        switch (argTs[0].getSort()) {
            case Type.BOOLEAN: return "java/lang/Boolean".equals(mi.owner)   && "(Z)Ljava/lang/Boolean;".equals(mi.desc);
            case Type.BYTE:    return "java/lang/Byte".equals(mi.owner)      && "(B)Ljava/lang/Byte;".equals(mi.desc);
            case Type.CHAR:    return "java/lang/Character".equals(mi.owner) && "(C)Ljava/lang/Character;".equals(mi.desc);
            case Type.SHORT:   return "java/lang/Short".equals(mi.owner)     && "(S)Ljava/lang/Short;".equals(mi.desc);
            case Type.INT:     return "java/lang/Integer".equals(mi.owner)   && "(I)Ljava/lang/Integer;".equals(mi.desc);
            case Type.LONG:    return "java/lang/Long".equals(mi.owner)      && "(J)Ljava/lang/Long;".equals(mi.desc);
            case Type.FLOAT:   return "java/lang/Float".equals(mi.owner)     && "(F)Ljava/lang/Float;".equals(mi.desc);
            case Type.DOUBLE:  return "java/lang/Double".equals(mi.owner)    && "(D)Ljava/lang/Double;".equals(mi.desc);

            default: return false;
        }

    }

    public static boolean isJavaUnBox(final AbstractInsnNode insn) {
        return (insn.getType() == AbstractInsnNode.METHOD_INSN) && isJavaUnBoxCall((MethodInsnNode) insn);
    }

    public static boolean isJavaUnBoxCall(final MethodInsnNode mi) {

        Type rt = Type.getReturnType(mi.desc);

        switch (rt.getSort()) {
            case Type.BOOLEAN: return "java/lang/Boolean".equals(mi.owner)   && "()Z".equals(mi.desc)  &&  "booleanValue".equals(mi.name);
            case Type.BYTE:    return "java/lang/Byte".equals(mi.owner)      && "()B".equals(mi.desc)  &&  "byteValue".equals(mi.name);
            case Type.CHAR:    return "java/lang/Character".equals(mi.owner) && "()C".equals(mi.desc)  &&  "charValue".equals(mi.name);
            case Type.SHORT:   return "java/lang/Short".equals(mi.owner)     && "()S".equals(mi.desc)  &&  "shortValue".equals(mi.name);
            case Type.INT:     return "java/lang/Integer".equals(mi.owner)   && "()I".equals(mi.desc)  &&  "intValue".equals(mi.name);
            case Type.LONG:    return "java/lang/Long".equals(mi.owner)      && "()J".equals(mi.desc)  &&  "longValue".equals(mi.name);
            case Type.FLOAT:   return "java/lang/Float".equals(mi.owner)     && "()F".equals(mi.desc)  &&  "floatValue".equals(mi.name);
            case Type.DOUBLE:  return "java/lang/Double".equals(mi.owner)    && "()D".equals(mi.desc)  &&  "doubleValue".equals(mi.name);

            default: return false;
        }

    }

    // ------------------------------------------------------------------------
    // jumps, backedges
    // ------------------------------------------------------------------------

    /**
     *  @return all the backedges bracketed between instructions `start` and `end`
     * */
    public static Map<JumpInsnNode, LabelNode> backedges(final AbstractInsnNode start, final AbstractInsnNode end) {
        Map<JumpInsnNode, LabelNode> result = new HashMap<JumpInsnNode, LabelNode>();
        Set<LabelNode> seen = new HashSet<LabelNode>();
        AbstractInsnNode current = start;
        boolean stop = false;
        do {
            if(current.getType() == AbstractInsnNode.LABEL) {
                seen.add((LabelNode)current);
            } else if(current.getType() == AbstractInsnNode.JUMP_INSN) {
                JumpInsnNode j = (JumpInsnNode)current;
                if(seen.contains(j.label)) {
                    result.put(j, j.label);
                }
            }
            if(current == end) {
                stop = true;
            } else {
                current = current.getNext();
            }
        } while(!stop);
        return result;
    }

    // ------------------------------------------------------------------------
    // cloning
    // ------------------------------------------------------------------------

    public static Map<LabelNode, LabelNode> clonedLabels(final MethodNode mnode) {
        return clonedLabels(mnode.instructions);
    }

    public static Map<LabelNode, LabelNode> clonedLabels(final InsnList is) {
        ListIterator<AbstractInsnNode> iter   = is.iterator();
        Map<LabelNode, LabelNode>      result = new HashMap<LabelNode, LabelNode>();
        while(iter.hasNext()) {
            AbstractInsnNode insn = iter.next();
            if(insn.getType() == AbstractInsnNode.LABEL) {
                result.put((LabelNode)insn, fabricateLabelNode());
            }
        }
        return result;
    }

    public static LabelNode fabricateLabelNode() {
        Label     lab  = new Label();
        LabelNode labN = new LabelNode(lab);
        lab.info       = labN;
        return labN;
    }

    /**
     * Returns a list of cloned instructions for those in input, except that FrameNodes aren't cloned
     * (therefore, the returned list may well be shorter than input).
     * The correspondence between original and clone is mapped in insnMap,
     * a Map provided by the caller which is populated in this method.
     */
    public static InsnList clonedInsns(
            final InsnList                                input,
            final Map<LabelNode, LabelNode>               labelMap,
            final Map<AbstractInsnNode, AbstractInsnNode> insnMap
    ) {
        assert insnMap.isEmpty();
        ListIterator<AbstractInsnNode> iter = input.iterator();
        InsnList output = new InsnList();
        while(iter.hasNext()) {
            AbstractInsnNode nxt = iter.next();
            if(nxt.getType() != AbstractInsnNode.FRAME) {
                // don't clone any frames as they most likely won't make sense at the new usage point
                AbstractInsnNode cln = nxt.clone(labelMap);
                output.add(cln);
                insnMap.put(nxt, cln);
            }
        }
        return output;
    }

    /**
     * @param prefix at the new usage point, pasting cloned names of local vars as-is might lead to duplicates,
     *               in particular for `this`. Thus a prefix (e.g., the callee's name) may be provided by the invoker.
     */
    public static List<LocalVariableNode> clonedLocalVariableNodes(final MethodNode mnode, final Map<LabelNode, LabelNode> labelMap, final String prefix) {
        Iterator<LocalVariableNode> iter   = mnode.localVariables.iterator();
        List<LocalVariableNode>     output = new LinkedList<LocalVariableNode>();
        while(iter.hasNext()) {
            LocalVariableNode oldLVN = iter.next();
            LocalVariableNode newLVN = clonedLocalVariableNode(oldLVN, labelMap, prefix);
            output.add(newLVN);
        }
        return output;
    }

    public static LocalVariableNode clonedLocalVariableNode(final LocalVariableNode old, final Map<LabelNode, LabelNode> labelMap, final String prefix) {
        LocalVariableNode result = new LocalVariableNode(
            prefix + old.name,
            old.desc,
            old.signature,
            labelMap.get(old.start),
            labelMap.get(old.end),
            old.index
        );
        return result;
    }

    public static List<TryCatchBlockNode> clonedTryCatchBlockNodes(final MethodNode mnode, final Map<LabelNode, LabelNode> labelMap) {
        Iterator<TryCatchBlockNode> iter    = mnode.tryCatchBlocks.iterator();
        List<TryCatchBlockNode>     output  = new LinkedList<TryCatchBlockNode>();
        while(iter.hasNext()) {
            TryCatchBlockNode oldTCB = iter.next();
            TryCatchBlockNode newTCB = clonedTryCatchBlockNode(oldTCB, labelMap);
            output.add(newTCB);
        }
        return output;
    }

    public static TryCatchBlockNode clonedTryCatchBlockNode(final TryCatchBlockNode old, final Map<LabelNode, LabelNode> labelMap) {
        TryCatchBlockNode result = new TryCatchBlockNode(
            labelMap.get(old.start),
            labelMap.get(old.end),
            labelMap.get(old.handler),
            old.type
        );
        return result;
    }

    /**
     *
     * Warning: none of
     *   visibleAnnotations nor invisibleAnnotations nor attrs nor annotationDefault nor
     *   visibleParameterAnnotations nor invisibleParameterAnnotations
     * are copied over from `orig` to the copy. That's responsibility of the invoker.
     *
     * */
    public static ClonedMethod clonedMethodNode(final MethodNode orig) {

        MethodNode dup =
                new MethodNode(
                        Opcodes.ASM4,
                        orig.access, orig.name,
                        orig.desc,   orig.signature,
                        (orig.exceptions.toArray(new String[orig.exceptions.size()]))
                );

        Map<LabelNode, LabelNode> labelMap = Util.clonedLabels(orig);
        Map<AbstractInsnNode, AbstractInsnNode> insnMap = new java.util.HashMap<AbstractInsnNode, AbstractInsnNode>();
        dup.instructions = Util.clonedInsns(orig.instructions, labelMap, insnMap);

        dup.tryCatchBlocks = Util.clonedTryCatchBlockNodes(orig, labelMap);
        dup.localVariables = Util.clonedLocalVariableNodes(orig, labelMap, "");

        dup.maxLocals = orig.maxLocals;
        dup.maxStack  = orig.maxStack;

        return new ClonedMethod(dup, labelMap, insnMap);
    }

    // ------------------------------------------------------------------------
    // method descriptors and their formal params
    // ------------------------------------------------------------------------

    /**
     *  @return the number of arguments the callsite expects on the operand stack,
     *          ie for instance-level methods that's one more than the number of arguments in the method's descriptor.
     */
    public static int expectedArgs(final MethodInsnNode callsite) {
        int result = Type.getArgumentTypes(callsite.desc).length;
        switch (callsite.getOpcode()) {
            case Opcodes.INVOKEVIRTUAL:
            case Opcodes.INVOKESPECIAL:
            case Opcodes.INVOKEINTERFACE:
                result++;
                break;
            case Opcodes.INVOKEDYNAMIC:
                result++;
                break;
            case Opcodes.INVOKESTATIC:
                break;
        }
        return result;
    }

    /**
     *  @return the number of arguments the callsite expects on the operand stack,
     *          ie for instance-level methods that's one more than the number of arguments in the method's descriptor.
     */
    public static int expectedArgs(final MethodNode mnode) {
        int formals = Type.getArgumentTypes(mnode.desc).length;
        return (isInstanceMethod(mnode) ? 1 : 0) + formals;
    }

    // ------------------------------------------------------------------------
    // maxLocals and maxStack
    // ------------------------------------------------------------------------

    /**
     * In order to run Analyzer.analyze() on a method, its `maxLocals` should have been computed.
     */
    public static boolean isReadyForAnalyzer(final MethodNode mnode) {
      return mnode.maxLocals != 0 || mnode.maxStack != 0;
    }

    /**
     * In order to run Analyzer.analyze() on a method, its `maxLocals` should have been computed.
     */
    public static void computeMaxLocalsMaxStack(final MethodNode mnode) {
        ClassWriter cw  = new ClassWriter(ClassWriter.COMPUTE_MAXS);
        String[] excs   = mnode.exceptions.toArray(new String[0]);
        MethodWriter mw = (MethodWriter)cw.visitMethod(mnode.access, mnode.name, mnode.desc, mnode.signature, excs);
        mnode.accept(mw);
        mnode.maxLocals = mw.getMaxLocals();
        mnode.maxStack  = mw.getMaxStack();
    }

    // ------------------------------------------------------------------------
    // miscellaneous
    // ------------------------------------------------------------------------

    public static void basicInterpret(final ClassNode cnode) throws AnalyzerException {
        Analyzer ca = new Analyzer<BasicValue>(new BasicInterpreter());
        Iterator<MethodNode> iterMethod = cnode.methods.iterator();
        while(iterMethod.hasNext()) {
            MethodNode mnode = iterMethod.next();
            if(!isReadyForAnalyzer(mnode)) {
                computeMaxLocalsMaxStack(mnode);
            }
            ca.analyze(cnode.name, mnode);
        }
    }

    // ------------------------------------------------------------------------
    // Textification
    // ------------------------------------------------------------------------

    /**
     * Returns a human-readable representation of the cnode ClassNode.
     */
    public static String textify(final ClassNode cnode) {
      scala.tools.asm.util.TraceClassVisitor trace = new scala.tools.asm.util.TraceClassVisitor(new java.io.PrintWriter(new java.io.StringWriter()));
      cnode.accept(trace);
      java.io.StringWriter sw = new java.io.StringWriter();
      java.io.PrintWriter  pw = new java.io.PrintWriter(sw);
      trace.p.print(pw);
      return sw.toString();
    }

    /**
     * Returns a human-readable representation of the code in the mnode MethodNode.
     */
    public static String textify(final MethodNode mnode) {
      scala.tools.asm.util.TraceClassVisitor trace = new scala.tools.asm.util.TraceClassVisitor(new java.io.PrintWriter(new java.io.StringWriter()));
      mnode.accept(trace);
      java.io.StringWriter sw = new java.io.StringWriter();
      java.io.PrintWriter  pw = new java.io.PrintWriter(sw);
      trace.p.print(pw);
      return sw.toString();
    }

    /**
     * Returns a human-readable representation of the given instruction.
     */
    public static String textify(final AbstractInsnNode insn) {
        scala.tools.asm.util.TraceMethodVisitor trace = new scala.tools.asm.util.TraceMethodVisitor(new Textifier());
        insn.accept(trace);
        java.io.StringWriter sw = new java.io.StringWriter();
        java.io.PrintWriter  pw = new java.io.PrintWriter(sw);
        trace.p.print(pw);
        return sw.toString().trim();
    }

    /**
     * Returns a human-readable representation of the given instruction sequence.
     */
    public static String textify(final InsnList insns) {
        scala.tools.asm.util.TraceMethodVisitor trace = new scala.tools.asm.util.TraceMethodVisitor(new Textifier());

        ListIterator<AbstractInsnNode> iter = insns.iterator();
        while(iter.hasNext()) {
            AbstractInsnNode insn = iter.next();
            insn.accept(trace);
        }

        java.io.StringWriter sw = new java.io.StringWriter();
        java.io.PrintWriter  pw = new java.io.PrintWriter(sw);
        trace.p.print(pw);
        return sw.toString().trim();
    }

    /**
     * Returns a human-readable representation of the given instruction sequence.
     */
    public static String textify(final Iterable<AbstractInsnNode> insns) {
        scala.tools.asm.util.TraceMethodVisitor trace = new scala.tools.asm.util.TraceMethodVisitor(new Textifier());

        Iterator<AbstractInsnNode> iter = insns.iterator();
        while(iter.hasNext()) {
            AbstractInsnNode insn = iter.next();
            insn.accept(trace);
        }

        java.io.StringWriter sw = new java.io.StringWriter();
        java.io.PrintWriter  pw = new java.io.PrintWriter(sw);
        trace.p.print(pw);
        return sw.toString().trim();
    }

    public static class ClonedMethod {

        public final MethodNode mnode;
        public final Map<LabelNode, LabelNode> labelMap;
        public final Map<AbstractInsnNode, AbstractInsnNode> insnMap;

        public ClonedMethod(
                final MethodNode mnode,
                final Map<LabelNode, LabelNode> labelMap,
                final Map<AbstractInsnNode, AbstractInsnNode> insnMap) {
            this.mnode    = mnode;
            this.labelMap = labelMap;
            this.insnMap  = insnMap;
        }

    } // end of class ClonedMethod

}