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
        if (insn.getOpcode() != Opcodes.LDC) {
             return false;
        }
        final LdcInsnNode ldc = (LdcInsnNode)insn;
        return (ldc.cst instanceof String);
    }

    public static boolean isTypeConstant(final AbstractInsnNode insn) {
        if (insn.getOpcode() != Opcodes.LDC) {
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
        if (Util.isLOAD(producer)) return true;
        // we leave out LDC <type> on purpose.
        if (Util.isPrimitiveConstant(producer) || Util.isStringConstant(producer)) return true;
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
        if (insn.getType() != AbstractInsnNode.JUMP_INSN) return false;
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

        if (x.getType() == AbstractInsnNode.VAR_INSN && y.getType() == AbstractInsnNode.VAR_INSN) {
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
        while (true) {
            current = current.getNext();
            if (current == null || isExecutable(current)) return current;
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
        while (true) {
            current = current.getNext();
            if (current == null || isExecutable(current) || isLabel(current)) return current;
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
        if (xTarget == null) return false;
        AbstractInsnNode yTarget = insnLabelledBy(y);
        return xTarget == yTarget;
    }

    // ------------------------------------------------------------------------
    // queries and operations on MethodNodes
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
            if (current.getType() == AbstractInsnNode.LABEL) {
                seen.add((LabelNode)current);
            } else if (current.getType() == AbstractInsnNode.JUMP_INSN) {
                JumpInsnNode j = (JumpInsnNode)current;
                if (seen.contains(j.label)) {
                    result.put(j, j.label);
                }
            }
            if (current == end) {
                stop = true;
            } else {
                current = current.getNext();
            }
        } while (!stop);
        return result;
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
        while (iter.hasNext()) {
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
        while (iter.hasNext()) {
            AbstractInsnNode insn = iter.next();
            insn.accept(trace);
        }

        java.io.StringWriter sw = new java.io.StringWriter();
        java.io.PrintWriter  pw = new java.io.PrintWriter(sw);
        trace.p.print(pw);
        return sw.toString().trim();
    }

}
