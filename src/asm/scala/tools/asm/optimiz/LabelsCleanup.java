/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.asm.optimiz;

import java.util.HashSet;
import java.util.Iterator;

import scala.tools.asm.tree.*;

/**
 *  Removes those LabelNodes and LineNumberNodes that are redundant.
 *
 *  A LabelNode is trivially not in use if it's not referred from any of:
 *    - control-flow transfer instruction,
 *    - LineNumberNode
 *    - exception-handler entry.
 *
 *  A LineNumberNode is redundant iff it doesn't denote an executable instruction.
 *
 *  The cleanup simplifies further transforms,
 *  and can be applied profitably after removal of unreachable code.
 *
 *  Notice that not all LabelNodes that are left denote the start of a basic block.
 *  Additionally, a basic block need not start with a LabelNode.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
public class LabelsCleanup {

    public HashSet<LabelNode> isTarget    = new HashSet<LabelNode>();
    public HashSet<LabelNode> isExBracket = new HashSet<LabelNode>();
    public HashSet<LabelNode> isDebugInfo = new HashSet<LabelNode>();

    /** after transform() has run, this field records whether
     *  at least one pass of this transformer modified something. */
    public boolean changed = false;

    public void transform(final MethodNode mn) {

        changed = false;
        boolean keepGoing = false;

        do {
            classifyLabelNodes(mn);
            keepGoing = pointlessLineNumbers(mn) || pointlessLabels(mn) || pointlessLocalVarEntries(mn);
            changed = (changed || keepGoing);
        } while (keepGoing);

    }

    private boolean pointlessLocalVarEntries(final MethodNode mn) {
        boolean changed = false;
        if (mn.localVariables != null) {
            Iterator<LocalVariableNode> lvIter = mn.localVariables.iterator();
            while (lvIter.hasNext()) {
                LocalVariableNode lvn = lvIter.next();
                if (lacksExecutableInsns(lvn.start, lvn.end)) {
                    changed = true;
                    lvIter.remove();
                }
            }
        }

        return changed;
    }

    private boolean lacksExecutableInsns(final LabelNode start, final LabelNode end) {
        assert start != null;
        assert end   != null;

        AbstractInsnNode current = start;
        while (current != end) {
            if (current.getOpcode() > 0) {
                return false;
            }
            current = current.getNext();
        }

        return true;
    }

    private boolean pointlessLineNumbers(final MethodNode mn) {
        InsnList insns = mn.instructions;
        Iterator<AbstractInsnNode> insnIter = insns.iterator();

        boolean changed = false;
        while (insnIter.hasNext()) {
            AbstractInsnNode insnNode = insnIter.next();
            if (insnNode instanceof LineNumberNode) {
                if (isPointless((LineNumberNode)insnNode)) {
                    insnIter.remove();
                    changed = true;
                }
            }
        }

        return changed;
    }

    /** Remove those LabelNodes trivially not in use. */
    private boolean pointlessLabels(final MethodNode mn) {
        InsnList insns = mn.instructions;
        Iterator<AbstractInsnNode> insnIter = insns.iterator();

        boolean changed = false;
        while (insnIter.hasNext()) {
            AbstractInsnNode insnNode = insnIter.next();
            if (insnNode.getType() == AbstractInsnNode.LABEL && canRemove(insnNode)) {
                insnIter.remove();
                changed = true;
            }
        }

        return changed;
    }

    /**
     *  A LineNumberNode is pointless (and can be removed) iff it doesn't denote an executable instruction.
     *  Given a LineNumberNode, we search for the next non-LabelNode, non-LineNumberNode, non-StackFrame.
     *  If none is found before reaching the end of the instruction stream, the LineNumberNode in question is pointless.
     *  Similarly in case an intervening LineNumberNode shows up before the first executable instruction
     *  (due to the way we emit code, a LineNumberNode always precedes the instruction it refers to, and follows a LabelNode).
     */
    private boolean isPointless(final LineNumberNode nn) {
        AbstractInsnNode nxt = nn.start;
        do {
            nxt = nxt.getNext();
            if (nxt == null) return true;
            if (nxt != nn) {
                if (nxt.getType() == AbstractInsnNode.LINE) return true;
                if (nxt.getOpcode() >= 0) return false;
                assert(nxt.getType() == AbstractInsnNode.LABEL || nxt.getType() == AbstractInsnNode.FRAME);
                // skip Frame nodes and LabelNodes.
            }
        } while (true);
    }

    private void classifyLabelNodes(final MethodNode mn) {
        isTarget.clear();
        isExBracket.clear();
        isDebugInfo.clear();

        InsnList insns = mn.instructions;
        Iterator<AbstractInsnNode> insnIter = insns.iterator();

        // collect LabelNodes deemed to be in use
        while (insnIter.hasNext()) {
            AbstractInsnNode insnNode = insnIter.next();
            if (insnNode instanceof JumpInsnNode) {
                JumpInsnNode j = (JumpInsnNode) insnNode;
                isTarget.add(j.label);
            } else if (insnNode instanceof LineNumberNode) {
                LineNumberNode lnn = (LineNumberNode) insnNode;
                isDebugInfo.add(lnn.start);
            } else if (insnNode instanceof LookupSwitchInsnNode) {
                LookupSwitchInsnNode sw = (LookupSwitchInsnNode) insnNode;
                isTarget.add(sw.dflt);
                Iterator<LabelNode> branchIter = sw.labels.iterator();
                while (branchIter.hasNext()) {
                    isTarget.add(branchIter.next());
                }
            } else if (insnNode instanceof TableSwitchInsnNode) {
                TableSwitchInsnNode sw = (TableSwitchInsnNode) insnNode;
                isTarget.add(sw.dflt);
                Iterator<LabelNode> branchIter = sw.labels.iterator();
                while (branchIter.hasNext()) {
                    isTarget.add(branchIter.next());
                }
            }
        }
        if (mn.localVariables != null) {
            Iterator<LocalVariableNode> lvIter = mn.localVariables.iterator();
            while (lvIter.hasNext()) {
                LocalVariableNode lvn = lvIter.next();
                isDebugInfo.add(lvn.start);
                isDebugInfo.add(lvn.end);
            }
        }
        if (mn.tryCatchBlocks != null) {
            Iterator<TryCatchBlockNode> tryIter = mn.tryCatchBlocks.iterator();
            while (tryIter.hasNext()) {
                TryCatchBlockNode tryNode = tryIter.next();
                // targets
                isTarget.add(tryNode.start);
                isTarget.add(tryNode.handler);
                // brackets
                isExBracket.add(tryNode.start);
                isExBracket.add(tryNode.end);
            }
        }
    }

    /**
     *  If the argument is trivially not in use, it can be removed.
     */
    public boolean canRemove(final AbstractInsnNode insn) {
        return !isTarget.contains(insn) && !isExBracket.contains(insn) && !isDebugInfo.contains(insn);
    }

}

