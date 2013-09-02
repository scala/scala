/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc.backend.bcode;

import static scala.tools.asm.Opcodes.ATHROW;
import static scala.tools.asm.Opcodes.GOTO;
import static scala.tools.asm.Opcodes.IRETURN;
import static scala.tools.asm.Opcodes.RETURN;

import java.util.*;

import scala.tools.asm.tree.AbstractInsnNode;
import scala.tools.asm.tree.InsnList;
import scala.tools.asm.tree.JumpInsnNode;
import scala.tools.asm.tree.LabelNode;
import scala.tools.asm.tree.MethodNode;

/**
 *  Replaces:
 *
 *    (1) a jump to a GOTO label instruction with a jump to label, and
 *    (2) a GOTO to one of:
 *          - a method-return instruction (whether IRETURN, ..., RETURN), or
 *          - an ATHROW instruction
 *        with a clone of that (method-return or ATHROW) instruction.
 *
 *  Even if covered by an exception handler, a "non-self-loop jump-only block" can always be removed.
 *
 *  For example, the following chain of "jump-only" blocks:
 *
 *          JUMP b1;
 *      b1: JUMP b2;
 *      b2: JUMP z
 *
 *  is reduced to just:
 *
 *      b1:
 *      b2: JUMP z
 *
 *  In particular, `JumpChainsCollapser` handles lassos:
 *
 *          JUMP e1;
 *      e1: JUMP e2;
 *      e2: JUMP e1;
 *
 *  is collapsed by making the start of the chain target directly the "final destination".
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *
 */
public class JumpChainsCollapser {

    /** after transform() has run, this field records whether
     *  at least one pass of this transformer modified something. */
    private boolean changed = false;

    public boolean changed() { return changed; }

    public void transform(final MethodNode mn) {
        changed = false;
        InsnList insns = mn.instructions;
        Iterator<AbstractInsnNode> i = insns.iterator();
        while (i.hasNext()) {
            AbstractInsnNode insn = i.next();
            if (insn instanceof JumpInsnNode) {
                JumpInsnNode source = (JumpInsnNode) insn;
                LabelNode finalDest = finalDestLabel(source);
                if (source.label != finalDest) {
                    source.label = finalDest;
                    changed = true;
                }
                if (source.getOpcode() == GOTO) {
                    AbstractInsnNode target = Util.insnLabelledBy(finalDest);
                    int op = target.getOpcode();
                    if ((op >= IRETURN && op <= RETURN) || op == ATHROW) {
                        insns.set(source, target.clone(null));
                        changed = true;
                    }
                }
            }
        }
    }

    private LabelNode finalDestLabel(final JumpInsnNode source) {
        assert source != null;

        HashSet<LabelNode> seenLabels = new HashSet<LabelNode>();
        seenLabels.add(source.label);

        LabelNode label = source.label;

        do {
            AbstractInsnNode dest = Util.insnLabelledBy(label);
            if (dest.getOpcode() != GOTO) {
                return label;
            }
            JumpInsnNode detour = (JumpInsnNode) dest;
            if (seenLabels.contains(detour.label)) {
                // we've found a loop (or lasso) from source to the instruction denoted by detour.label
                return detour.label;
            }
            seenLabels.add(detour.label);
            label = detour.label;
        } while (true);

    }
}

