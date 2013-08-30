/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc.backend.bcode;

import java.util.HashSet;
import java.util.Iterator;

import scala.tools.asm.Opcodes;
import scala.tools.asm.tree.*;

/**
 *  Removes redundant exception-entries,
 *  ie those protecting a range whose only executable instructions are nop.
 *  Any LineNumberNode or LabelNode in the range are left in place,
 *  will be removed by follow-up `LabelsCleanup`
 *
 *  Just the exception-entry is removed. The bytecode for the exception handler itself
 *  is left in place, given that it *could* be reachable via normal-control flow
 *  (although some JIT compilers will bailout compilation if that's the case).
 *  In case the exception handler code turns out to be unreachable,
 *  dead-code-elimination will take care of it.
 *
 *  The bytecode for a finally-clause that protects an empty block is never removed by this transform.
 *  In more detail, a finally-clause results in both an EH-version (reachable via exceptional control flow)
 *  and another non-EH version (reachable via normal-control-flow).
 *  None of those code blocks is removed by this transform,
 *  they will be removed (if found to be unreachable) by dead-code elimination.
 *
 *  See also Improving the Precision and Correctness of Exception Analysis in Soot,
 *           http://www.sable.mcgill.ca/publications/techreports/#report2003-3
 *
 *  Sidenote: on CLR, the "EH-version" of a finally-clause protecting an empty-block
 *  can't be removed, because exceptions are handled specially there.
 *  For example, code running within a finally block is never interrupted by Abort exceptions.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
public class DanglingExcHandlers {

    /** after transform() has run, this field records whether
     *  at least one pass of this transformer modified something. */
    private boolean changed = false;

    public boolean changed() { return changed; }

    public void transform(final MethodNode mn) {

        changed = false;

        if (mn.tryCatchBlocks == null) { return; }

        Iterator<TryCatchBlockNode> tryIter = mn.tryCatchBlocks.iterator();
        while (tryIter.hasNext()) {
            TryCatchBlockNode tcb = tryIter.next();

            assert mn.instructions.contains(tcb.start);
            assert mn.instructions.contains(tcb.end);

            if (containsJustNopsOrGotos(tcb.start, tcb.end)) {
                changed = true;
                tryIter.remove();
            }
        }

    }

    /**
     *  Any LineNumberNode or LabelNode or FrameNode will be skipped.
     */
    public boolean containsJustNopsOrGotos(final LabelNode start, final LabelNode end) {
        assert start != null;
        assert end   != null;

        AbstractInsnNode current = start;
        while (current != end) {
          boolean skip   = (current.getOpcode() == -1);
          boolean isNOP  = (current.getOpcode() == Opcodes.NOP);
          boolean isGoto = (current.getOpcode() == Opcodes.GOTO);
          if (!skip && !isNOP && !isGoto) {
              return false;
          }
          current = current.getNext();
        }

        return true;
    }

}

