/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.asm.optimiz;

import java.util.HashSet;
import java.util.Iterator;

import scala.tools.asm.Opcodes;
import scala.tools.asm.tree.*;

/**
 *  Removes redundant exception-entries,
 *  ie those protecting a range whose only executable instructions are nop.
 *  Any LineNumberNode or LabelNode in the range can be left in place,
 *  will be removed by follow-up `LabelsCleanup`
 *
 *  TODO Actually, can the EH for a finally-clause be removed when protecting an empty block? May be not.
 *
 *  TODO Improving the Precision and Correctness of Exception Analysis in Soot,
 *  TODO http://www.sable.mcgill.ca/publications/techreports/#report2003-3
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
public class DanglingExcHandlers {

    /** after transform() has run, this field records whether
     *  at least one pass of this transformer modified something. */
    public boolean changed = false;

    public void transform(final MethodNode mn) {

        changed = false;

        if(mn.tryCatchBlocks == null) { return; }

        Iterator<TryCatchBlockNode> tryIter = mn.tryCatchBlocks.iterator();
        while (tryIter.hasNext()) {
            TryCatchBlockNode tcb = tryIter.next();

            assert mn.instructions.contains(tcb.start);
            assert mn.instructions.contains(tcb.end);

            if(containsJustNopsOrGotos(tcb.start, tcb.end)) {
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
        while(current != end) {
          boolean skip   = (current.getOpcode() == -1);
          boolean isNOP  = (current.getOpcode() == Opcodes.NOP);
          boolean isGoto = (current.getOpcode() == Opcodes.GOTO);
          if(!skip && !isNOP && !isGoto) {
              return false;
          }
          current = current.getNext();
        }

        return true;
    }

}
