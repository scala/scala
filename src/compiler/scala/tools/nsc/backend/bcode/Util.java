/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc.backend.bcode;

import scala.tools.asm.ClassWriter;
import scala.tools.asm.MethodWriter;

import scala.tools.asm.tree.AbstractInsnNode;
import scala.tools.asm.tree.LabelNode;
import scala.tools.asm.tree.MethodNode;

/**
 *  Utilities.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
public class Util {

    // ------------------------------------------------------------------------
    // instruction lookup
    // ------------------------------------------------------------------------

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


}
