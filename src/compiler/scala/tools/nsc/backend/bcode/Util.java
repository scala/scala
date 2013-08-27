/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc.backend.bcode;

import scala.tools.asm.tree.AbstractInsnNode;
import scala.tools.asm.tree.LabelNode;
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


}
