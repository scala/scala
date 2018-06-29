/* NSC -- new Scala compiler
 * Copyright 2018 LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc.backend.jvm;

import scala.tools.asm.Label;
import scala.tools.asm.tree.ClassNode;
import scala.tools.asm.tree.LabelNode;

/**
 * A subclass of {@link LabelNode} to add user-definable flags.
 */
public class LabelNode1 extends LabelNode {
    public LabelNode1() {
    }

    public LabelNode1(Label label) {
        super(label);
    }

    public int flags;
}
