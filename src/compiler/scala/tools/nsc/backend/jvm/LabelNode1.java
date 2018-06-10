package scala.tools.nsc.backend.jvm;

import scala.tools.asm.Label;
import scala.tools.asm.tree.LabelNode;

public class LabelNode1 extends LabelNode {
    public LabelNode1() {
    }

    public LabelNode1(Label label) {
        super(label);
    }

    public int flags;
}
