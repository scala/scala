package scala.tools.nsc.backend.jvm;

import org.objectweb.asm.Label;
import org.objectweb.asm.tree.LabelNode;

public class LabelNode1 extends LabelNode {
    public LabelNode1() {
    }

    public LabelNode1(Label label) {
        super(label);
    }

    public int flags;
}
