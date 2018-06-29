package scala.tools.nsc.backend.jvm;

import org.objectweb.asm.Label;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.tree.LabelNode;
import org.objectweb.asm.tree.MethodNode;

public class MethodNode1 extends MethodNode {
    public MethodNode1(int api, int access, String name, String descriptor, String signature, String[] exceptions) {
        super(api, access, name, descriptor, signature, exceptions);
    }

    public MethodNode1(int access, String name, String descriptor, String signature, String[] exceptions) {
        this(Opcodes.ASM6, access, name, descriptor, signature, exceptions);
    }

    public MethodNode1(int api) {
        super(api);
    }

    public MethodNode1() {
        this(Opcodes.ASM6);
//        instructions = new InsnList() {
//            @Override
//            public void add(AbstractInsnNode insnNode) {
//                // TODO assert(insnNode.index == -1)
//                super.add(insnNode);
//            }
//        };
    }

    @Override
    protected LabelNode getLabelNode(Label label) {
        if (!(label.info instanceof LabelNode)) {
            label.info = new LabelNode1(label);
        }
        return (LabelNode) label.info;
    }
}
