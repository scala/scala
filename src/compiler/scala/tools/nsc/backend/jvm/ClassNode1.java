package scala.tools.nsc.backend.jvm;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.MethodNode;

public class ClassNode1 extends ClassNode {
    public ClassNode1() {
        this(Opcodes.ASM6);
    }

    public ClassNode1(int api) {
        super(api);
    }

    @Override
    public MethodVisitor visitMethod(int access, String name, String descriptor, String signature, String[] exceptions) {
        MethodNode method = new MethodNode1(access, name, descriptor, signature, exceptions);
        methods.add(method);
        return method;
    }
}
