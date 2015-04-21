import java.util.*;
import scala.tools.asm.*;

// generated with
//    git clone alewando/scala_groovy_interop
//    SCALA_HOME=... GROOVY_HOME=... ant
//    cd /code/scala2
//    java  -classpath build/asm/classes:/Users/jason/code/scala_groovy_interop/classes:/code/scala2/build/pack/lib/scala-library.jar:/usr/local/Cellar/groovy/2.4.1/libexec/embeddable/groovy-all-2.4.1.jar scala.tools.asm.util.ASMifier 'GroovyInterface$1'
//    java  -classpath build/asm/classes:/Users/jason/code/scala_groovy_interop/classes:/code/scala2/build/pack/lib/scala-library.jar:/usr/local/Cellar/groovy/2.4.1/libexec/embeddable/groovy-all-2.4.1.jar scala.tools.asm.util.ASMifier 'GroovyInterface$1'
public class GroovyInterfaceDump implements Opcodes {

    public static byte[] dump () throws Exception {

        ClassWriter cw = new ClassWriter(0);
        FieldVisitor fv;
        MethodVisitor mv;
        AnnotationVisitor av0;

        cw.visit(V1_5, ACC_PUBLIC + ACC_ABSTRACT + ACC_INTERFACE, "GroovyInterface", null, "java/lang/Object", null);

        cw.visitInnerClass("GroovyInterface$1", "GroovyInterface", "1", ACC_SYNTHETIC);

        cw.visitInnerClass("GroovyInterface$__clinit__closure1", null, null, 0);

        {
            fv = cw.visitField(ACC_PUBLIC + ACC_FINAL + ACC_STATIC, "closure", "Ljava/lang/Object;", null, null);
            fv.visitEnd();
        }
        {
            mv = cw.visitMethod(ACC_STATIC, "<clinit>", "()V", null, null);
            mv.visitCode();
            mv.visitTypeInsn(NEW, "GroovyInterface$__clinit__closure1");
            mv.visitInsn(DUP);
            mv.visitFieldInsn(GETSTATIC, "GroovyInterface$1", "$class$GroovyInterface", "Ljava/lang/Class;");
            mv.visitFieldInsn(GETSTATIC, "GroovyInterface$1", "$class$GroovyInterface", "Ljava/lang/Class;");
            mv.visitMethodInsn(INVOKESPECIAL, "GroovyInterface$__clinit__closure1", "<init>", "(Ljava/lang/Object;Ljava/lang/Object;)V", false);
            mv.visitVarInsn(ASTORE, 0);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(PUTSTATIC, "GroovyInterface", "closure", "Ljava/lang/Object;");
            mv.visitVarInsn(ALOAD, 0);
            mv.visitInsn(POP);
            mv.visitInsn(RETURN);
            mv.visitMaxs(4, 1);
            mv.visitEnd();
        }
        cw.visitEnd();

        return cw.toByteArray();
    }
}

