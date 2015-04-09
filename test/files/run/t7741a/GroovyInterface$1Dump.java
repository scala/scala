import java.util.*;
import scala.tools.asm.*;

// generated with
//    git clone alewando/scala_groovy_interop
//    SCALA_HOME=... GROOVY_HOME=... ant
//    cd /code/scala2
//    java  -classpath build/asm/classes:/Users/jason/code/scala_groovy_interop/classes:/code/scala2/build/pack/lib/scala-library.jar:/usr/local/Cellar/groovy/2.4.1/libexec/embeddable/groovy-all-2.4.1.jar scala.tools.asm.util.ASMifier 'GroovyInterface$1'
//    java  -classpath build/asm/classes:/Users/jason/code/scala_groovy_interop/classes:/code/scala2/build/pack/lib/scala-library.jar:/usr/local/Cellar/groovy/2.4.1/libexec/embeddable/groovy-all-2.4.1.jar scala.tools.asm.util.ASMifier 'GroovyInterface$1'
public class GroovyInterface$1Dump implements Opcodes {

    public static byte[] dump () throws Exception {

        ClassWriter cw = new ClassWriter(0);
        FieldVisitor fv;
        MethodVisitor mv;
        AnnotationVisitor av0;

        cw.visit(V1_5, ACC_SUPER + ACC_SYNTHETIC, "GroovyInterface$1", null, "java/lang/Object", new String[] {});

        cw.visitInnerClass("GroovyInterface$1", "GroovyInterface", "1", ACC_SYNTHETIC);

        {
            fv = cw.visitField(ACC_STATIC + ACC_SYNTHETIC, "$class$GroovyInterface", "Ljava/lang/Class;", null, null);
            fv.visitEnd();
        }
        {
            fv = cw.visitField(ACC_PRIVATE + ACC_STATIC + ACC_SYNTHETIC, "$staticClassInfo", "Lorg/codehaus/groovy/reflection/ClassInfo;", null, null);
            fv.visitEnd();
        }
        {
            fv = cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_TRANSIENT + ACC_SYNTHETIC, "__$stMC", "Z", null, null);
            fv.visitEnd();
        }
        {
            fv = cw.visitField(ACC_PRIVATE + ACC_TRANSIENT + ACC_SYNTHETIC, "metaClass", "Lgroovy/lang/MetaClass;", null, null);
            fv.visitEnd();
        }
        {
            fv = cw.visitField(ACC_PRIVATE + ACC_STATIC + ACC_SYNTHETIC, "$callSiteArray", "Ljava/lang/ref/SoftReference;", null, null);
            fv.visitEnd();
        }
        {
            mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
            mv.visitMethodInsn(INVOKESTATIC, "GroovyInterface$1", "$getCallSiteArray", "()[Lorg/codehaus/groovy/runtime/callsite/CallSite;", false);
            mv.visitVarInsn(ASTORE, 1);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKEVIRTUAL, "GroovyInterface$1", "$getStaticMetaClass", "()Lgroovy/lang/MetaClass;", false);
            mv.visitVarInsn(ASTORE, 2);
            mv.visitVarInsn(ALOAD, 2);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitInsn(SWAP);
            mv.visitFieldInsn(PUTFIELD, "GroovyInterface$1", "metaClass", "Lgroovy/lang/MetaClass;");
            mv.visitVarInsn(ALOAD, 2);
            mv.visitInsn(POP);
            mv.visitInsn(RETURN);
            mv.visitMaxs(2, 3);
            mv.visitEnd();
        }
        {
            mv = cw.visitMethod(ACC_PROTECTED + ACC_SYNTHETIC, "$getStaticMetaClass", "()Lgroovy/lang/MetaClass;", null, null);
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Object", "getClass", "()Ljava/lang/Class;", false);
            mv.visitLdcInsn(Type.getType("LGroovyInterface$1;"));
            Label l0 = new Label();
            mv.visitJumpInsn(IF_ACMPEQ, l0);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESTATIC, "org/codehaus/groovy/runtime/ScriptBytecodeAdapter", "initMetaClass", "(Ljava/lang/Object;)Lgroovy/lang/MetaClass;", false);
            mv.visitInsn(ARETURN);
            mv.visitLabel(l0);
            mv.visitFieldInsn(GETSTATIC, "GroovyInterface$1", "$staticClassInfo", "Lorg/codehaus/groovy/reflection/ClassInfo;");
            mv.visitVarInsn(ASTORE, 1);
            mv.visitVarInsn(ALOAD, 1);
            Label l1 = new Label();
            mv.visitJumpInsn(IFNONNULL, l1);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Object", "getClass", "()Ljava/lang/Class;", false);
            mv.visitMethodInsn(INVOKESTATIC, "org/codehaus/groovy/reflection/ClassInfo", "getClassInfo", "(Ljava/lang/Class;)Lorg/codehaus/groovy/reflection/ClassInfo;", false);
            mv.visitInsn(DUP);
            mv.visitVarInsn(ASTORE, 1);
            mv.visitFieldInsn(PUTSTATIC, "GroovyInterface$1", "$staticClassInfo", "Lorg/codehaus/groovy/reflection/ClassInfo;");
            mv.visitLabel(l1);
            mv.visitVarInsn(ALOAD, 1);
            mv.visitMethodInsn(INVOKEVIRTUAL, "org/codehaus/groovy/reflection/ClassInfo", "getMetaClass", "()Lgroovy/lang/MetaClass;", false);
            mv.visitInsn(ARETURN);
            mv.visitMaxs(2, 2);
            mv.visitEnd();
        }
        {
            mv = cw.visitMethod(ACC_PUBLIC + ACC_SYNTHETIC, "getMetaClass", "()Lgroovy/lang/MetaClass;", null, null);
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(GETFIELD, "GroovyInterface$1", "metaClass", "Lgroovy/lang/MetaClass;");
            mv.visitInsn(DUP);
            Label l0 = new Label();
            mv.visitJumpInsn(IFNULL, l0);
            mv.visitInsn(ARETURN);
            mv.visitLabel(l0);
            mv.visitInsn(POP);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitInsn(DUP);
            mv.visitMethodInsn(INVOKEVIRTUAL, "GroovyInterface$1", "$getStaticMetaClass", "()Lgroovy/lang/MetaClass;", false);
            mv.visitFieldInsn(PUTFIELD, "GroovyInterface$1", "metaClass", "Lgroovy/lang/MetaClass;");
            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(GETFIELD, "GroovyInterface$1", "metaClass", "Lgroovy/lang/MetaClass;");
            mv.visitInsn(ARETURN);
            mv.visitMaxs(2, 1);
            mv.visitEnd();
        }
        {
            mv = cw.visitMethod(ACC_PUBLIC + ACC_SYNTHETIC, "setMetaClass", "(Lgroovy/lang/MetaClass;)V", null, null);
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitVarInsn(ALOAD, 1);
            mv.visitFieldInsn(PUTFIELD, "GroovyInterface$1", "metaClass", "Lgroovy/lang/MetaClass;");
            mv.visitInsn(RETURN);
            mv.visitMaxs(2, 2);
            mv.visitEnd();
        }
        {
            mv = cw.visitMethod(ACC_PUBLIC + ACC_SYNTHETIC, "invokeMethod", "(Ljava/lang/String;Ljava/lang/Object;)Ljava/lang/Object;", null, null);
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKEVIRTUAL, "GroovyInterface$1", "getMetaClass", "()Lgroovy/lang/MetaClass;", false);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitVarInsn(ALOAD, 1);
            mv.visitVarInsn(ALOAD, 2);
            mv.visitMethodInsn(INVOKEINTERFACE, "groovy/lang/MetaClass", "invokeMethod", "(Ljava/lang/Object;Ljava/lang/String;Ljava/lang/Object;)Ljava/lang/Object;", true);
            mv.visitInsn(ARETURN);
            mv.visitMaxs(4, 3);
            mv.visitEnd();
        }
        {
            mv = cw.visitMethod(ACC_PUBLIC + ACC_SYNTHETIC, "getProperty", "(Ljava/lang/String;)Ljava/lang/Object;", null, null);
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKEVIRTUAL, "GroovyInterface$1", "getMetaClass", "()Lgroovy/lang/MetaClass;", false);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitVarInsn(ALOAD, 1);
            mv.visitMethodInsn(INVOKEINTERFACE, "groovy/lang/MetaClass", "getProperty", "(Ljava/lang/Object;Ljava/lang/String;)Ljava/lang/Object;", true);
            mv.visitInsn(ARETURN);
            mv.visitMaxs(3, 2);
            mv.visitEnd();
        }
        {
            mv = cw.visitMethod(ACC_PUBLIC + ACC_SYNTHETIC, "setProperty", "(Ljava/lang/String;Ljava/lang/Object;)V", null, null);
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKEVIRTUAL, "GroovyInterface$1", "getMetaClass", "()Lgroovy/lang/MetaClass;", false);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitVarInsn(ALOAD, 1);
            mv.visitVarInsn(ALOAD, 2);
            mv.visitMethodInsn(INVOKEINTERFACE, "groovy/lang/MetaClass", "setProperty", "(Ljava/lang/Object;Ljava/lang/String;Ljava/lang/Object;)V", true);
            mv.visitInsn(RETURN);
            mv.visitMaxs(4, 3);
            mv.visitEnd();
        }
        {
            mv = cw.visitMethod(ACC_STATIC, "<clinit>", "()V", null, null);
            mv.visitCode();
            mv.visitLdcInsn(Type.getType("LGroovyInterface;"));
            mv.visitVarInsn(ASTORE, 0);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(PUTSTATIC, "GroovyInterface$1", "$class$GroovyInterface", "Ljava/lang/Class;");
            mv.visitVarInsn(ALOAD, 0);
            mv.visitInsn(POP);
            mv.visitInsn(RETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
        }
        {
            mv = cw.visitMethod(ACC_PRIVATE + ACC_STATIC + ACC_SYNTHETIC, "$createCallSiteArray", "()Lorg/codehaus/groovy/runtime/callsite/CallSiteArray;", null, null);
            mv.visitCode();
            mv.visitLdcInsn(new Integer(0));
            mv.visitTypeInsn(ANEWARRAY, "java/lang/String");
            mv.visitVarInsn(ASTORE, 0);
            mv.visitTypeInsn(NEW, "org/codehaus/groovy/runtime/callsite/CallSiteArray");
            mv.visitInsn(DUP);
            mv.visitLdcInsn(Type.getType("LGroovyInterface$1;"));
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESPECIAL, "org/codehaus/groovy/runtime/callsite/CallSiteArray", "<init>", "(Ljava/lang/Class;[Ljava/lang/String;)V", false);
            mv.visitInsn(ARETURN);
            mv.visitMaxs(4, 1);
            mv.visitEnd();
        }
        {
            mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC + ACC_SYNTHETIC, "$getCallSiteArray", "()[Lorg/codehaus/groovy/runtime/callsite/CallSite;", null, null);
            mv.visitCode();
            mv.visitFieldInsn(GETSTATIC, "GroovyInterface$1", "$callSiteArray", "Ljava/lang/ref/SoftReference;");
            Label l0 = new Label();
            mv.visitJumpInsn(IFNULL, l0);
            mv.visitFieldInsn(GETSTATIC, "GroovyInterface$1", "$callSiteArray", "Ljava/lang/ref/SoftReference;");
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/ref/SoftReference", "get", "()Ljava/lang/Object;", false);
            mv.visitTypeInsn(CHECKCAST, "org/codehaus/groovy/runtime/callsite/CallSiteArray");
            mv.visitInsn(DUP);
            mv.visitVarInsn(ASTORE, 0);
            Label l1 = new Label();
            mv.visitJumpInsn(IFNONNULL, l1);
            mv.visitLabel(l0);
            mv.visitMethodInsn(INVOKESTATIC, "GroovyInterface$1", "$createCallSiteArray", "()Lorg/codehaus/groovy/runtime/callsite/CallSiteArray;", false);
            mv.visitVarInsn(ASTORE, 0);
            mv.visitTypeInsn(NEW, "java/lang/ref/SoftReference");
            mv.visitInsn(DUP);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESPECIAL, "java/lang/ref/SoftReference", "<init>", "(Ljava/lang/Object;)V", false);
            mv.visitFieldInsn(PUTSTATIC, "GroovyInterface$1", "$callSiteArray", "Ljava/lang/ref/SoftReference;");
            mv.visitLabel(l1);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(GETFIELD, "org/codehaus/groovy/runtime/callsite/CallSiteArray", "array", "[Lorg/codehaus/groovy/runtime/callsite/CallSite;");
            mv.visitInsn(ARETURN);
            mv.visitMaxs(3, 1);
            mv.visitEnd();
        }
        cw.visitEnd();

        return cw.toByteArray();
    }
}
