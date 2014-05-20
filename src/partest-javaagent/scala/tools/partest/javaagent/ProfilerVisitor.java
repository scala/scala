/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Grzegorz Kossakowski
 */

package scala.tools.partest.javaagent;

import scala.tools.asm.ClassVisitor;
import scala.tools.asm.MethodVisitor;
import scala.tools.asm.Opcodes;

public class ProfilerVisitor extends ClassVisitor implements Opcodes {

  private static String profilerClass = "scala/tools/partest/instrumented/Profiler";

  public ProfilerVisitor(final ClassVisitor cv) {
    super(ASM4, cv);
  }

  private String className = null;

  @Override
  public void visit(int version, int access, String name, String signature, String superName, String[] interfaces) {
    className = name;
    super.visit(version, access, name, signature, superName, interfaces);
  }

  public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
    // delegate the method call to the next
    // chained visitor
    MethodVisitor mv = cv.visitMethod(access, name, desc, signature, exceptions);
    if (!profilerClass.equals(className)) {
      // only instrument non-abstract methods
      if((access & ACC_ABSTRACT) == 0) {
        assert(className != null);
        /* The following instructions do not modify compressed stack frame map so
         * we don't need to worry about recalculating stack frame map. Specifically,
         * let's quote "ASM 4.0, A Java bytecode engineering library" guide (p. 40):
         *
         *   In order to save space, a compiled method does not contain one frame per
         *   instruction: in fact it contains only the frames for the instructions
         *   that correspond to jump targets or exception handlers, or that follow
         *   unconditional jump instructions. Indeed the other frames can be easily
         *   and quickly inferred from these ones.
         *
         * Instructions below are just loading constants and calling a method so according
         * to definition above they do not contribute to compressed stack frame map.
         */
        mv.visitLdcInsn(className);
        mv.visitLdcInsn(name);
        mv.visitLdcInsn(desc);
        mv.visitMethodInsn(INVOKESTATIC, profilerClass, "methodCalled",
            "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V", false);
      }
    }
    return mv;
  }

}
