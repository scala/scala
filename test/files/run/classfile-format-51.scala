import java.io.{File, FileOutputStream}

import scala.tools.nsc.settings.ScalaVersion
import scala.tools.partest._
import scala.tools.asm
import asm.{AnnotationVisitor, ClassWriter, FieldVisitor, Handle, MethodVisitor, Opcodes}
import Opcodes._

// This test ensures that we can read JDK 7 (classfile format 51) files, including those
// with invokeDynamic instructions and associated constant pool entries
// to do that it first uses ASM to generate a class called DynamicInvoker. Then
// it runs a normal compile on the source in the 'code' field that refers to
// DynamicInvoker. Any failure will be dumped to std out.
//
// By its nature the test can only work on JDK 7+ because under JDK 6 some of the
// classes referred to by DynamicInvoker won't be available and DynamicInvoker won't
// verify. So the test includes a version check that short-circuits the whole test
// on JDK 6
object Test extends DirectTest {
  override def extraSettings: String = "-optimise -usejavacp -d " + testOutput.path + " -cp " + testOutput.path

  def generateClass() {
    val invokerClassName =  "DynamicInvoker"
    val bootstrapMethodName = "bootstrap"
    val bootStrapMethodType = "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"
    val targetMethodName = "target"
    val targetMethodType = "()Ljava/lang/String;"

    val cw = new ClassWriter(0)
    cw.visit(V1_7, ACC_PUBLIC + ACC_SUPER, invokerClassName, null, "java/lang/Object", null)

    val constructor = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
    constructor.visitCode()
    constructor.visitVarInsn(ALOAD, 0)
    constructor.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    constructor.visitInsn(RETURN)
    constructor.visitMaxs(1, 1)
    constructor.visitEnd()

    val target = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, targetMethodName, targetMethodType, null, null)
    target.visitCode()
    target.visitLdcInsn("hello")
    target.visitInsn(ARETURN)
    target.visitMaxs(1, 1)
    target.visitEnd()

    val bootstrap = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, bootstrapMethodName, bootStrapMethodType, null, null)
    bootstrap.visitCode()
//  val lookup = MethodHandles.lookup();
    bootstrap.visitMethodInsn(INVOKESTATIC, "java/lang/invoke/MethodHandles", "lookup", "()Ljava/lang/invoke/MethodHandles$Lookup;", false)
    bootstrap.visitVarInsn(ASTORE, 3) // lookup

//  val clazz = lookup.lookupClass();
    bootstrap.visitVarInsn(ALOAD, 3) // lookup
    bootstrap.visitMethodInsn(INVOKEVIRTUAL, "java/lang/invoke/MethodHandles$Lookup", "lookupClass", "()Ljava/lang/Class;", false)
    bootstrap.visitVarInsn(ASTORE, 4) // clazz

// val methodType = MethodType.fromMethodDescriptorString("()Ljava/lang/String, clazz.getClassLoader()")
    bootstrap.visitLdcInsn("()Ljava/lang/String;")
    bootstrap.visitVarInsn(ALOAD, 4) // CLAZZ
    bootstrap.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Class", "getClassLoader", "()Ljava/lang/ClassLoader;", false)
    bootstrap.visitMethodInsn(INVOKESTATIC, "java/lang/invoke/MethodType", "fromMethodDescriptorString", "(Ljava/lang/String;Ljava/lang/ClassLoader;)Ljava/lang/invoke/MethodType;", false)
    bootstrap.visitVarInsn(ASTORE, 5) // methodType

//  val methodHandle = lookup.findStatic(thisClass, "target", methodType)
    bootstrap.visitVarInsn(ALOAD, 3) // lookup
    bootstrap.visitVarInsn(ALOAD, 4) // clazz
    bootstrap.visitLdcInsn("target")
    bootstrap.visitVarInsn(ALOAD, 5) // methodType
    bootstrap.visitMethodInsn(INVOKEVIRTUAL, "java/lang/invoke/MethodHandles$Lookup", "findStatic", "(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/MethodHandle;", false)
    bootstrap.visitVarInsn(ASTORE, 6) // methodHandle

//  new ConstantCallSite(methodHandle)
    bootstrap.visitTypeInsn(NEW, "java/lang/invoke/ConstantCallSite")
    bootstrap.visitInsn(DUP)
    bootstrap.visitVarInsn(ALOAD, 6) // methodHandle
    bootstrap.visitMethodInsn(INVOKESPECIAL, "java/lang/invoke/ConstantCallSite", "<init>", "(Ljava/lang/invoke/MethodHandle;)V", false)
    bootstrap.visitInsn(ARETURN)
    bootstrap.visitMaxs(4,7)
    bootstrap.visitEnd()

    val test = cw.visitMethod(ACC_PUBLIC + ACC_FINAL, "test", s"()Ljava/lang/String;", null, null)
    test.visitCode()
    val bootstrapHandle = new Handle(H_INVOKESTATIC, invokerClassName, bootstrapMethodName, bootStrapMethodType)
    test.visitInvokeDynamicInsn("invoke", targetMethodType, bootstrapHandle)
    test.visitInsn(ARETURN)
    test.visitMaxs(1, 1)
    test.visitEnd()

    cw.visitEnd()
    val bytes = cw.toByteArray()

    val fos = new FileOutputStream(new File(s"${testOutput.path}/$invokerClassName.class"))
    try
      fos write bytes
    finally
      fos.close()

  }

  def code =
"""
object Driver {
  val invoker = new DynamicInvoker()
  println(invoker.test())
}
"""

  override def show(): Unit = {
    // redirect err to out, for logging
    val prevErr = System.err
    System.setErr(System.out)
    try {
      // this test is only valid under JDK 1.7+
      testUnderJavaAtLeast("1.7") {
        generateClass()
        compile()
        ()
      } otherwise {
        ()
      }
    }
    finally
      System.setErr(prevErr)
  }
}
