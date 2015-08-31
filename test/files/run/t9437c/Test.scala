import java.io.{File, FileOutputStream}

import scala.tools.nsc.settings.ScalaVersion
import scala.tools.partest._
import scala.tools.asm
import asm.{AnnotationVisitor, ClassWriter, FieldVisitor, Handle, MethodVisitor, Opcodes}
import Opcodes._

// This test ensures that we can read JDK 8 (classfile format 52) files with
// parameter names. To do that it first uses ASM to generate a class containing
// these additional attributes. Then it runs a normal compile on Scala source
// that uses the class with named arguments.
// Any failure will be dumped to std out.
object Test extends DirectTest {
  override def extraSettings: String = "-usejavacp -d " + testOutput.path + " -cp " + testOutput.path

  def generateCode(): Unit = {
    val className =  "Foo"

    val cw = new ClassWriter(0)
    cw.visit(52, ACC_PUBLIC + ACC_SUPER, className, null, "java/lang/Object", null);

    val mvC = cw.visitMethod(ACC_PUBLIC, "<init>", "(ILjava/lang/String;JFD)V", null, null);
    mvC.visitParameter("a", ACC_FINAL);
    mvC.visitParameter("_", ACC_FINAL);
    mvC.visitParameter("***", ACC_FINAL);
    mvC.visitParameter("unary_!", ACC_FINAL);
    mvC.visitParameter("ABC", ACC_FINAL);
    mvC.visitCode();
    mvC.visitVarInsn(ALOAD, 0);
    mvC.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
    mvC.visitInsn(RETURN);
    mvC.visitMaxs(1, 8);
    mvC.visitEnd();

    val mvM = cw.visitMethod(ACC_PUBLIC, "bar", "(ILjava/lang/String;JFD)Lscala/runtime/Null$;", null, null);
    mvM.visitParameter("a", ACC_FINAL);
    mvM.visitParameter("_", ACC_FINAL);
    mvM.visitParameter("***", ACC_FINAL);
    mvM.visitParameter("unary_!", ACC_FINAL);
    mvM.visitParameter("ABC", ACC_FINAL);
    mvM.visitCode();
    mvM.visitInsn(ACONST_NULL);
    mvM.visitInsn(ARETURN);
    mvM.visitMaxs(1, 8);
    mvM.visitEnd();

    cw.visitEnd();

    val bytes = cw.toByteArray()

    val fos = new FileOutputStream(new File(s"${testOutput.path}/$className.class"))
    try
      fos write bytes
    finally
      fos.close()

  }

  def code =
"""
class Driver {
  val constrParams = classOf[Foo].getConstructors.head.getParameters
  val methodParams = classOf[Foo].getDeclaredMethods.head.getParameters

  def printParams(params: Array[java.lang.reflect.Parameter]) = {
    params.foreach { param =>
      println(s"name: ${param.getName}; isNamePresent: ${param.isNamePresent}; isSynthetic: ${param.isSynthetic}")
    }
  }

  printParams(constrParams)
  printParams(methodParams)

  val foo = new Foo(a = 1, `_` = "2", *** = 3L, `unary_!` = 4.0f, ABC = 5.0)
  foo.bar(a = 1, `_` = "2", *** = 3L, `unary_!` = 4.0f, ABC = 5.0)
}
"""

  override def show(): Unit = {
    // redirect err to out, for logging
    val prevErr = System.err
    System.setErr(System.out)
    try {
      generateCode()
      compile()
      Class.forName("Driver").newInstance()
    }
    finally
      System.setErr(prevErr)
  }
}
