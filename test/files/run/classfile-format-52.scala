import java.io.{File, FileOutputStream}

import scala.tools.partest.DirectTest
import scala.tools.partest.nest.StreamCapture
import scala.tools.asm
import asm.{ClassWriter, Opcodes}
import Opcodes._

// This test ensures that we can read JDK 8 (classfile format 52) files, including those
// with default methods. To do that it first uses ASM to generate an interface called
// HasDefaultMethod. Then it runs a normal compile on Scala source that extends that
// interface. Any failure will be dumped to std out.
//
// By its nature the test can only work on JDK 8+ because under JDK 7- the
// interface won't verify.
object Test extends DirectTest {
  override def extraSettings: String = s"-opt:inline:** -usejavacp -cp ${testOutput.path}"

  def generateInterface(): Unit = {
    val interfaceName =  "HasDefaultMethod"
    val methodType = "()Ljava/lang/String;"

    val cw = new ClassWriter(0)
    cw.visit(52, ACC_PUBLIC+ACC_ABSTRACT+ACC_INTERFACE, interfaceName, null, "java/lang/Object", null)

    def createMethod(flags:Int, name: String): Unit = {
      val method = cw.visitMethod(flags, name, methodType, null, null)
      method.visitCode()
      method.visitLdcInsn(s"hello from $name")
      method.visitInsn(ARETURN)
      method.visitMaxs(1, 1)
      method.visitEnd()
    }

    createMethod(ACC_PUBLIC, "publicMethod")
    createMethod(ACC_PUBLIC+ACC_STATIC, "staticMethod")
    createMethod(ACC_PRIVATE, "privateMethod")

    cw.visitEnd()
    val bytes = cw.toByteArray()

    val fos = new FileOutputStream(new File(s"${testOutput.path}/$interfaceName.class"))
    try
      fos write bytes
    finally
      fos.close()

  }

  def code =
"""
class Driver extends HasDefaultMethod {
  println(publicMethod())
  println(HasDefaultMethod.staticMethod())
}
"""

  override def show(): Unit = StreamCapture.redirErr {
    generateInterface()
    compile()
    Class.forName("Driver").getDeclaredConstructor().newInstance()
    ()
  }
}
