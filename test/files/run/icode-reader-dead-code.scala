import java.io.{FileOutputStream, FileInputStream}

import scala.tools.asm.{ClassWriter, Opcodes, ClassReader}
import scala.tools.asm.tree.{InsnNode, ClassNode}
import scala.tools.nsc.backend.jvm.AsmUtils
import scala.tools.partest.DirectTest
import scala.collection.JavaConverters._

/**
 * Test that the ICodeReader does not crash if the bytecode of a method has unreachable code.
 */
object Test extends DirectTest {
  def code: String = ???

  def show(): Unit = {
    // The bytecode of f will be modified using ASM by `addDeadCode`
    val aCode =
      """
        |package p
        |class A {
        |  @inline final def f = 1
        |}
      """.stripMargin

    val bCode =
      """
        |package p
        |class B {
        |  def g = (new A()).f
        |}
      """.stripMargin

    compileString(newCompiler("-usejavacp"))(aCode)

    addDeadCode()

    // If inlining fails, the compiler will issue an inliner warning that is not present in the
    // check file
    compileString(newCompiler("-usejavacp", "-opt:l:classpath"))(bCode)
  }

  def readClass(file: String) = {
    val cnode = new ClassNode()
    val is = new FileInputStream(file)
    val reader = new ClassReader(is)
    reader.accept(cnode, 0)
    is.close()
    cnode
  }

  def writeClass(file: String, cnode: ClassNode): Unit = {
    val writer = new ClassWriter(0)
    cnode.accept(writer)

    val os = new FileOutputStream(file)
    os.write(writer.toByteArray)
    os.close()
  }

  def addDeadCode() {
    val file = (testOutput / "p" / "A.class").path
    val cnode = readClass(file)
    val method = cnode.methods.asScala.find(_.name == "f").head

    AsmUtils.traceMethod(method)

    val insns = method.instructions
    val it = insns.iterator()
    while (it.hasNext) {
      val in = it.next()
      if (in.getOpcode == Opcodes.IRETURN) {
        // Insert an ATHROW before the IRETURN. The IRETURN will then be dead code.
        // The ICodeReader should not crash if there's dead code.
        insns.insert(in.getPrevious, new InsnNode(Opcodes.ATHROW))
      }
    }

    AsmUtils.traceMethod(method)

    writeClass(file, cnode)
  }
}
