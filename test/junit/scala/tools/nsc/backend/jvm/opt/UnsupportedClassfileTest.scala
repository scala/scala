package scala.tools.nsc.backend.jvm.opt

import java.nio.file.{Files, Path}

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm.{ClassWriter, Opcodes}
import scala.tools.testkit.BytecodeTesting.newCompiler

@RunWith(classOf[JUnit4])
class UnsupportedClassfileTest {
  // A classfile from a newer JDK than ASM supports, like `java/lang/Object` in scala/bug#13204
  def writeClassWithUnsupportedVersion(dir: Path): Unit = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, "p/J", null, "java/lang/Object", null)
    val mv = cw.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "f", "()I", null, null)
    mv.visitCode()
    mv.visitInsn(Opcodes.ICONST_1)
    mv.visitInsn(Opcodes.IRETURN)
    mv.visitMaxs(0, 0)
    mv.visitEnd()
    cw.visitEnd()
    val bytes = cw.toByteArray
    bytes(6) = 0x7f // major version
    Files.createDirectories(dir.resolve("p"))
    Files.write(dir.resolve("p/J.class"), bytes)
  }

  @Test
  def t13204(): Unit = {
    val dir = Files.createTempDirectory("t13204")
    try {
      writeClassWithUnsupportedVersion(dir)
      val code = "class C { def g = p.J.f() }"
      def warnings(args: String) = {
        var ws = List.empty[String]
        newCompiler(extraArgs = s"-opt:inline:** -cp $dir $args").compileClasses(code, allowMessage = i => { ws ::= i.msg; true })
        ws
      }
      assertEquals(Nil, warnings(""))
      val ws = warnings("-Wopt:no-inline-missing-bytecode")
      assertTrue(ws.toString, ws.exists(_.contains("Error while reading the classfile of p.J for the optimizer")))
    } finally {
      Files.deleteIfExists(dir.resolve("p/J.class"))
      Files.deleteIfExists(dir.resolve("p"))
      Files.deleteIfExists(dir)
    }
  }
}
