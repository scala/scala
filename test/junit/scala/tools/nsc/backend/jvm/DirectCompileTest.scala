package scala.tools.nsc.backend.jvm

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Assert._
import CodeGenTools._
import scala.tools.asm.Opcodes._
import scala.tools.partest.ASMConverters._

@RunWith(classOf[JUnit4])
class DirectCompileTest {
  val compiler = newCompiler(extraArgs = "-Ybackend:GenBCode")

  @Test
  def testCompile(): Unit = {
    val List(("C.class", bytes)) = compile(compiler)(
      """
        |class C {
        |  def f = 1
        |}
      """.stripMargin)
    def s(i: Int, n: Int) = (bytes(i) & 0xff) << n
    assertTrue((s(0, 24) | s(1, 16) | s(2, 8) | s(3, 0)) == 0xcafebabe) // mocha java latte machiatto surpreme dark roasted espresso
  }

  @Test
  def testCompileClasses(): Unit = {
    val List(cClass, cModuleClass) = compileClasses(compiler)(
      """
        |class C
        |object C
      """.stripMargin)

    assertTrue(cClass.name == "C")
    assertTrue(cModuleClass.name == "C$")

    val List(dMirror, dModuleClass) = compileClasses(compiler)(
      """
        |object D
      """.stripMargin)

    assertTrue(dMirror.name == "D")
    assertTrue(dModuleClass.name == "D$")
  }

  @Test
  def testCompileMethods(): Unit = {
    val List(f, g) = compileMethods(compiler)(
      """
        |def f = 10
        |def g = f
      """.stripMargin)
    assertTrue(f.name == "f")
    assertTrue(g.name == "g")

    assertTrue(instructionsFromMethod(f).dropNonOp ===
      List(IntOp(BIPUSH, 10), Op(IRETURN)))

    assertTrue(instructionsFromMethod(g).dropNonOp ===
      List(VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C", "f", "()I", false), Op(IRETURN)))
  }

  @Test
  def testDropNonOpAliveLabels(): Unit = {
    val List(f) = compileMethods(compiler)("""def f(x: Int) = if (x == 0) "a" else "b"""")
    assertTrue(instructionsFromMethod(f).dropNonOp === List(
        VarOp(ILOAD, 1),
        Op(ICONST_0),
        Jump(IF_ICMPEQ, Label(6)),
        Jump(GOTO, Label(10)),
        Label(6),
        Ldc(LDC, "a"),
        Jump(GOTO, Label(13)),
        Label(10),
        Ldc(LDC, "b"),
        Label(13),
        Op(ARETURN)
    ))
  }
}
