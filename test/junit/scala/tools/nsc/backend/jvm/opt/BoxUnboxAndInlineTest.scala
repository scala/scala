package scala.tools.nsc.backend.jvm.opt

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm.Opcodes
import scala.tools.asm.Opcodes._
import scala.tools.asm.util.CheckMethodAdapter
import scala.tools.nsc.backend.jvm.MethodNode1
import scala.tools.partest.ASMConverters._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

/**
  * Tests for boxing/unboxing optimizations.
  */
@RunWith(classOf[JUnit4])
class BoxUnboxAndInlineTest extends BytecodeTesting {
  override def compilerArgs = "-opt:l:inline -opt-inline-from:**/*"
  import compiler._

  // Was crashing in 2.13.x once https://github.com/scala/scala/pull/9433 was merged in.
  // Failure was: scala.tools.asm.tree.analysis.AnalyzerException: Error at instruction 16: Cannot pop operand off an empty stack.
  // Discussion: https://github.com/scala/scala/pull/9495#issuecomment-779600132
  @Test
  def unboxAsmCrash(): Unit = {
    val code =
      """package p1; class C {
        |def f(b: java.lang.Byte) = {
        |  var box = 0
        |
        |  @inline def writeBox: Unit = {
        |    box = 1
        |  }
        |
        |  writeBox
        |}
        |}""".stripMargin
    val c = compileClass(code)
    assertSameSummary(getMethod(c, "f"), List(RETURN))

  }

  // This bytecode pattern results from `unboxAsmCrash` in 2.13.x and exposes a bug in
  // https://github.com/scala/scala/pull/9392, which landed originally in 2.12.x.
  //
  // The bytecode shape after the inliner differs in 2.12.x to masks the bug, probably due to
  // https://github.com/scala/scala/pull/7133, which is 2.13.x only.
  //
  // This test constructs the problematic bytecode shape directly. Before the patch, it
  // has an extra POP instruction which would be reported as invalid bytecode by CheckMethodAdapter.
  // "Error at instruction 5: Cannot pop operand off an empty stack. m()V"
  //
  @Test
  def unboxAsmCrashDirect(): Unit = {
    val code: List[Instruction] = List(
      Label(1),
      Op(ACONST_NULL),
      Invoke(INVOKESTATIC, "scala/runtime/ObjectRef", "create", "(Ljava/lang/Object;)Lscala/runtime/ObjectRef;", false),
      VarOp(ASTORE, 1),
      Op(ACONST_NULL),
      VarOp(ALOAD, 1),
      Op(POP),
      VarOp(ASTORE, 2),
      VarOp(ALOAD, 1),
      VarOp(ALOAD, 2),
      Field(PUTFIELD, "scala/runtime/ObjectRef", "elem", "Ljava/lang/Object;"),
      Op(ACONST_NULL),
      VarOp(ASTORE, 2),
      Op(RETURN)
    )
    val method = genMethod(localVars = List(
      LocalVariable("this", "Lcom/foo/Bar;", None, Label(1), Label(1), 1),
      LocalVariable("x", "Lscala/runtime/ObjectRef;", None, Label(1), Label(1), 1),
      LocalVariable("y", "Lscala/runtime/ObjectRef;", None, Label(1), Label(1), 1),
      // introduced by the box/unbox transform, we create the slot ahead of time. CheckMethodAdapter
      // relies on it to verify the bytecode.
      LocalVariable("z", "Lscala/runtime/ObjectRef;", None, Label(1), Label(1), 1)
    ))(code: _*)

    val r = new compiler.global.Run()
    compiler.global.enteringPhase(r.jvmPhase) {
      compiler.global.genBCode.postProcessor.initialize()
      val changed = compiler.global.genBCode.postProcessor.localOpt.boxUnbox.boxUnboxElimination(method, "p1.Owner")
      assert(changed)
      method.visitMaxs(2, 4)
      val labelInsnIndices = new java.util.HashMap[scala.tools.asm.Label, java.lang.Integer]()
      method.instructions.resetLabels()

      val checker = new CheckMethodAdapter(Opcodes.V1_8, "m", "()V", new MethodNode1(), labelInsnIndices)
      method.accept(checker)

      assertSameCode(convertMethod(method), List(
        Op(ACONST_NULL),
        VarOp(ASTORE, 3),
        Op(ACONST_NULL),
        VarOp(ASTORE, 2),
        VarOp(ALOAD, 2),
        VarOp(ASTORE, 3),
        Op(ACONST_NULL),
        VarOp(ASTORE, 2),
        Op(RETURN)))
    }
  }
}
