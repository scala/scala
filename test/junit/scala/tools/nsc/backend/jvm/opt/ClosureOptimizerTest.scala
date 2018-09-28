package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm.Opcodes._
import scala.tools.partest.ASMConverters._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class ClosureOptimizerTest extends BytecodeTesting {
  override def compilerArgs = "-opt:l:inline -opt-inline-from:** -opt-warnings:_"
  import compiler._

  @Test
  def nothingTypedClosureBody(): Unit = {
    val code =
      """abstract class C {
        |  def isEmpty: Boolean
        |  @inline final def getOrElse[T >: C](f: => T) = if (isEmpty) f else this
        |  def t = getOrElse(throw new Error(""))
        |}
      """.stripMargin

    val c = compileClass(code)
    assertSameSummary(getMethod(c, "t"), List(ALOAD, "isEmpty", IFEQ /*12*/, NEW, DUP, LDC, "<init>", ATHROW, -1 /*12*/, ALOAD, ARETURN))
  }

  @Test
  def nullTypedClosureBody(): Unit = {
    val code =
      """abstract class C {
        |  def isEmpty: Boolean
        |  @inline final def getOrElse[T >: C](f: => T) = if (isEmpty) f else this
        |  def t = getOrElse(null)
        |}
      """.stripMargin

    val c = compileClass(code)
    assertSameSummary(getMethod(c, "t"), List(ALOAD, "isEmpty", IFEQ /*9*/, ACONST_NULL, GOTO /*12*/, -1 /*9*/, ALOAD, -1 /*12*/, CHECKCAST, ARETURN))
  }

  @Test
  def makeLMFCastExplicit(): Unit = {
    val code =
      """class C {
        |  def t(l: List[String]) = {
        |    val fun: String => String = s => s
        |    fun(l.head)
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertSameCode(getMethod(c, "t"),
      List(VarOp(ALOAD, 1), Invoke(INVOKEVIRTUAL, "scala/collection/immutable/List", "head", "()Ljava/lang/Object;", false),
        TypeOp(CHECKCAST, "java/lang/String"), Op(ARETURN)))
  }

  @Test
  def closureOptWithUnreachableCode(): Unit = {
    // this example used to crash the ProdCons analysis in the closure optimizer - ProdCons
    // expects no unreachable code.
    val code =
      """class C {
        |  @inline final def m = throw new Error("")
        |  def t = {
        |    val f = (x: Int) => x + 1
        |    m
        |    f(10) // unreachable after inlining m
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertSameSummary(getMethod(c, "t"), List(NEW, DUP, LDC, "<init>", ATHROW))
  }
}
