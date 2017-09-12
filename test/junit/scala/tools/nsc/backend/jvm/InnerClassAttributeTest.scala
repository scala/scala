package scala.tools.nsc.backend.jvm

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.JavaConverters._
import scala.tools.asm.Opcodes._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class InnerClassAttributeTest extends BytecodeTesting {
  import compiler._

  val optCompiler = cached("optCompiler", () => newCompiler(extraArgs = "-opt:l:inline -opt-inline-from:**"))

  @Test
  def javaInnerClassInGenericSignatureOnly(): Unit = {
    val jCode =
      """public class A {
        |  public static class B { }
        |}
      """.stripMargin
    val code =
      """class C {
        |  def foo: Option[A.B] = ???
        |}
      """.stripMargin
    val c = compileClass(code, javaCode = List((jCode, "A.java")))
    assertEquals(c.innerClasses.asScala.toList.map(_.name), List("A$B"))
  }

  @Test
  def t10180(): Unit = {
    val code =
      """class Base[T]
        |class C { class D }
        |abstract class E { def foo: Option[C#D] }
        |class F { private[this] val foo: Option[C#D] = null }
        |abstract class G extends Base[C#D]
        |abstract class H[T <: Base[C#D]]
        |abstract class I { def foo[T <: Base[C#D]] = 42 }
        |abstract class J { def foo[T <: Base[Array[C#D]]] = 42 }
      """.stripMargin
    val List(_, _, _, e, f, g, h, i, j) = compileClasses(code)
    for (k <- List(e, f, g, h, i, j))
      assertEquals(k.innerClasses.asScala.toList.map(_.name), List("C$D"))
  }

  @Test
  def methodHandlesLookupInDeserializeLambda(): Unit = {
    // After inlining the closure, the only remaining reference in the classfile to `MethodHandles$Lookup`
    // is in the `$deserializeLambda$` method. In 2.12.3, this leads to a missing InnerClass entry.
    // The `$deserializeLambda$` is redundant and could be removed (scala-dev#62).
    val code =
      """class C {
        |  @inline final def h(f: Int => Int) = f(1)
        |  def f = h(x => x)
        |}
      """.stripMargin
    val c = optCompiler.compileClass(code)
    // closure is inlined
    assertSameSummary(getMethod(c, "f"), List(ICONST_1, "$anonfun$f$1", IRETURN))
    assertEquals(c.innerClasses.asScala.toList.map(_.name), List("java/lang/invoke/MethodHandles$Lookup"))
  }
}
