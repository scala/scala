package scala.tools.nsc.backend.jvm

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.JavaConverters._
import scala.tools.testing.BytecodeTesting

@RunWith(classOf[JUnit4])
class InnerClassAttributeTest extends BytecodeTesting {
  import compiler._

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
    // No InnerClass entry for A$B due to scala/bug#10180
    assert(c.innerClasses.asScala.isEmpty)
  }
}
