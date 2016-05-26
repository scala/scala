package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class InlinerSeparateCompilationTest {
  val args = "-opt:l:classpath"

  @Test
  def inlnieMixedinMember(): Unit = {
    val codeA =
      """trait T {
        |  @inline def f = 0
        |}
        |object O extends T {
        |  @inline def g = 1
        |}
      """.stripMargin

    val codeB =
      """class C {
        |  def t1(t: T) = t.f
        |  def t2 = O.f
        |  def t3 = O.g
        |}
      """.stripMargin

    val warn = "T::f()I is annotated @inline but cannot be inlined: the method is not final and may be overridden"
    val List(c, o, oMod, t) = compileClassesSeparately(List(codeA, codeB), args + " -opt-warnings", _.msg contains warn)
    assertInvoke(getMethod(c, "t1"), "T", "f")
    assertNoInvoke(getMethod(c, "t2"))
    assertNoInvoke(getMethod(c, "t3"))
  }

  @Test
  def inlineSealedMember(): Unit = {
    val codeA =
      """sealed trait T {
        |  @inline def f = 1
        |}
      """.stripMargin

    val codeB =
      """class C {
        |  def t1(t: T) = t.f
        |}
      """.stripMargin

    val List(c, t) = compileClassesSeparately(List(codeA, codeB), args)
    assertNoInvoke(getMethod(c, "t1"))
  }

  @Test
  def inlineInheritedMember(): Unit = {
    val codeA =
      """trait T {
        |  @inline final def f = 1
        |}
        |trait U extends T {
        |  @inline final def g = f
        |}
      """.stripMargin

    val codeB =
      """class C extends U {
        |  def t1 = this.f
        |  def t2 = this.g
        |  def t3(t: T) = t.f
        |}
      """.stripMargin

    val List(c, t, u) = compileClassesSeparately(List(codeA, codeB), args)
    for (m <- List("t1", "t2", "t3")) assertNoInvoke(getMethod(c, m))
  }

  @Test
  def inlineWithSelfType(): Unit = {
    val assembly =
      """trait Assembly extends T {
        |  @inline final def g = 1
        |  @inline final def n = m
        |}
      """.stripMargin

    val codeA =
      s"""trait T { self: Assembly =>
         |  @inline final def f = g
         |  @inline final def m = 1
         |}
         |$assembly
      """.stripMargin

    val List(a, t) = compileClassesSeparately(List(codeA, assembly), args)
    assertNoInvoke(getMethod(t, "f"))
    assertNoInvoke(getMethod(a, "n"))
  }
}
