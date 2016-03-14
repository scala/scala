package scala.tools.nsc
package backend.jvm
package opt

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.tools.asm.Opcodes._
import org.junit.Assert._

import CodeGenTools._
import scala.tools.partest.ASMConverters
import ASMConverters._
import AsmUtils._

import scala.collection.convert.decorateAsScala._

object InlinerSeparateCompilationTest {
  val args = "-Yopt:l:classpath"
}

@RunWith(classOf[JUnit4])
class InlinerSeparateCompilationTest {
  import InlinerSeparateCompilationTest._

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

    val warns = Set(
      "T::f()I is annotated @inline but cannot be inlined: the method is not final and may be overridden",
      // TODO SD-85
      """O$::f()I is annotated @inline but could not be inlined:
        |The callee O$::f()I contains the instruction INVOKESPECIAL T.f ()I
        |that would cause an IllegalAccessError when inlined into class C""".stripMargin)
    val List(c, o, oMod, t) = compileClassesSeparately(List(codeA, codeB), args + " -Yopt-warnings", i => warns.exists(i.msg contains _))
    assertInvoke(getSingleMethod(c, "t1"), "T", "f")
//    assertNoInvoke(getSingleMethod(c, "t2")) // SD-85
    assertNoInvoke(getSingleMethod(c, "t3"))
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
    assertNoInvoke(getSingleMethod(c, "t1"))
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
    for (m <- List("t1", "t2", "t3")) assertNoInvoke(getSingleMethod(c, m))
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
    assertNoInvoke(getSingleMethod(t, "f"))
    assertNoInvoke(getSingleMethod(a, "n"))
  }
}
