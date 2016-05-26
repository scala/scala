package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.JavaConverters._
import scala.tools.partest.ASMConverters._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class UnusedLocalVariablesTest extends BytecodeTesting {
  override def compilerArgs = "-opt:unreachable-code"
  import compiler._

  @Test
  def removeUnusedVar(): Unit = {
    val code = """def f(a: Long, b: String, c: Double): Unit = { return; var x = a; var y = x + 10 }"""
    assertLocalVarCount(code, 4) // `this, a, b, c`

    val code2 = """def f(): Unit = { var x = if (true) return else () }"""
    assertLocalVarCount(code2, 1) // x is eliminated, constant folding in scalac removes the if

    val code3 = """def f: Unit = return""" // paramless method
    assertLocalVarCount(code3, 1) // this
  }

  @Test
  def keepUsedVar(): Unit = {
    val code = """def f(a: Long, b: String, c: Double): Unit = { val x = 10 + a; val y = x + 10 }"""
    assertLocalVarCount(code, 6)

    val code2 = """def f(a: Long): Unit = { var x = if (a == 0l) return else () }"""
    assertLocalVarCount(code2, 3) // remains
  }

  @Test
  def constructorLocals(): Unit = {
    val code = """class C {
                 |  def this(a: Int) = {
                 |    this()
                 |    throw new Exception("")
                 |    val y = 0
                 |  }
                 |}
                 |""".stripMargin
    val cls = compileClass(code)
    val m = convertMethod(cls.methods.asScala.toList.find(_.desc == "(I)V").get)
    assertTrue(m.localVars.length == 2) // this, a, but not y


    val code2 =
      """class C {
        |  {
        |    throw new Exception("")
        |    val a = 0
        |  }
        |}
        |
        |object C {
        |  {
        |    throw new Exception("")
        |    val b = 1
        |  }
        |}
      """.stripMargin

    val List(cls2, companion2) = compileClasses(code2)

    assertTrue(getMethod(cls2, "<init>").localVars.length == 1) // this
    assertTrue(getMethod(companion2, "<init>").localVars.length == 1) // this
  }

  def assertLocalVarCount(code: String, numVars: Int): Unit = {
    assertTrue(compileMethod(code).localVars.length == numVars)
  }

}
