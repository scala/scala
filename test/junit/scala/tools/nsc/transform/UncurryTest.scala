package scala.tools.nsc
package transform

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.BytecodeTesting

@RunWith(classOf[JUnit4])
class UncurryTest {
  import BytecodeTesting._

  @Test def emptyVarargsDoesNotAllocate(): Unit = {
    val code =
      """
        |def test0 = va0()
        |def test1 = va0(0)
        |def test2 = va1(0)
        |def test3 = va1(0, 0)
        |def va0(elems: Int*): Int = ???
        |def va1(first: Int, rest: Int*): Int = ???
        |
      """.stripMargin
    val compiler = newCompiler()
    val List(test0, test1, test2, test3, va0, va1) = compiler.compileMethods(code)
    assertDoesNotInvoke(test0, "wrapIntArray")
    assertInvoke(test1, "scala/runtime/ScalaRunTime$", "wrapIntArray")
    assertDoesNotInvoke(test2, "wrapIntArray")
    assertInvoke(test3, "scala/runtime/ScalaRunTime$", "wrapIntArray")
  }

}
