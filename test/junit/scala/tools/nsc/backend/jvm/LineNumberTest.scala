package scala.tools.nsc
package backend.jvm

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class LineNumberTest extends BytecodeTesting {

  import compiler._

  @Test
  def lineNumberSynthetics(): Unit = {
    // Checking that minimalRemoveUnreachableCode doesn't eliminate the local variable
    // enties for a, a1
    val code                    =
      """
        |class Test {
        |  def main() {
        |    for {
        |      a <- Seq(1)
        |      a1 = a + 1
        |    } {
        |      def getA = a
        |      def getA1 = a1
        |      println("")
        |    }
        |  }
        |}
        |""".stripMargin
    val cls = compileClass(code)
    val m = getMethod(cls, "$anonfun$main$2")
    assertEquals(m.localVars.toString, m.localVars.map(_.name).sorted, List("a", "a1", "x$1"))
  }
}
