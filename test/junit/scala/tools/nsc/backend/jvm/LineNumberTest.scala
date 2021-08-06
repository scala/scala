package scala.tools.nsc
package backend.jvm

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._

class LineNumberTest extends BytecodeTesting {

  import compiler._

  @Test
  def lineNumberSynthetics(): Unit = {
    // Checking that minimalRemoveUnreachableCode doesn't eliminate the local variable
    // enties for a, a1
    val code                    =
      """
        |class Test {
        |  def main(): Unit = {
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
    assertEquals(m.localVars.map(_.name).sorted, List("a", "a1", "x$1"), m.localVars.toString)
  }
}
