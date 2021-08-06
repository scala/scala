package scala.tools.nsc
package transform

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

import scala.tools.testkit.ASMConverters.LineNumber
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._

class MixinTest extends BytecodeTesting {
  import compiler._

  @Test
  def outerAccessorPosition(): Unit = {
    val code =
      """                        // 1
        |class a {               // 2
        |  trait inner {         // 3
        |    def aa = a.this     // 4
        |  }                     // 5
        |}                       // 6
        |class b extends a {     // 7
        |  class z extends inner // 8
        |}                       // 9
        |""".stripMargin

    val List(_, _, _, bz) = compileClasses(code)
    assertEquals("b$z", bz.name)
    val method = getMethod(bz, "a$inner$$$outer")
    val lineNumbers = method.instructions.collect {
      case LineNumber(l, _) => l
    }
    assertEquals(List(8), lineNumbers) // this used to be "line 3".
  }
}
