//> using options -opt:inline:** -Yopt-inline-heuristics:everything

import scala.tools.asm.Opcodes
import scala.tools.partest.BytecodeTest
import scala.tools.testkit.ASMConverters._

// `getstatic p/PublicSub.sf` in p.C names the public class p.PublicSub, so it can be inlined
// into Test, although the field is declared in the package-private class p.Base.
object Test extends BytecodeTest {
  def field = p.C.field()

  def show(): Unit = {
    assert(field == 1)
    val insns = convertMethod(getMethod(loadClassNode("Test$"), "field")).instructions
    assert(insns.contains(Field(Opcodes.GETSTATIC, "p/PublicSub", "sf", "I")), insns)
  }
}
