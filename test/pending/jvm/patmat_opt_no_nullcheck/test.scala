/*
 * filter: inliner warning; re-run with
 */
import scala.tools.partest.{ BytecodeTest, ASMConverters }

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("SameBytecode")
    // ASM and GenBCode assign variable slots slightly differently
    val instrsA = ASMConverters.instructionsFromMethod(getMethod(classNode, "a"))
    val instrsB = ASMConverters.instructionsFromMethod(getMethod(classNode, "b"))
    assert(ASMConverters.equivalentBytecode(instrsA, instrsB), diffInstructions(instrsA, instrsB)) // doesn't work
  }
}
