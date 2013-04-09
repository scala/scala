import scala.tools.partest.BytecodeTest

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("SameBytecode")
    sameBytecode(getMethod(classNode, "a"), getMethod(classNode, "b"))
  }
}
