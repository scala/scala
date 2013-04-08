import scala.tools.partest.BytecodeTest

object Test extends BytecodeTest {
  def show {
    val node1 = loadClassNode("Sub1")
    val node2 = loadClassNode("Sub2")

    sameMethodAndFieldSignatures(node1, node2)
  }
}
