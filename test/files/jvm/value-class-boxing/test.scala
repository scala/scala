import scala.tools.partest.BytecodeTest

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("SameBytecode")
    List("a2", "a3", "a4") foreach { m =>
      print(m + " and a1: ")
      sameBytecode(getMethod(classNode, "a1"), getMethod(classNode, m))
    }
    List("b2", "b3", "b4", "b5") foreach { m =>
      print(m + " and b1: ")
      sameBytecode(getMethod(classNode, "b1"), getMethod(classNode, m))
    }
  }
}
