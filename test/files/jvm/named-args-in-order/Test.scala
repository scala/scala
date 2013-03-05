import scala.tools.partest.BytecodeTest

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("SameBytecode")
    def sameAsA(meth: String) =
      sameBytecode(getMethod(classNode, "a"), getMethod(classNode, meth))
    Seq("b", "c", "d").foreach(sameAsA)
  }
}
