import scala.tools.partest.{BytecodeTest, ASMConverters}
import ASMConverters._

object Test extends BytecodeTest {
  def show: Unit = {
    val gIns = instructionsFromMethod(getMethod(loadClassNode("B"), "g"))
    val hIns = instructionsFromMethod(getMethod(loadClassNode("C"), "h"))
    // val invocation = Invoke(INVOKESTATIC, A_1, bar, ()I, false)
    for (i <- List(gIns, hIns)) {
      assert(i exists {
        case Invoke(_, _, "bar", "()I", _) => true
        case _ => false
      }, i mkString "\n")
    }
  }
}
