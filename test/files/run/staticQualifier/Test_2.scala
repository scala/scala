import scala.tools.partest.BytecodeTest
import scala.tools.testkit.ASMConverters._
import scala.tools.asm.Opcodes._

object Test extends BytecodeTest {
  val f2 = 42

  def getT: Test.type = {
    println("hai")
    Test
  }

  def show(): Unit = {
    println(A.foo(Test.f2))
    println(A.foo(getT.f2))

    val ins = instructionsFromMethod(getMethod(loadClassNode("Test$"), "show"))
    val gs = ins.count {
      case Field(GETSTATIC, "Test$", "f2", _) => true
      case _ => false
    }
    assert(gs == 2)
  }
}
