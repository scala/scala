import scala.collection.JavaConverters._
import scala.tools.asm
import scala.tools.asm.Opcodes
import scala.tools.partest.BytecodeTest

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("Props")
    val methods = classNode.methods.iterator().asScala.filter( m => m.name == "create")

    for (m <- methods if (m.access & Opcodes.ACC_VARARGS) > 0) {
      println(s"Found vararg overload for method ${m.name}")
    }
  }
}
