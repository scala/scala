import scala.tools.partest.BytecodeTest
import scala.tools.asm
import asm.tree.InsnList
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("DEH_1")
    // DEH_1 source code contains an exception-entry that EssentialCleanser.cleanseMethod()
    // removes via scala.tools.asm.optimiz.DanglingExcHandlers
    for (methodNode <- classNode.methods.asScala) {
      if (!methodNode.tryCatchBlocks.isEmpty) println(s"Found at least one exception-entry in ${methodNode.name}")
    }
  }

}
