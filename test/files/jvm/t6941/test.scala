import scala.tools.partest.{BytecodeTest, ASMConverters}

import scala.tools.nsc.util.JavaClassPath
import java.io.InputStream
import scala.tools.asm
import asm.ClassReader
import asm.tree.{ClassNode, InsnList}
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("SameBytecode")
    similarBytecode(getMethod(classNode, "a"), getMethod(classNode, "b"), ASMConverters.equivalentBytecode(_, _))
  }
}
