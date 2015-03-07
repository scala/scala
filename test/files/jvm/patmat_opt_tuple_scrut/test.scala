import scala.tools.partest.BytecodeTest

import scala.tools.nsc.util.JavaClassPath
import java.io.InputStream
import scala.tools.asm
import asm.ClassReader
import asm.tree.{ClassNode, InsnList}
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  def show: Unit = List("Simple", "Constructor", "Referenced", "Sneaky") map (loadClassNode(_)) foreach { classNode =>
    sameBytecode(getMethod(classNode, "a"), getMethod(classNode, "b"))
  }
}
