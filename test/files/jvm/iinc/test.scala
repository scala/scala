import scala.tools.partest.BytecodeTest

import scala.tools.asm.tree.IincInsnNode

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("Increment")
    for (name <- List("increment", "wideIncrement", "tooBigForIinc")) {
      println(s"def $name")
      getMethod(classNode, name).instructions.toArray().collect {
        case insn: IincInsnNode => println(s"  iinc ${insn.incr}")
      }
      println(s"end $name")
    }
  }
}

