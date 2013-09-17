import scala.tools.partest.BytecodeTest
import scala.tools.asm.util._
import scala.tools.nsc.util.stringFromWriter

object Test extends BytecodeTest {
  def show {
    val classNode = loadClassNode("Lean")
    def showMethod(name: String) {
      val meth = getMethod(classNode, name)
      println(name)
      val textifier = new Textifier()
      meth.accept(new TraceMethodVisitor(textifier))
      println(stringFromWriter(w => textifier.print(w)))
      println()
    }
    showMethod("string")
    showMethod("module")
  }
}

class Lean {
  def string {
    "" == toString
  }

  def module {
    Nil == (toString: Any)
  }
}
