import scala.tools.partest.BytecodeTest
import scala.tools.asm.util._
import scala.tools.nsc.util.stringFromWriter

object Test extends BytecodeTest {
  def show {
    val classNode = loadClassNode("Lean")
    val meth = getMethod(classNode, "foo")
    println(meth.getClass)
    val textifier = new Textifier()
    meth.accept(new TraceMethodVisitor(textifier))
    println(stringFromWriter(w => textifier.print(w)))
  }
}

class Lean {
  def foo {
    "" == toString
  }
}
