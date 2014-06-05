import java.io.PrintWriter;

import scala.tools.partest.BytecodeTest
import scala.tools.asm.util._
import scala.tools.nsc.util.stringFromWriter

object Test extends BytecodeTest {
  def show {
    val classNode = loadClassNode("Symbols", skipDebugInfo = true)
    val textifier = new Textifier
    classNode.accept(new TraceClassVisitor(null, textifier, null))

    val classString = stringFromWriter(w => textifier.print(w))
    val result =
      classString.split('\n')
        .dropWhile(elem => elem != "public class Symbols {")
        .filterNot(elem => elem.startsWith("  @Lscala/reflect/ScalaSignature") || elem.startsWith("  ATTRIBUTE ScalaSig"))
    result foreach println
  }
}
