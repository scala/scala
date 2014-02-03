import java.io.PrintWriter;

import scala.tools.partest.BytecodeTest
import scala.tools.asm.util._
import scala.tools.nsc.util.stringFromWriter

object Test extends BytecodeTest {
  def annotationsForClass(className: String): Option[String] = {
    val classNode = loadClassNode(className, skipDebugInfo = false)
    val textifier = new Textifier
    classNode.accept(new TraceClassVisitor(null, textifier, null))

    val classString = stringFromWriter(w => textifier.print(w))
    classString.split('\n').find(_.contains("@Ljava")).map(_.trim)
  }

  def show {
    println(annotationsForClass("S"))
    println(annotationsForClass("C"))
    println(annotationsForClass("R"))
  }
}
