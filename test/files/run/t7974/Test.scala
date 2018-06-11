import java.io.PrintWriter

import scala.tools.partest.BytecodeTest
import scala.tools.nsc.backend.jvm.AsmUtils
import org.objectweb.asm.util._
import scala.tools.nsc.util.stringFromWriter
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("Symbols", skipDebugInfo = true)
    classNode.methods.asScala.foreach(m => println(AsmUtils.textify(m)))
  }
}
