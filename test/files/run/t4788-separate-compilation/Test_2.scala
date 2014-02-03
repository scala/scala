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
    classString
      .split('\n')
      .filterNot(_.contains("@Lscala/reflect/ScalaSignature"))
      .find(_.contains("@L"))
      .map(_.trim)
  }

  def show {
    // It seems like @java.lang.Deprecated shows up in both the
    // Deprecated attribute and RuntimeVisibleAnnotation attribute,
    // while @scala.deprecated only shows up in the Deprecated attribute.
    // The check file just documents status quo, not sure if Scala
    // should brought in line with Java or not...
    // See the commit message and SI-8883 for more info.
    println(annotationsForClass("DJava"))
    println(annotationsForClass("DScala"))

    println(annotationsForClass("S"))
    println(annotationsForClass("C"))
    println(annotationsForClass("R"))
  }
}
