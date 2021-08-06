package scala.lang.annotations

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

import scala.jdk.CollectionConverters._
import scala.tools.nsc.backend.jvm.AsmUtils
import scala.tools.testkit.ASMConverters._
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._

class BytecodeTest extends BytecodeTesting {
  import compiler._

  @Test
  def t8731(): Unit = {
    val code =
      """class C {
        |  def f(x: Int) = (x: @annotation.switch) match {
        |    case 1 => 0
        |    case 2 => 1
        |    case 3 => 2
        |  }
        |  final val K = 10
        |  def g(x: Int) = (x: @annotation.switch) match {
        |    case K => 0
        |    case 1 => 10
        |    case 2 => 20
        |  }
        |}
      """.stripMargin

    val c = compileClass(code)

    assertTrue(getInstructions(c, "f").count(_.isInstanceOf[TableSwitch]) == 1)
    assertTrue(getInstructions(c, "g").count(_.isInstanceOf[LookupSwitch]) == 1)
  }

  @Test
  def t8926(): Unit = {
    import scala.reflect.internal.util.BatchSourceFile

    // this test cannot be implemented using partest because of its mixed-mode compilation strategy:
    // partest first compiles all files with scalac, then the java files, and then again the scala
    // using the output classpath. this shadows the bug scala/bug#8926.

    val annotA =
      """import java.lang.annotation.Retention;
        |import java.lang.annotation.RetentionPolicy;
        |@Retention(RetentionPolicy.RUNTIME)
        |public @interface AnnotA { }
      """.stripMargin
    val annotB = "public @interface AnnotB { }"

    val scalaSrc =
      """@AnnotA class A
        |@AnnotB class B
      """.stripMargin

    val run = new global.Run()
    run.compileSources(List(new BatchSourceFile("AnnotA.java", annotA), new BatchSourceFile("AnnotB.java", annotB), new BatchSourceFile("Test.scala", scalaSrc)))
    val outDir = global.settings.outputDirs.getSingleOutput.get
    val outfiles = (for (f <- outDir.iterator if !f.isDirectory) yield (f.name, f.toByteArray)).toList

    def check(classfile: String, annotName: String) = {
      val f = (outfiles collect { case (`classfile`, bytes) => AsmUtils.readClass(bytes) }).head
      val descs = f.visibleAnnotations.asScala.map(_.desc).toList
      assertTrue(descs exists (_ contains annotName), descs.toString)
    }

    check("A.class", "AnnotA")

    // known issue scala/bug#8928: the visibility of AnnotB should be CLASS, but annotation classes without
    // a @Retention annotation are currently emitted as RUNTIME.
    check("B.class", "AnnotB")
  }
}
