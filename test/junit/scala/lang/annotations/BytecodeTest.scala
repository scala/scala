package scala.lang.annotations

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.JavaConverters._
import scala.tools.nsc.backend.jvm.AsmUtils
import scala.tools.partest.ASMConverters._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
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
    // using the output classpath. this shadows the bug SI-8926.

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
      assertTrue(descs.toString, descs exists (_ contains annotName))
    }

    check("A.class", "AnnotA")

    // known issue SI-8928: the visibility of AnnotB should be CLASS, but annotation classes without
    // a @Retention annotation are currently emitted as RUNTIME.
    check("B.class", "AnnotB")
  }
}