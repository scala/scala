package scala.issues

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.tools.asm.Opcodes
import scala.tools.nsc.backend.jvm.AsmUtils
import scala.tools.nsc.backend.jvm.CodeGenTools._
import org.junit.Assert._
import scala.collection.JavaConverters._
import scala.tools.partest.ASMConverters._
import scala.tools.testing.ClearAfterClass

object BytecodeTest extends ClearAfterClass.Clearable {
  var compiler = newCompiler()
  def clear(): Unit = { compiler = null }
}

@RunWith(classOf[JUnit4])
class BytecodeTest extends ClearAfterClass {
  ClearAfterClass.stateToClear = BytecodeTest
  val compiler = BytecodeTest.compiler

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

    val List(c) = compileClasses(compiler)(code)

    assertTrue(getSingleMethod(c, "f").instructions.count(_.isInstanceOf[TableSwitch]) == 1)
    assertTrue(getSingleMethod(c, "g").instructions.count(_.isInstanceOf[LookupSwitch]) == 1)
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

    val run = new compiler.Run()
    run.compileSources(List(new BatchSourceFile("AnnotA.java", annotA), new BatchSourceFile("AnnotB.java", annotB), new BatchSourceFile("Test.scala", scalaSrc)))
    val outDir = compiler.settings.outputDirs.getSingleOutput.get
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

  @Test
  def t6288bJumpPosition(): Unit = {
    val code =
      """object Case3 {                                 // 01
        | def unapply(z: Any): Option[Int] = Some(-1)   // 02
        | def main(args: Array[String]) {               // 03
        |    ("": Any) match {                          // 04
        |      case x : String =>                       // 05
        |        println("case 0")                      // 06 println and jump at 6
        |      case _ =>                                // 07
        |        println("default")                     // 08 println and jump at 8
        |    }                                          // 09
        |    println("done")                            // 10
        |  }
        |}
      """.stripMargin
    val List(mirror, module) = compileClasses(compiler)(code)

    val unapplyLineNumbers = getSingleMethod(module, "unapply").instructions.filter(_.isInstanceOf[LineNumber])
    assert(unapplyLineNumbers == List(LineNumber(2, Label(0))), unapplyLineNumbers)

    import Opcodes._
    val expected = List(
      LineNumber(4, Label(0)),
      LineNumber(5, Label(5)),
      Jump(IFNE, Label(11)),
      Jump(GOTO, Label(20)),

      LineNumber(6, Label(11)),
      Invoke(INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false),
      Jump(GOTO, Label(33)),

      LineNumber(5, Label(20)),
      Jump(GOTO, Label(24)),

      LineNumber(8, Label(24)),
      Invoke(INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false),
      Jump(GOTO, Label(33)),

      LineNumber(10, Label(33)),
      Invoke(INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false)
    )

    val mainIns = getSingleMethod(module, "main").instructions filter {
      case _: LineNumber | _: Invoke | _: Jump => true
      case _ => false
    }
    assertSameCode(mainIns, expected)
  }
}
