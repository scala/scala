///> using dep org.scala-lang:scala-compiler:2.13.7

import scala.tools.nsc.{Global, Settings}
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.io.VirtualDirectory

object Test {
  val virtualDirectory = new VirtualDirectory("(memory)", None)
  val settings = new Settings()
  settings.usejavacp.value = true
  settings.outputDirs.setSingleOutput(virtualDirectory)
  //settings.processArgumentString("-Vdebug-type-error -deprecation -Vsymbols -Xdev -Vprint:_ -Vdebug -Vlog:_")
  val global = new Global(settings)

  def main(args: Array[String]): Unit = {
    val codeWithNoError =
      """
      class Hello1 {
        def hello(): Unit = {
          println("Hi there!")
        }
      }
    """

    val codeWithBenignError =
      """
      class Hello2 {
        def hello(): Unit = {
          printlnx("Hi there!")
        }
      }
    """

    val codeWithDeadlyError =
      """
      class Hello3 {
        def pic = 20
        def makePic(s: Int = {
            pic
        }
      }
    """

    val code2WithNoError =
      """
      class Hello4 {
        def hello(): Unit = {
          val x = Seq(1, 2, 3)
          println(x)
        }
      }
    """

    println("Doing first compilation")
    compileCode(codeWithDeadlyError) // early error broke package object loading
    compileCode(codeWithNoError)
    compileCode(codeWithBenignError)
    compileCode(codeWithDeadlyError)

    println("\n\nDoing last compilation")
    compileCode(code2WithNoError)
  }

  def compileCode(code: String): Unit = {
    val run = new global.Run
    val sourceFile = new BatchSourceFile("scripteditor", code)
    global.reporter.reset()
    run.compileSources(List(sourceFile))
    if (global.reporter.hasErrors) {
      println("Compilation failed!")
    } else {
      println("Compilation successful!")
    }
  }
}
