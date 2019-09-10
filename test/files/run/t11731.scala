
import scala.annotation._
import scala.reflect.internal.util.{BatchSourceFile, NoFile, SourceFile}
import scala.reflect.io.{Path, PlainFile, VirtualFile}
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.partest.DirectTest
import scala.util.Properties.isWin

object Test extends DirectTest {

  override def code = "class C { def f = Set(1, 2, 3) toList }"

  override def reporter(settings: Settings) = new StoreReporter(settings)

  private def sourceAt(pathology: String): SourceFile = {
    val path = Path(pathology)
    val file = new PlainFile(path)
    new BatchSourceFile(file, code)
  }
  private def virtually(name: String, pathology: String): SourceFile = {
    val file = new VirtualFile(name, s"$pathology/$name")
    new BatchSourceFile(file, code)
  }
  private def noFile: SourceFile = {
    new BatchSourceFile(NoFile, code)
  }
  // breaks CompilationUnit
  @unused private def broken: SourceFile = {
    new BatchSourceFile(null: PlainFile, code)
  }

  private def evilly(path: String) = path.replace("/", "\\")

  private def fakeSbt = new sbt.FakeSbt

  override def show() = {
    val global = newCompiler("-usejavacp", "-feature")

    def checkMsg(): Unit =
      assert(global.reporter.asInstanceOf[StoreReporter].infos.head.msg.contains("postfix operator"))
    def checkErr(): Unit = {
      assert(global.reporter.hasErrors)
      assert(global.reporter.errorCount == 1)
      assert(!global.reporter.hasWarnings)
      checkMsg()
    }
    def checkWarn(): Unit = {
      assert(!global.reporter.hasErrors)
      assert(global.reporter.hasWarnings)
      assert(global.reporter.warningCount == 1)
      checkMsg()
    }
    fakeSbt.run {
      withRun(global)(_.compileSources(sourceAt("/tmp/xsbt/Compat.scala") :: Nil))
    }
    checkWarn()
    fakeSbt.run {
      withRun(global)(_.compileSources(sourceAt("sbt/Compat.scala") :: Nil))
    }
    checkErr()
    fakeSbt.run {
      withRun(global)(_.compileSources(sourceAt("C.scala") :: Nil))
    }
    checkErr()
    fakeSbt.run {
      withRun(global)(_.compileSources(sourceAt("Compat.scala") :: Nil))
    }
    checkErr()
    fakeSbt.run {
      withRun(global)(_.compileSources(virtually("Compat.scala", "xsbt") :: Nil))
    }
    checkErr()
    fakeSbt.run {
      withRun(global)(_.compileSources(noFile :: Nil))
    }
    checkErr()
    // run last to sanity check reporter state is reset
    fakeSbt.run {
      withRun(global)(_.compileSources(sourceAt("xsbt/Compat.scala") :: Nil))
    }
    checkWarn()

    if (isWin) {
      fakeSbt.run {
        withRun(global)(_.compileSources(sourceAt(evilly("/tmp/xsbt/Compat.scala")) :: Nil))
      }
      checkWarn()
    }
  }
}

package sbt {
  class FakeSbt {
    def run(body: => Unit): Unit = body
  }
}
