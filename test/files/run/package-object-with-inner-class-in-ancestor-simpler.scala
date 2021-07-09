import scala.reflect.io.Path
import scala.tools.partest._
import java.io.File

object Test extends StoreReporterDirectTest {
  def A   = "package b; class A"
  def pkg = "package object b extends A"
  def M   = "package b; class M"

  override def extraSettings = s"-cp ${sys.props("partest.lib")}${File.pathSeparator}$testOutput"

  def show(): Unit = {
    compiles(A, pkg, M)
    delete(testOutput / "b" / "A.class")
    compiles(M, A)
  }

  def compiles(codes: String*) = {
    val global = newCompiler()
    withRun(global)(_ compileSources newSources(codes: _*))
    assert(!global.reporter.hasErrors, storeReporter.infos.mkString("\n"))
  }

  def delete(paths: Path*) = paths.foreach(p => assert(p.delete(), s"$p didn't delete"))
  def code = ""
}
