import scala.reflect.io.Path
import scala.tools.partest._
import java.io.File

object Test extends StoreReporterDirectTest {
  class V1 {
    def O   = "package b; object O { def o = \"\" }"
    def A   = "package b; class A { class C { O.o } }"
    def pkg = "package object b extends A"
  }
  class V2 extends V1 {
    override def O = "package b; object O { def o = 42 }"
  }

  override def extraSettings = s"-cp ${sys.props("partest.lib")}${File.pathSeparator}$testOutput"

  def show(): Unit = {
    val v1 = new V1
    compiles(v1.O, v1.A, v1.pkg)
    delete(testOutput / "b" / "A.class", testOutput / "b" / "A$C.class")
    val v2 = new V2
    compiles(v2.O, v2.A)
  }

  def compiles(codes: String*) = {
    val global = newCompiler()
    withRun(global)(_ compileSources newSources(codes: _*))
    assert(!global.reporter.hasErrors, storeReporter.infos.mkString("\n"))
  }

  def delete(paths: Path*) = paths.foreach(p => assert(p.delete(), s"$p didn't delete"))
  def code = ""
}
