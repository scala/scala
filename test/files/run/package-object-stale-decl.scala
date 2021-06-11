import scala.reflect.io.Path
import scala.tools.partest._
import java.io.File

object Test extends StoreReporterDirectTest {
  class V1 {
    def pkg = "package object b extends B"
    def B   = "package b; class B { def stale = 42 }"
    def A   = "package b; class A { stale }"
  }
  class V2 extends V1 {
    override def B   = "package b; class B { }"
  }

  override def extraSettings = s"-cp ${sys.props("partest.lib")}${File.pathSeparator}$testOutput"

  def show(): Unit = {
    val v1 = new V1
    val v2 = new V2
    compiles(v1.A, v1.B, v1.pkg)()
    delete(testOutput / "b" / "A.class")
    compiles(v2.B, v2.A)(Some("not found: value stale"))
  }

  def compiles(codes: String*)(expectedError: Option[String] = None) = {
    val global = newCompiler()
    withRun(global)(_ compileSources newSources(codes: _*))
    val reporterOutput = storeReporter.infos.map(x => x.pos.showError(x.msg)).mkString("\n")
    expectedError match {
      case None =>
        assert(!global.reporter.hasErrors, reporterOutput)
      case Some(text) =>
        assert(global.reporter.hasErrors, "expected compile failure, got success")
        assert(reporterOutput.contains(text), reporterOutput)
    }
  }

  def delete(paths: Path*) = paths.foreach(p => assert(p.delete(), s"$p didn't delete"))
  def code = ""
}
