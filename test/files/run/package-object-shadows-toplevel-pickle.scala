import scala.tools.partest._
import java.io.File

// A top-level definition whose name the package object also defines (or inherits) is shadowed by
// the package object's member, but its classfiles must still carry its pickle: once that member is
// removed, a separate compilation must see the top-level definition.
object Test extends StoreReporterDirectTest {
  class V1 {
    def PT  = "package a; trait PT { object Foo; object Bar; def Baz = 0 }"
    def pkg = "package a; package object b extends a.PT { object Qux }"
    def Foo = "package a.b; object Foo { def x = 1 }"
    def Bar = "package a.b; class Bar; object Bar { def y = 2 }"
    def Baz = "package a.b; object Baz { def z = 3 }"
    def Qux = "package a.b; object Qux { def w = 4 }"
  }
  class V2 extends V1 {
    override def PT  = "package a; trait PT"
    override def pkg = "package a; package object b extends a.PT"
  }
  def client = "package a.b; object Client { Foo.x; new Bar; Bar.y; Baz.z; Qux.w }"

  override def extraSettings = s"-cp ${sys.props("partest.lib")}${File.pathSeparator}$testOutput"

  def show(): Unit = {
    val v1 = new V1
    val v2 = new V2
    compiles(v1.PT, v1.pkg, v1.Foo, v1.Bar, v1.Baz, v1.Qux)
    compiles(v2.PT, v2.pkg)
    compiles(client)
  }

  def compiles(codes: String*) = {
    val global = newCompiler()
    withRun(global)(_ compileSources newSources(codes: _*))
    val reporterOutput = storeReporter.infos.map(x => x.pos.showError(x.msg)).mkString("\n")
    assert(!global.reporter.hasErrors, reporterOutput)
  }

  def code = ""
}
