import scala.tools.partest._
import java.io.File

object Test extends StoreReporterDirectTest {
  def code = ???

  def compileCode(code: String) = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(code)
  }

  def show(): Unit = {
    deletePackage("p1/p2/p3")
    deletePackage("p1/p2")

    compileCode("""
package sample

class Test {
  final class Inner extends A.A_Inner {
    def foo = 42
  }

  def test = new Inner().foo
}
    """)
    assert(storeReporter.infos.isEmpty, storeReporter.infos.mkString("\n"))
  }

  def deletePackage(name: String) {
    val directory = new File(testOutput.path, name)
    for (f <- directory.listFiles()) {
      assert(f.getName.endsWith(".class"))
      assert(f.delete())
    }
    assert(directory.listFiles().isEmpty)
    assert(directory.delete())
  }
}
