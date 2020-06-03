import scala.tools.partest._
import java.io.File

object Test extends StoreReporterDirectTest {
  def code = """
package sample

class Test {
  final class Inner extends A.A_Inner {
    def foo = 42
  }

  def test = new Inner().foo
}
  """

  override def extraSettings = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    s"-cp $classpath"
  }

  def show(): Unit = {
    deletePackage("p1/p2/p3")
    deletePackage("p1/p2")
    compile()
    assert(storeReporter.infos.isEmpty, storeReporter.infos.mkString("\n"))
  }

  def deletePackage(name: String): Unit = {
    val directory = new File(testOutput.path, name)
    for (f <- directory.listFiles()) {
      assert(f.getName.endsWith(".class"))
      assert(f.delete())
    }
    assert(directory.listFiles().isEmpty)
    assert(directory.delete())
  }
}
