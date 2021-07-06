import scala.tools.partest._
import java.io.File

object Test extends StoreReporterDirectTest {
  def code = ???

  def compileCode(code: String) = {
    val classpath = pathOf(sys.props("partest.lib"), testOutput.path)
    compileString(newCompiler("-cp", classpath))(code)
  }

  def show(): Unit = {
    compileCode("""
      object U {
        def foo(log: vanishing.Vanishing) = ()
      }

      package vanishing {
        class Vanishing
      }
      """)
    assert(filteredInfos.isEmpty, filteredInfos)
    deletePackage("vanishing")
    compileCode("""
      class Test {
        U
      }
    """)
    assert(storeReporter.infos.isEmpty, storeReporter.infos.mkString("\n")) // Included a MissingRequirementError before.
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
