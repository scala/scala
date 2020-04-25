import scala.tools.partest._
import java.io.File

object Test extends StoreReporterDirectTest {
  def code = """
package sample {

  class A1 {
    def irrelevant: p1.p2.p3.DeleteMe = null
  }
  object A1 {
    class A1_Inner
  }
}

package p1 {
  class LeaveMe
  package p2 {
    package p3 {
      class DeleteMe
    }
  }
}
  """

  override def extraSettings = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    s"-cp $classpath"
  }

  def show(): Unit = {
    compile()
    assert(filteredInfos.isEmpty, filteredInfos)
    deletePackage("p1/p2/p3")
    deletePackage("p1/p2")
    compileString(newCompiler())("""
package sample

class Test {
  final class Inner extends A1.A1_Inner {
    def foo = 42
  }

  def test = new Inner().foo
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
