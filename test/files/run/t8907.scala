import scala.tools.partest._
import java.io.File

object Test extends StoreReporterDirectTest {
  def code = ???

  def compileCode(code: String) = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(code)
  }

  def show(): Unit = {
    compileCode("""
      class C { class Inner }

      class D {
        object O {
          def foo(c: C)(i: c.Inner): c.Inner = ???
        }
      }
      """)
    assert(filteredInfos.isEmpty, filteredInfos)
    deleteClass("C")
    compileCode("""
      class E {
        def foo = {
          (null: D).toString
        }
      }
    """)
    assert(storeReporter.infos.isEmpty, storeReporter.infos.mkString("\n")) // Included a MissingRequirementError before.
  }

  def deleteClass(name: String) {
    val classFile = new File(testOutput.path, name + ".class")
    assert(classFile.exists)
    assert(classFile.delete())  
  }
}
