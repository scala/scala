import scala.tools.partest._
import java.io.File

// used to crash with an assertion failure in flatten because the type symbol created for the missing
// package was a ClassSymbol, not a PackageClassSymbol
//   - isFlattenablePrefix(vanishingPackage) was true (wrongly)
//   - therefore flatten tried to flatten the class defined in the package, but the class is
//     top-level, vanishingClass.enclosingTopLevelClass is NoSymbol
object Test extends StoreReporterDirectTest {
  def code = ???

  def compileCode(code: String) = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(code)
  }

  def show(): Unit = {
    compileCode("""
      class Outer {
        class Nested extends vanishing.Vanishing
      }

      package vanishing {
        class Vanishing
      }
      """)
    assert(filteredInfos.isEmpty, filteredInfos)
    deletePackage("vanishing")
    compileCode("""
      class Test {
        def f(o: Outer): Outer = o
      }
    """)
    assert(storeReporter.infos.isEmpty, storeReporter.infos.mkString("\n")) // Included a MissingRequirementError before.
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
