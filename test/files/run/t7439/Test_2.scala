import scala.tools.partest._
import java.io.File

object Test extends StoreReporterDirectTest {
  def code = ???

  def compileCode(code: String) = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(code)
  }

  def C = """
    class C {
      new B_1
    }
  """

  def show(): Unit = {
    //compileCode(C)
    assert(filteredInfos.isEmpty, filteredInfos)

    // blow away the entire package
    val a1Class = new File(testOutput.path, "A_1.class")
    assert(a1Class.exists)
    assert(a1Class.delete())
    // testIdent normalizes to separate names using '/' regardless of platform, drops all but last two parts
    println(s"Recompiling after deleting ${a1Class.testIdent}")

    // bad symbolic reference error expected (but no stack trace!)
    compileCode(C)
    println(storeReporter.infos.mkString("\n")) // Included a NullPointerException before.
  }
}
