import scala.tools.partest._
import java.io.File

object Test extends StoreReporterDirectTest {
  def code = ???

  def compileCode(code: String) = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path, "-Ydebug"))(code)
  }

  def code1 = """
    package pack1
    trait T
    object `package` extends T
  """

  def show(): Unit = {
    compileCode(code1)
    val pack1 = new File(testOutput.path, "pack1")
    val tClass = new File(pack1, "T.class")
    assert(tClass.exists)
    assert(tClass.delete())

    // allowed to compile, no direct reference to `T`
    compileCode(code1)
    assert(filteredInfos.isEmpty, filteredInfos)
  }
}
